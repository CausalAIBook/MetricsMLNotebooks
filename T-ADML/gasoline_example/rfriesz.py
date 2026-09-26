from econml.grf._base_grf import BaseGRF
from econml.utilities import cross_product
import numpy as np
import scipy.stats
import statsmodels.api as sm

def mean_ci(data, confidence=0.95):
    a = 1.0 * np.array(data)
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * scipy.stats.t.ppf((1 + confidence) / 2., n-1)
    return m, m-h, m+h


def poly_feature_fns(degree):
    def poly(d, sign=1.0):
        return lambda x: sign * x[:, [0]]**d
    return [poly(t) for t in np.arange(0, degree + 1)]


class RFrr(BaseGRF):
    """
    A forest that estimates a riesz representer of a linear functional,
    using the linear moment equation:

    .. code-block::

        E[ phi(T,X) phi(T,X)' alpha(x) - m(Z; phi) | X=x] = 0

    Equivalently it can be viewed as minimizing the loss function:

    .. code-block::

        E[ (phi(T,X)' alpha(x))^2 - 2 * <alpha(x), m(Z; phi)> | X=x]


    Parameters
    ----------
    riesz_feature_fns : list of lambda
        A list of functions that each take as input the concatenation of [T;X] and return
        either a scalar, representating the value of a feature phi(T,X), or a tuple
        of a scalar, where the first entry is the feature and the second the gradient of the
        feature with respect to its first argument. This will be used to approximate
        the riesz representer function via a local sieve.

    moment_fn : lambda x, fn -> float
        A moment function m([T;X], g) that takes as input the concatenation of [T;X] and a
        function and return a linear moment. This is the moment whose average value we want
        to estimate

    l2 : float, optional (default=0.01)
        An l2 penalty added to the covariance matrix J(node)=E[phi(T,X) phi(T,X)' | X in node],
        to be used for calculating the local parameter. Equivalent to using a modified jabocian of:
        J(node) + lambda * I

    n_estimators : int, default=100
        Number of trees

    criterion : {``"mse"``, ``"het"``}, default="mse"
        The function to measure the quality of a split. Supported criteria
        are "mse" for the mean squared error in a linear moment estimation tree and "het" for
        heterogeneity score.

        - The "mse" criterion finds splits that minimize the score:

          .. code-block::

            sum_{child} E[ (phi(T,X)' alpha(x))^2 - 2 * <alpha(x), m(Z; phi)> | X=child] weight(child)
            + sum_{child} E[ (<theta(x), phi(T, X)> - Y)^2 | X=child] weight(child)

          Internally, this criterion is approximated by computationally simpler variants for
          computational purposes. In particular, it is replaced by::

              sum_{child} weight(child) * rho1(child).T @ E[phi(T,X) phi(T,X)' | X in child] @ rho1(child)
              + sum_{child} weight(child) * rho2(child).T @ E[phi(T,X) phi(T,X)' | X in child] @ rho2(child)

          where:

          .. code-block::

            rho1(child) := E[phi(T,X) phi(T,X)' | X in parent]^{-1}
                                    * E[m(Z; phi) - phi(T,X) phi(T,X)'alpha(parent) | X in child]
            rho2(child) := E[phi(T,X) phi(T,X)' | X in parent]^{-1}
                                    * E[(Y - <theta(parent), phi(T,X)) phi(T,X) | X in child]

          This can be thought as a heterogeneity inducing score, but putting more weight on scores
          with a large minimum eigenvalue of the child jacobian ``E[phi(T,X) phi(T,X)' | X in child]``,
          which leads to smaller variance of the estimate and stronger identification of the parameters.

        - The "het" criterion finds splits that maximize the pure parameter heterogeneity score:

          .. code-block::

            sum_{child} weight(child) * rho(child).T @ rho(child)\

          This can be thought as an approximation to the ideal heterogeneity score:

          .. code-block::

              weight(left) * weight(right) || theta(left) - theta(right)||_2^2 / weight(parent)^2

          as outlined in [cf1]_

    max_depth : int, default=None
        The maximum depth of the tree. If None, then nodes are expanded until
        all leaves are pure or until all leaves contain less than
        min_samples_split samples.

    min_samples_split : int or float, default=10
        The minimum number of samples required to split an internal node:

        - If int, then consider `min_samples_split` as the minimum number.
        - If float, then `min_samples_split` is a fraction and
          `ceil(min_samples_split * n_samples)` are the minimum
          number of samples for each split.

    min_samples_leaf : int or float, default=5
        The minimum number of samples required to be at a leaf node.
        A split point at any depth will only be considered if it leaves at
        least ``min_samples_leaf`` training samples in each of the left and
        right branches.  This may have the effect of smoothing the model,
        especially in regression.

        - If int, then consider `min_samples_leaf` as the minimum number.
        - If float, then `min_samples_leaf` is a fraction and
          `ceil(min_samples_leaf * n_samples)` are the minimum
          number of samples for each node.

    min_weight_fraction_leaf : float, default=0.0
        The minimum weighted fraction of the sum total of weights (of all
        the input samples) required to be at a leaf node. Samples have
        equal weight when sample_weight is not provided.

    min_var_fraction_leaf : None or float in (0, 1], default=None
        A constraint on some proxy of the variation of the treatment vector that should be contained within each
        leaf as a percentage of the total variance of the treatment vector on the whole sample. This avoids
        performing splits where either the variance of the treatment is small and hence the local parameter
        is not well identified and has high variance. The proxy of variance is different for different criterion,
        primarily for computational efficiency reasons.

        - If ``criterion='het'``, then this constraint translates to:

          .. code-block::

            for all i in {1, ..., T.shape[1]}:
                E[T[i]^2 | X in leaf] > `min_var_fraction_leaf` * E[T[i]^2]

          When ``T`` is the residual treatment (i.e. centered), this translates to a requirement that

          .. code-block::

            for all i in {1, ..., T.shape[1]}:
                Var(T[i] | X in leaf) > `min_var_fraction_leaf` * Var(T[i])

        - If ``criterion='mse'``, because the criterion stores more information about the leaf for
          every candidate split, then this constraint imposes further constraints on the pairwise correlations
          of different coordinates of each treatment, i.e.:

          .. code-block::

            for all i neq j:
              sqrt(Var(T[i]|X in leaf) * Var(T[j]|X in leaf) * (1 - rho(T[i], T[j]| in leaf)^2))
                  > `min_var_fraction_leaf` sqrt(Var(T[i]) * Var(T[j]) * (1 - rho(T[i], T[j])^2))

          where rho(X, Y) is the Pearson correlation coefficient of two random variables X, Y. Thus this
          constraint also enforces that no two pairs of treatments be very co-linear within a leaf. This
          extra constraint primarily has bite in the case of more than two input treatments and also avoids
          leafs where the parameter estimate has large variance due to local co-linearities of the treatments.

    min_var_leaf_on_val : bool, default=False
        Whether the `min_var_fraction_leaf` constraint should also be enforced to hold on the validation set of the
        honest split too. If `min_var_leaf=None` then this flag does nothing. Setting this to True should
        be done with caution, as this partially violates the honesty structure, since the treatment variable
        of the validation set is used to inform the split structure of the tree. However, this is a benign
        dependence as it only uses local correlation structure of the treatment T to decide whether
        a split is feasible.

    max_features : int, float or {"auto", "sqrt", "log2"}, default=None
        The number of features to consider when looking for the best split:

        - If int, then consider `max_features` features at each split.
        - If float, then `max_features` is a fraction and
          `int(max_features * n_features)` features are considered at each
          split.
        - If "auto", then `max_features=n_features`.
        - If "sqrt", then `max_features=sqrt(n_features)`.
        - If "log2", then `max_features=log2(n_features)`.
        - If None, then `max_features=n_features`.

        Note: the search for a split does not stop until at least one
        valid partition of the node samples is found, even if it requires to
        effectively inspect more than ``max_features`` features.

    min_impurity_decrease : float, default=0.0
        A node will be split if this split induces a decrease of the impurity
        greater than or equal to this value.
        The weighted impurity decrease equation is the following::

            N_t / N * (impurity - N_t_R / N_t * right_impurity
                                - N_t_L / N_t * left_impurity)

        where ``N`` is the total number of samples, ``N_t`` is the number of
        samples at the current node, ``N_t_L`` is the number of samples in the
        left child, and ``N_t_R`` is the number of samples in the right child.
        ``N``, ``N_t``, ``N_t_R`` and ``N_t_L`` all refer to the weighted sum,
        if ``sample_weight`` is passed.

    max_samples : int or float in (0, 1], default=.45,
        The number of samples to use for each subsample that is used to train each tree:

        - If int, then train each tree on `max_samples` samples, sampled without replacement from all the samples
        - If float, then train each tree on ceil(`max_samples` * `n_samples`), sampled without replacement
          from all the samples.

        If ``inference=True``, then `max_samples` must either be an integer smaller than `n_samples//2` or a float
        less than or equal to .5.

    min_balancedness_tol: float in [0, .5], default=.45
        How imbalanced a split we can tolerate. This enforces that each split leaves at least
        (.5 - min_balancedness_tol) fraction of samples on each side of the split; or fraction
        of the total weight of samples, when sample_weight is not None. Default value, ensures
        that at least 5% of the parent node weight falls in each side of the split. Set it to 0.0 for no
        balancedness and to .5 for perfectly balanced splits. For the formal inference theory
        to be valid, this has to be any positive constant bounded away from zero.

    honest : bool, default=True
        Whether each tree should be trained in an honest manner, i.e. the training set is split into two equal
        sized subsets, the train and the val set. All samples in train are used to create the split structure
        and all samples in val are used to calculate the value of each node in the tree.

    inference : bool, default=True
        Whether inference (i.e. confidence interval construction and uncertainty quantification of the estimates)
        should be enabled. If `inference=True`, then the estimator uses a bootstrap-of-little-bags approach
        to calculate the covariance of the parameter vector, with am objective Bayesian debiasing correction
        to ensure that variance quantities are positive.

    subforest_size : int, default=4,
        The number of trees in each sub-forest that is used in the bootstrap-of-little-bags calculation.
        The parameter `n_estimators` must be divisible by `subforest_size`. Should typically be a small constant.

    n_jobs : int or None, default=-1
        The number of parallel jobs to be used for parallelism; follows joblib semantics.
        ``n_jobs=-1`` means all available cpu cores. ``n_jobs=None`` means no parallelism.

    random_state : int, RandomState instance or None, default=None
        Controls the randomness of the estimator. The features are always
        randomly permuted at each split. When ``max_features < n_features``, the algorithm will
        select ``max_features`` at random at each split before finding the best
        split among them. But the best found split may vary across different
        runs, even if ``max_features=n_features``. That is the case, if the
        improvement of the criterion is identical for several splits and one
        split has to be selected at random. To obtain a deterministic behaviour
        during fitting, ``random_state`` has to be fixed to an integer.

    verbose : int, default=0
        Controls the verbosity when fitting and predicting.

    warm_start : bool, default=``False``
        When set to ``True``, reuse the solution of the previous call to fit
        and add more estimators to the ensemble, otherwise, just fit a whole
        new forest. If ``True``, then `oob_predict` method for out-of-bag predictions is not available.

    Attributes
    ----------
    feature_importances_ : ndarray of shape (n_features,)
        The feature importances based on the amount of parameter heterogeneity they create.
        The higher, the more important the feature.
        The importance of a feature is computed as the (normalized) total heterogeneity that the feature
        creates. Each split that the feature was chosen adds::

            parent_weight * (left_weight * right_weight)
                * mean((value_left[k] - value_right[k])**2) / parent_weight**2

        to the importance of the feature. Each such quantity is also weighted by the depth of the split.
        By default splits below ``max_depth=4`` are not used in this calculation and also each split
        at depth `depth`, is re-weighted by ``1 / (1 + `depth`)**2.0``. See the method ``feature_importances``
        for a method that allows one to change these defaults.

    estimators_ : list of objects of type :class:`~econml.grf.GRFTree`
        The fitted trees.
    """

    def __init__(self, *,
                 riesz_feature_fns,
                 moment_fn,
                 l2=0.01,
                 n_estimators=100,
                 criterion="mse",
                 max_depth=None,
                 min_samples_split=10,
                 min_samples_leaf=5,
                 min_weight_fraction_leaf=0.,
                 min_var_fraction_leaf=None,
                 min_var_leaf_on_val=False,
                 max_features="auto",
                 min_impurity_decrease=0.,
                 max_samples=.45,
                 min_balancedness_tol=.45,
                 honest=True,
                 inference=True,
                 fit_intercept=True,
                 subforest_size=4,
                 n_jobs=-1,
                 random_state=None,
                 verbose=0,
                 warm_start=False):
        self.riesz_feature_fns = riesz_feature_fns
        self.moment_fn = moment_fn
        self.l2 = l2
        super().__init__(n_estimators=n_estimators,
                         criterion=criterion,
                         max_depth=max_depth,
                         min_samples_split=min_samples_split,
                         min_samples_leaf=min_samples_leaf,
                         min_weight_fraction_leaf=min_weight_fraction_leaf,
                         min_var_fraction_leaf=min_var_fraction_leaf,
                         min_var_leaf_on_val=min_var_leaf_on_val,
                         max_features=max_features,
                         min_impurity_decrease=min_impurity_decrease,
                         max_samples=max_samples,
                         min_balancedness_tol=min_balancedness_tol,
                         honest=honest,
                         inference=inference,
                         fit_intercept=False,
                         subforest_size=subforest_size,
                         n_jobs=n_jobs,
                         random_state=random_state,
                         verbose=verbose,
                         warm_start=warm_start)

    def _get_alpha_and_pointJ(self, X, T, y):
        n_riesz_feats = len(self.riesz_feature_fns)
        TX = np.hstack([T, X])
        riesz_feats = np.hstack([feat_fn(TX)
                                 for feat_fn in self.riesz_feature_fns])
        mfeats = np.hstack([self.moment_fn(TX, feat_fn)
                            for feat_fn in self.riesz_feature_fns])
        alpha = np.zeros((X.shape[0], n_riesz_feats))
        alpha[:, :n_riesz_feats] = mfeats
        riesz_cov_matrix = cross_product(riesz_feats, riesz_feats).reshape(
            (X.shape[0], n_riesz_feats, n_riesz_feats)) 
        penalty = self.l2 * np.eye(n_riesz_feats)
        penalty[0, 0] = 0
        pointJ = riesz_cov_matrix + penalty
        return alpha, pointJ.reshape((X.shape[0], -1))

    def _get_n_outputs_decomposition(self, X, T, y):
        n_relevant_outputs = len(self.riesz_feature_fns)
        n_outputs = n_relevant_outputs
        return n_outputs, n_relevant_outputs

    def _translate(self, point, TX):
        riesz_feats = np.hstack([feat_fn(TX)
                                 for feat_fn in self.riesz_feature_fns])
        n_riesz_feats = riesz_feats.shape[1]
        riesz = np.sum(point[:, :n_riesz_feats] * riesz_feats, axis=1)
        return riesz

    def fit(self, X):
        return super().fit(X[:, 1:], X[:, [0]], X[:, [0]])

    def predict(self, X_test):
        point = super().predict(X_test[:, 1:])
        return self._translate(point, X_test)



class RFreg(BaseGRF):
    """
    A forest that estimates a regression function with pre-specified features,
    using the linear moment equations:

    .. code-block::

        E[ (<theta(x), phi(T, X)> - Y) phi(T, X) | X=x] = 0

    Equivalently it can be viewed as minimizing jointly the loss functions:

    .. code-block::

        E[ (<theta(x), phi(T, X)> - Y)^2 | X=x]


    Parameters
    ----------
    reg_feature_fns : list of lambda
        A list of functions that each take as input the concatenation of [T;X] and return
        either a scalar, representating the value of a feature phi(T,X), or a tuple
        of a scalar, where the first entry is the feature and the second the gradient of the
        feature with respect to its first argument. This will be used to approximate
        the regression function via a local sieve.

    l2 : float, optional (default=0.01)
        An l2 penalty added to the covariance matrix J(node)=E[phi(T,X) phi(T,X)' | X in node],
        to be used for calculating the local parameter. Equivalent to using a modified jabocian of:
        J(node) + lambda * I

    n_estimators : int, default=100
        Number of trees

    criterion : {``"mse"``, ``"het"``}, default="mse"
        The function to measure the quality of a split. Supported criteria
        are "mse" for the mean squared error in a linear moment estimation tree and "het" for
        heterogeneity score.

        - The "mse" criterion finds splits that minimize the score:

          .. code-block::

            sum_{child} E[ (phi(T,X)' alpha(x))^2 - 2 * <alpha(x), m(Z; phi)> | X=child] weight(child)
            + sum_{child} E[ (<theta(x), phi(T, X)> - Y)^2 | X=child] weight(child)

          Internally, this criterion is approximated by computationally simpler variants for
          computational purposes. In particular, it is replaced by::

              sum_{child} weight(child) * rho1(child).T @ E[phi(T,X) phi(T,X)' | X in child] @ rho1(child)
              + sum_{child} weight(child) * rho2(child).T @ E[phi(T,X) phi(T,X)' | X in child] @ rho2(child)

          where:

          .. code-block::

            rho1(child) := E[phi(T,X) phi(T,X)' | X in parent]^{-1}
                                    * E[m(Z; phi) - phi(T,X) phi(T,X)'alpha(parent) | X in child]
            rho2(child) := E[phi(T,X) phi(T,X)' | X in parent]^{-1}
                                    * E[(Y - <theta(parent), phi(T,X)) phi(T,X) | X in child]

          This can be thought as a heterogeneity inducing score, but putting more weight on scores
          with a large minimum eigenvalue of the child jacobian ``E[phi(T,X) phi(T,X)' | X in child]``,
          which leads to smaller variance of the estimate and stronger identification of the parameters.

        - The "het" criterion finds splits that maximize the pure parameter heterogeneity score:

          .. code-block::

            sum_{child} weight(child) * rho(child).T @ rho(child)\

          This can be thought as an approximation to the ideal heterogeneity score:

          .. code-block::

              weight(left) * weight(right) || theta(left) - theta(right)||_2^2 / weight(parent)^2

          as outlined in [cf1]_

    max_depth : int, default=None
        The maximum depth of the tree. If None, then nodes are expanded until
        all leaves are pure or until all leaves contain less than
        min_samples_split samples.

    min_samples_split : int or float, default=10
        The minimum number of samples required to split an internal node:

        - If int, then consider `min_samples_split` as the minimum number.
        - If float, then `min_samples_split` is a fraction and
          `ceil(min_samples_split * n_samples)` are the minimum
          number of samples for each split.

    min_samples_leaf : int or float, default=5
        The minimum number of samples required to be at a leaf node.
        A split point at any depth will only be considered if it leaves at
        least ``min_samples_leaf`` training samples in each of the left and
        right branches.  This may have the effect of smoothing the model,
        especially in regression.

        - If int, then consider `min_samples_leaf` as the minimum number.
        - If float, then `min_samples_leaf` is a fraction and
          `ceil(min_samples_leaf * n_samples)` are the minimum
          number of samples for each node.

    min_weight_fraction_leaf : float, default=0.0
        The minimum weighted fraction of the sum total of weights (of all
        the input samples) required to be at a leaf node. Samples have
        equal weight when sample_weight is not provided.

    min_var_fraction_leaf : None or float in (0, 1], default=None
        A constraint on some proxy of the variation of the treatment vector that should be contained within each
        leaf as a percentage of the total variance of the treatment vector on the whole sample. This avoids
        performing splits where either the variance of the treatment is small and hence the local parameter
        is not well identified and has high variance. The proxy of variance is different for different criterion,
        primarily for computational efficiency reasons.

        - If ``criterion='het'``, then this constraint translates to:

          .. code-block::

            for all i in {1, ..., T.shape[1]}:
                E[T[i]^2 | X in leaf] > `min_var_fraction_leaf` * E[T[i]^2]

          When ``T`` is the residual treatment (i.e. centered), this translates to a requirement that

          .. code-block::

            for all i in {1, ..., T.shape[1]}:
                Var(T[i] | X in leaf) > `min_var_fraction_leaf` * Var(T[i])

        - If ``criterion='mse'``, because the criterion stores more information about the leaf for
          every candidate split, then this constraint imposes further constraints on the pairwise correlations
          of different coordinates of each treatment, i.e.:

          .. code-block::

            for all i neq j:
              sqrt(Var(T[i]|X in leaf) * Var(T[j]|X in leaf) * (1 - rho(T[i], T[j]| in leaf)^2))
                  > `min_var_fraction_leaf` sqrt(Var(T[i]) * Var(T[j]) * (1 - rho(T[i], T[j])^2))

          where rho(X, Y) is the Pearson correlation coefficient of two random variables X, Y. Thus this
          constraint also enforces that no two pairs of treatments be very co-linear within a leaf. This
          extra constraint primarily has bite in the case of more than two input treatments and also avoids
          leafs where the parameter estimate has large variance due to local co-linearities of the treatments.

    min_var_leaf_on_val : bool, default=False
        Whether the `min_var_fraction_leaf` constraint should also be enforced to hold on the validation set of the
        honest split too. If `min_var_leaf=None` then this flag does nothing. Setting this to True should
        be done with caution, as this partially violates the honesty structure, since the treatment variable
        of the validation set is used to inform the split structure of the tree. However, this is a benign
        dependence as it only uses local correlation structure of the treatment T to decide whether
        a split is feasible.

    max_features : int, float or {"auto", "sqrt", "log2"}, default=None
        The number of features to consider when looking for the best split:

        - If int, then consider `max_features` features at each split.
        - If float, then `max_features` is a fraction and
          `int(max_features * n_features)` features are considered at each
          split.
        - If "auto", then `max_features=n_features`.
        - If "sqrt", then `max_features=sqrt(n_features)`.
        - If "log2", then `max_features=log2(n_features)`.
        - If None, then `max_features=n_features`.

        Note: the search for a split does not stop until at least one
        valid partition of the node samples is found, even if it requires to
        effectively inspect more than ``max_features`` features.

    min_impurity_decrease : float, default=0.0
        A node will be split if this split induces a decrease of the impurity
        greater than or equal to this value.
        The weighted impurity decrease equation is the following::

            N_t / N * (impurity - N_t_R / N_t * right_impurity
                                - N_t_L / N_t * left_impurity)

        where ``N`` is the total number of samples, ``N_t`` is the number of
        samples at the current node, ``N_t_L`` is the number of samples in the
        left child, and ``N_t_R`` is the number of samples in the right child.
        ``N``, ``N_t``, ``N_t_R`` and ``N_t_L`` all refer to the weighted sum,
        if ``sample_weight`` is passed.

    max_samples : int or float in (0, 1], default=.45,
        The number of samples to use for each subsample that is used to train each tree:

        - If int, then train each tree on `max_samples` samples, sampled without replacement from all the samples
        - If float, then train each tree on ceil(`max_samples` * `n_samples`), sampled without replacement
          from all the samples.

        If ``inference=True``, then `max_samples` must either be an integer smaller than `n_samples//2` or a float
        less than or equal to .5.

    min_balancedness_tol: float in [0, .5], default=.45
        How imbalanced a split we can tolerate. This enforces that each split leaves at least
        (.5 - min_balancedness_tol) fraction of samples on each side of the split; or fraction
        of the total weight of samples, when sample_weight is not None. Default value, ensures
        that at least 5% of the parent node weight falls in each side of the split. Set it to 0.0 for no
        balancedness and to .5 for perfectly balanced splits. For the formal inference theory
        to be valid, this has to be any positive constant bounded away from zero.

    honest : bool, default=True
        Whether each tree should be trained in an honest manner, i.e. the training set is split into two equal
        sized subsets, the train and the val set. All samples in train are used to create the split structure
        and all samples in val are used to calculate the value of each node in the tree.

    inference : bool, default=True
        Whether inference (i.e. confidence interval construction and uncertainty quantification of the estimates)
        should be enabled. If `inference=True`, then the estimator uses a bootstrap-of-little-bags approach
        to calculate the covariance of the parameter vector, with am objective Bayesian debiasing correction
        to ensure that variance quantities are positive.

    subforest_size : int, default=4,
        The number of trees in each sub-forest that is used in the bootstrap-of-little-bags calculation.
        The parameter `n_estimators` must be divisible by `subforest_size`. Should typically be a small constant.

    n_jobs : int or None, default=-1
        The number of parallel jobs to be used for parallelism; follows joblib semantics.
        ``n_jobs=-1`` means all available cpu cores. ``n_jobs=None`` means no parallelism.

    random_state : int, RandomState instance or None, default=None
        Controls the randomness of the estimator. The features are always
        randomly permuted at each split. When ``max_features < n_features``, the algorithm will
        select ``max_features`` at random at each split before finding the best
        split among them. But the best found split may vary across different
        runs, even if ``max_features=n_features``. That is the case, if the
        improvement of the criterion is identical for several splits and one
        split has to be selected at random. To obtain a deterministic behaviour
        during fitting, ``random_state`` has to be fixed to an integer.

    verbose : int, default=0
        Controls the verbosity when fitting and predicting.

    warm_start : bool, default=``False``
        When set to ``True``, reuse the solution of the previous call to fit
        and add more estimators to the ensemble, otherwise, just fit a whole
        new forest. If ``True``, then `oob_predict` method for out-of-bag predictions is not available.

    Attributes
    ----------
    feature_importances_ : ndarray of shape (n_features,)
        The feature importances based on the amount of parameter heterogeneity they create.
        The higher, the more important the feature.
        The importance of a feature is computed as the (normalized) total heterogeneity that the feature
        creates. Each split that the feature was chosen adds::

            parent_weight * (left_weight * right_weight)
                * mean((value_left[k] - value_right[k])**2) / parent_weight**2

        to the importance of the feature. Each such quantity is also weighted by the depth of the split.
        By default splits below ``max_depth=4`` are not used in this calculation and also each split
        at depth `depth`, is re-weighted by ``1 / (1 + `depth`)**2.0``. See the method ``feature_importances``
        for a method that allows one to change these defaults.

    estimators_ : list of objects of type :class:`~econml.grf.GRFTree`
        The fitted trees.
    """

    def __init__(self, *,
                 reg_feature_fns,
                 l2=0.01,
                 n_estimators=100,
                 criterion="mse",
                 max_depth=None,
                 min_samples_split=10,
                 min_samples_leaf=5,
                 min_weight_fraction_leaf=0.,
                 min_var_fraction_leaf=None,
                 min_var_leaf_on_val=False,
                 max_features="auto",
                 min_impurity_decrease=0.,
                 max_samples=.45,
                 min_balancedness_tol=.45,
                 honest=True,
                 inference=True,
                 fit_intercept=True,
                 subforest_size=4,
                 n_jobs=-1,
                 random_state=None,
                 verbose=0,
                 warm_start=False):
        self.reg_feature_fns = reg_feature_fns
        self.l2 = l2
        super().__init__(n_estimators=n_estimators,
                         criterion=criterion,
                         max_depth=max_depth,
                         min_samples_split=min_samples_split,
                         min_samples_leaf=min_samples_leaf,
                         min_weight_fraction_leaf=min_weight_fraction_leaf,
                         min_var_fraction_leaf=min_var_fraction_leaf,
                         min_var_leaf_on_val=min_var_leaf_on_val,
                         max_features=max_features,
                         min_impurity_decrease=min_impurity_decrease,
                         max_samples=max_samples,
                         min_balancedness_tol=min_balancedness_tol,
                         honest=honest,
                         inference=inference,
                         fit_intercept=False,
                         subforest_size=subforest_size,
                         n_jobs=n_jobs,
                         random_state=random_state,
                         verbose=verbose,
                         warm_start=warm_start)

    def fit(self, X, y):
        return super().fit(X[:, 1:], X[:, [0]], y)

    def _get_alpha_and_pointJ(self, X, T, y):
        n_reg_feats = len(self.reg_feature_fns)
        n_feats = n_reg_feats
        TX = np.hstack([T, X])
        reg_feats = np.hstack([feat_fn(TX)
                               for feat_fn in self.reg_feature_fns])
        alpha = y.reshape(-1, 1) * reg_feats
        reg_cov_matrix = cross_product(reg_feats, reg_feats).reshape(
            (X.shape[0], n_reg_feats, n_reg_feats))
        penalty = self.l2 * np.eye(n_reg_feats)
        penalty[0, 0] = 0
        pointJ = reg_cov_matrix + penalty
        return alpha, pointJ.reshape((X.shape[0], -1))

    def _get_n_outputs_decomposition(self, X, T, y):
        n_relevant_outputs = len(self.reg_feature_fns)
        n_outputs = n_relevant_outputs
        return n_outputs, n_relevant_outputs

    def _translate(self, point, TX):
        reg_feats = np.hstack([feat_fn(TX)
                               for feat_fn in self.reg_feature_fns])
        reg = np.sum(point * reg_feats, axis=1)
        return reg

    def predict(self, X_test):
        point = super().predict(X_test[:, 1:])
        return self._translate(point, X_test)
