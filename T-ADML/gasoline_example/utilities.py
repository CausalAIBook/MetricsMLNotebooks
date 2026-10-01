import numpy as np
from sklearn.base import BaseEstimator, clone

import warnings
from functools import partial

import numpy as np
import scipy
import scipy.sparse as sp
from scipy.linalg import svd
try:
    from scipy.fft import fft, ifft
except ImportError:   # scipy < 1.4
    from scipy.fftpack import fft, ifft

from sklearn.base import BaseEstimator
from sklearn.base import TransformerMixin
from sklearn.utils import check_random_state, as_float_array
from sklearn.utils.extmath import safe_sparse_dot
from sklearn.utils.validation import check_is_fitted
from sklearn.metrics.pairwise import pairwise_kernels, KERNEL_PARAMS, PAIRWISE_KERNEL_FUNCTIONS, _parallel_pairwise, _pairwise_callable
from sklearn.metrics.pairwise import rbf_kernel, cosine_similarity, pairwise_kernels


def rmse(x, y):
    return np.sqrt(np.mean((x - y)**2))

def mean_ci(data, confidence=0.95):
    a = 1.0 * np.array(data).flatten()
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * scipy.stats.t.ppf((1 + confidence) / 2., n-1)
    return m, se, m-h, m+h


def binary_kernel(X, Y=None):
    return 1.0 * (X[:, [0]] == X[:, [0]].T) if Y is None else 1.0 * (X[:, [0]] == Y[:, [0]].T)


def binary_or_rbf_kernel(X, Y=None, *, gamma):
    return binary_kernel(X, Y=Y) if len(np.unique(X)) == 2 else rbf_kernel(X, Y=Y, gamma=gamma)


def prod_kernel(X, Y=None, *, gamma):

    if hasattr(gamma, '__len__'):
        X = X.copy()
        X[:, 1:] = X[:, 1:] * np.sqrt(gamma).reshape(1, -1)
        if Y is not None:
            Y = Y.copy()
            Y[:, 1:] = Y[:, 1:] * np.sqrt(gamma).reshape(1, -1)
        gamma = 1

    if Y is None:
        # return np.product([binary_or_rbf_kernel(X[:, [t]], gamma=gamma) for t in np.arange(1, X.shape[1])], axis=0)  * binary_kernel(X, Y)
        return rbf_kernel(X[:, 1:], gamma=gamma)  * binary_kernel(X, Y)
    else:
        # return np.product([binary_or_rbf_kernel(X[:, [t]], Y=Y[:, [t]], gamma=gamma) for t in np.arange(1, X.shape[1])], axis=0)  * binary_kernel(X, Y)
        return rbf_kernel(X[:, 1:], Y=Y[:, 1:], gamma=gamma) * binary_kernel(X, Y)


class AutoKernel:

    def __init__(self, *, type='var'):
        self.type = type
    
    def fit(self, X):
        if self.type == 'var':
            self.gamma_ = 1/ ((X.shape[1] - 1) * np.var(X[:, 1:], axis=0))
        if self.type == 'median':
            self.gamma_ = np.array([1 / ((X.shape[1] - 1) * np.median(np.abs(X[:, [i]] - X[:, [i]].T))**2)
                                    for i in np.arange(1, X.shape[1])])
        self.kernel_ = lambda X, Y=None: prod_kernel(X, Y=Y, gamma=self.gamma_)
        return self

class FitParamsWrapper:

    def __init__(self, model, **fit_params):
        self.model = model
        self.fit_params = fit_params
    
    def fit(self, X):
        return self.model.fit(X, **self.fit_params)
    
    def predict(self, X):
        return self.model.predict(X)

class PluginRR(BaseEstimator):

    def __init__(self, *, model_t, min_propensity=0):
        self.model_t = clone(model_t, safe=False)
        self.min_propensity = min_propensity
    
    def fit(self, X):
        self.model_t_ = clone(self.model_t, safe=False).fit(X[:, 1:], X[:, 0])
        return self
    
    def predict(self, X):
        propensity = np.clip(self.model_t_.predict_proba(X[:, 1:]), self.min_propensity, 1 - self.min_propensity)
        return X[:, 0] / propensity[:, 1] - (1 - X[:, 0]) / propensity[:, 0]



def pairwise_kernels(X, Y=None, metric="linear", *, filter_params=False,
                     n_jobs=None, **kwds):
    if metric in PAIRWISE_KERNEL_FUNCTIONS:
        if filter_params:
            kwds = {k: kwds[k] for k in kwds
                    if k in KERNEL_PARAMS[metric]}
        func = PAIRWISE_KERNEL_FUNCTIONS[metric]
    elif callable(metric):
        func = metric
    else:
        raise ValueError("Unknown kernel %r" % metric)

    return _parallel_pairwise(X, Y, func, n_jobs, **kwds)

class Nystroem(TransformerMixin, BaseEstimator):

    def __init__(self, kernel="rbf", *, gamma=None, coef0=None, degree=None,
                 kernel_params=None, n_components=100, random_state=None,
                 n_jobs=None):

        self.kernel = kernel
        self.gamma = gamma
        self.coef0 = coef0
        self.degree = degree
        self.kernel_params = kernel_params
        self.n_components = n_components
        self.random_state = random_state
        self.n_jobs = n_jobs

    def fit(self, X, y=None):
        """Fit estimator to data.
        Samples a subset of training points, computes kernel
        on these and computes normalization matrix.
        Parameters
        ----------
        X : array-like of shape (n_samples, n_features)
            Training data.
        """
        X = self._validate_data(X, accept_sparse='csr')
        rnd = check_random_state(self.random_state)
        n_samples = X.shape[0]

        # get basis vectors
        if self.n_components > n_samples:
            # XXX should we just bail?
            n_components = n_samples
            warnings.warn("n_components > n_samples. This is not possible.\n"
                          "n_components was set to n_samples, which results"
                          " in inefficient evaluation of the full kernel.")

        else:
            n_components = self.n_components
        n_components = min(n_samples, n_components)
        inds = rnd.permutation(n_samples)
        basis_inds = inds[:n_components]
        basis = X[basis_inds]

        basis_kernel = pairwise_kernels(basis, metric=self.kernel,
                                        filter_params=True,
                                        n_jobs=self.n_jobs,
                                        **self._get_kernel_params())
        # sqrt of kernel matrix on basis vectors
        U, S, V = svd(basis_kernel)
        S = np.maximum(S, 1e-12)
        self.normalization_ = np.dot(U / np.sqrt(S), V)
        self.components_ = basis
        self.component_indices_ = inds
        return self

    def transform(self, X):
        """Apply feature map to X.
        Computes an approximate feature map using the kernel
        between some training points and X.
        Parameters
        ----------
        X : array-like of shape (n_samples, n_features)
            Data to transform.
        Returns
        -------
        X_transformed : ndarray of shape (n_samples, n_components)
            Transformed data.
        """
        check_is_fitted(self)
        X = self._validate_data(X, accept_sparse='csr', reset=False)

        kernel_params = self._get_kernel_params()
        embedded = pairwise_kernels(X, self.components_,
                                    metric=self.kernel,
                                    filter_params=True,
                                    n_jobs=self.n_jobs,
                                    **kernel_params)
        return np.dot(embedded, self.normalization_.T)

    def _get_kernel_params(self):
        params = self.kernel_params
        if params is None:
            params = {}
        if not callable(self.kernel) and self.kernel != 'precomputed':
            for param in (KERNEL_PARAMS[self.kernel]):
                if getattr(self, param) is not None:
                    params[param] = getattr(self, param)
        else:
            if (self.gamma is not None or
                    self.coef0 is not None or
                    self.degree is not None):
                raise ValueError("Don't pass gamma, coef0 or degree to "
                                 "Nystroem if using a callable "
                                 "or precomputed kernel")

        return params

    def _more_tags(self):
        return {
            '_xfail_checks': {
                'check_transformer_preserve_dtypes':
                'dtypes are preserved but not at a close enough precision',
            },
            'preserves_dtype': [np.float64, np.float32]
        }