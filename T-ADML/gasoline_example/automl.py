import numpy as np
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from econml.sklearn_extensions.model_selection import GridSearchCVList
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.linear_model import LassoCV, LogisticRegressionCV, LinearRegression, Lasso, LogisticRegression
from rfriesz import RFreg, RFrr, poly_feature_fns
from sievereisz import SieveRR, SparseSieveRR, Featurizer
from sklearn.base import clone, BaseEstimator


# Generic machine learning regressor with cross validation
def first_stage_reg(numeric_columns=[], cv=None):
    return GridSearchCVList([
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('lasso', Lasso())]),
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('feat', Featurizer(degree=1, normalize=False)),
                                       ('lasso', Lasso())]),
                             RandomForestRegressor(n_estimators=100, random_state=123),
                             RFreg(reg_feature_fns=poly_feature_fns(10),
                                   min_var_leaf_on_val=True,
                                   min_impurity_decrease=1e-4, max_samples=.85, inference=False, subforest_size=1,
                                   random_state=123)
                            ],
                             param_grid_list=[
                                              {'lasso__alpha': [1e-6, 1e-5, 0.0001, .001, .01, .1, 1, 10]},
                                              {'lasso__alpha': [1e-6, 1e-5, 0.0001, .001, .01, .1, 1, 10]},
                                              {'max_depth': [3, 5],
                                               'min_samples_leaf': [10, 50]},
                                              {'reg_feature_fns': [poly_feature_fns(2)],
                                               'min_samples_leaf': [10],
                                               'min_var_fraction_leaf': [0.01, 0.1],
                                               'l2': [1e-3, 1e-2]}
                             ],
                             cv=cv,
                             scoring='neg_mean_squared_error',
                             n_jobs=-1)

# Returns a function that when called with data (X, y), returns a generator that when called
# returns an un-fitted regression estimator; e.g. the one with the best hyperparameters.
def reg_fn_gen(numeric_columns, cv=None, verbose=0):
    def get_reg_fn(X, y):
        est = first_stage_reg(numeric_columns, cv=cv).fit(X, y)
        if verbose > 0:
            print('Chose: ', est.best_score_, est.best_estimator_)
            print('Scores: ', [(l.best_score_, l.best_estimator_) for l in est._gcv_list])
        return lambda: clone(est.best_estimator_)
    return get_reg_fn


# The negative of the reisz score: L(g) := E[2 * m(X;g) - g(X)^2]
def neg_reisz_score_gen(moment_fn):
    def neg_reisz_score(model, X):
        return np.mean(2 * moment_fn(X, model.predict) - model.predict(X)**2)
    return neg_reisz_score

# A generic machine learning riesz representer estimator with cross-validation
def first_stage_rr(moment_fn, numeric_columns, cv=None):
    return GridSearchCVList([
                            SieveRR(featurizer=Featurizer(degree=1), moment_fn=moment_fn),
                            SieveRR(featurizer=Featurizer(degree=2), moment_fn=moment_fn),
                            SparseSieveRR(featurizer=Featurizer(degree=2), moment_fn=moment_fn, tol=1e-4),
                            SparseSieveRR(featurizer=Featurizer(degree=1), moment_fn=moment_fn, tol=1e-4),
                            RFrr(riesz_feature_fns=poly_feature_fns(10),
                                        moment_fn=moment_fn,
                                        min_var_leaf_on_val=True,
                                        min_impurity_decrease=0.001,
                                        max_samples=.65,
                                        inference=False,
                                        subforest_size=1,
                                        random_state=123)
                            ],
                            param_grid_list=[
                                   {'alpha': np.geomspace(1e-6, 1e3, 20)},
                                   {'alpha': np.geomspace(1e-6, 1e3, 20)},
                                   {'alpha': np.geomspace(1e-6, 1e3, 20),
                                    'l1_ratio': [.5, 1.0]},
                                   {'alpha': np.geomspace(1e-6, 1e3, 20),
                                    'l1_ratio': [.5, 1.0]},
                                   {'riesz_feature_fns': [poly_feature_fns(2), poly_feature_fns(3)],
                                    'min_samples_leaf': [10, 50],
                                    'min_var_fraction_leaf': [0.01, 0.1],
                                    'l2': [1e-5, 1e-3],
                                    'max_depth': [5, None]}
                           ],
                           scoring=neg_reisz_score_gen(moment_fn),
                           cv=cv, verbose=0, n_jobs=-1)


# Returns a function that when called with data X, returns a generator that when called
# returns an un-fitted riesz estimator; e.g. the one with the best hyperparameters.
def rr_fn_gen(moment_fn, numeric_columns, cv=None, verbose=0):
    def get_rr_fn(X):
        est = first_stage_rr(moment_fn, numeric_columns, cv=cv).fit(X)
        if verbose > 0:
            print('Chose: ', est.best_score_, est.best_estimator_)
            print('Scores: ', [(l.best_score_, l.best_estimator_) for l in est._gcv_list])
        return lambda: clone(est.best_estimator_)
    return get_rr_fn


# Generic machine learning regressor with cross validation
def first_stage_model_y(numeric_columns=[], cv=None):
    return GridSearchCVList([
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('lasso', Lasso())]),
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('feat', Featurizer(degree=1, normalize=False)),
                                       ('lasso', Lasso())]),
                             RandomForestRegressor(n_estimators=100, random_state=123)
                            ],
                             param_grid_list=[
                                              {'lasso__alpha': [1e-6, 1e-5, 0.0001, .001, .01, .1, 1, 10]},
                                              {'lasso__alpha': [1e-6, 1e-5, 0.0001, .001, .01, .1, 1, 10]},
                                              {'max_depth': [3, 5],
                                               'min_samples_leaf': [10, 50]},
                             ],
                             cv=cv,
                             scoring='neg_mean_squared_error',
                             n_jobs=-1)

# Returns a function that when called with data (X, y), returns a generator that when called
# returns an un-fitted regression estimator; e.g. the one with the best hyperparameters.
def model_y_fn_gen(numeric_columns, cv=None, verbose=0):
    def get_reg_fn(X, y):
        est = first_stage_model_y(numeric_columns, cv=cv).fit(X, y)
        if verbose > 0:
            print('Chose: ', est.best_score_, est.best_estimator_)
            print('Scores: ', [(l.best_score_, l.best_estimator_) for l in est._gcv_list])
        return lambda: clone(est.best_estimator_)
    return get_reg_fn


class RegRandomForestClassifier(BaseEstimator):
    def __init__(self, *, max_depth=None, min_samples_leaf=1):
        self.max_depth = max_depth
        self.min_samples_leaf = min_samples_leaf
    
    def fit(self, X, y):
        self.model_ = RandomForestClassifier(n_estimators=100, random_state=123,
                                             max_depth=self.max_depth,
                                             min_samples_leaf=self.min_samples_leaf)
        self.model_.fit(X, y)
        return self

    def predict(self, X):
        return self.model_.predict_proba(X)[:, 1]

class RegLogisticRegression(BaseEstimator):
    def __init__(self, *, C=1):
        self.C = C

    def fit(self, X, y):
        self.model_ = LogisticRegression(C=self.C, max_iter=1000, random_state=123)
        self.model_.fit(X, y)
        return self

    def predict(self, X):
        return self.model_.predict_proba(X)[:, 1]

# Generic machine learning classifier with cross validation
def first_stage_model_t(numeric_columns=[], cv=None):
    return GridSearchCVList([
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('lasso', Lasso())]),
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('feat', Featurizer(degree=1, normalize=False)),
                                       ('lasso', Lasso())]),
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('lg', RegLogisticRegression())]),
                             Pipeline([('scale', ColumnTransformer([('num', StandardScaler(), numeric_columns)],
                                                                   remainder='passthrough')),
                                       ('feat', Featurizer(degree=1, normalize=False)),
                                       ('lg', RegLogisticRegression())]),
                             RandomForestRegressor(n_estimators=100, random_state=123),
                             RegRandomForestClassifier(),
                            ],
                             param_grid_list=[
                                              {'lasso__alpha': [1e-6, 1e-5, 0.0001, .001, .01, .1, 1, 10]},
                                              {'lasso__alpha': [1e-6, 1e-5, 0.0001, .001, .01, .1, 1, 10]},
                                              {'lg__C': [1e-3, 1e-2, 1e-1, 1, 10, 100]},
                                              {'lg__C': [1e-3, 1e-2, 1e-1, 1, 10, 100]},
                                              {'max_depth': [3, 5],
                                               'min_samples_leaf': [10, 50]},
                                              {'max_depth': [3, 5],
                                               'min_samples_leaf': [10, 50]},
                             ],
                             cv=cv,
                             scoring='neg_mean_squared_error',
                             n_jobs=-1)

# Returns a function that when called with data (X, y), returns a generator that when called
# returns an un-fitted regression estimator; e.g. the one with the best hyperparameters.
def model_t_fn_gen(numeric_columns, cv=None, verbose=0):
    def get_reg_fn(X, y):
        est = first_stage_model_t(numeric_columns, cv=cv).fit(X, y)
        if verbose > 0:
            print('Chose: ', est.best_score_, est.best_estimator_)
            print('Scores: ', [(l.best_score_, l.best_estimator_) for l in est._gcv_list])
        return lambda: clone(est.best_estimator_)
    return get_reg_fn
