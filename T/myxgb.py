from xgboost import XGBRegressor, XGBClassifier
from sklearn.model_selection import train_test_split
import xgboost as xgb
import numpy as np
import xgboost as xgb
from typing import Tuple
from sklearn.base import BaseEstimator, clone
from econml.sklearn_extensions.model_selection import WeightedKFold
from sklearn.utils import check_X_y

def gradient(predt: np.ndarray, dtrain: xgb.DMatrix) -> np.ndarray:
    '''Compute the gradient squared log error.'''
    y = dtrain.get_label().flatten()
    w = dtrain.get_weight().flatten()
    return w * (predt.flatten() - y)

def hessian(predt: np.ndarray, dtrain: xgb.DMatrix) -> np.ndarray:
    '''Compute the hessian for squared log error.'''
    w = dtrain.get_weight().flatten()
    return w * np.ones(predt.shape[0])

def weighted_squared(predt: np.ndarray,
                dtrain: xgb.DMatrix) -> Tuple[np.ndarray, np.ndarray]:
    '''Weighted Squared Error objective.
    '''
    grad = gradient(predt, dtrain)
    hess = hessian(predt, dtrain)
    return grad, hess

def weighted_metric(predt: np.ndarray, dtrain: xgb.DMatrix) -> Tuple[str, float]:
    ''' Root mean squared log error metric.'''
    y = dtrain.get_label()
    w = dtrain.get_weight()
    elements = w.flatten() * np.power((predt.flatten() - y.flatten()), 2)
    return 'WeightedRMSE', float(np.sqrt(np.mean(elements)))

class RegWrapper(BaseEstimator):
    def __init__(self, model):
        self.model = model
    def fit(self, X, y):
        self.model_ = clone(self.model)
        self.model_.fit(X, y)
        return self
    def predict(self, X):
        return self.model_.predict_proba(X)[:, 1]

class MyXGBRegressor(XGBRegressor):
    
    def fit(self, X, y):
        X, Xval, y, yval = train_test_split(X, y, shuffle=True, test_size=.2, random_state=self.random_state)
        super().fit(X, y, eval_set=[(Xval, yval)], verbose=False)
        return self

class MyXGBClassifier(XGBClassifier):
    
    def fit(self, X, y):
        X, Xval, y, yval = train_test_split(X, y, shuffle=True, test_size=.2, stratify=y, random_state=self.random_state)
        super().fit(X, y, eval_set=[(Xval, yval)], verbose=False)
        return self

    def predict(self, X):
        return self.predict_proba(X)[:, 1]

class MyWeightedΧGBRegressor(BaseEstimator):
    
    def __init__(self, *, max_depth=2, learning_rate=.05, n_estimators=500,
                early_stopping_rounds=5, min_child_weight=20,
                verbosity=0, random_state=123):
        self.max_depth = max_depth
        self.learning_rate = learning_rate
        self.n_estimators = n_estimators
        self.early_stopping_rounds = early_stopping_rounds
        self.min_child_weight = min_child_weight
        self.verbosity = verbosity
        self.random_state = random_state
        return

    def fit(self, X, y, *, sample_weight):
        # X, Xval, y, yval, sample_weight, sample_weight_val = train_test_split(X, y, sample_weight,
        #                                                                       shuffle=True, test_size=.2,
        #                                                                       random_state=self.random_state)
        X, y = check_X_y(X, y)
        kf = WeightedKFold(n_splits=5, shuffle=True, random_state=self.random_state)
        train, test = kf.split(X, y, sample_weight=sample_weight)[0]
        X, Xval, y, yval = X[train], X[test], y[train], y[test]
        sample_weight, sample_weight_val = sample_weight[train], sample_weight[test]
        dtrain = xgb.DMatrix(X, y, weight=sample_weight)
        dval = xgb.DMatrix(Xval, yval, weight=sample_weight_val)
        self.model = xgb.train({'max_depth': self.max_depth,
                                'learning_rate': self.learning_rate, 
                                'min_child_weight': self.min_child_weight,
                                'disable_default_eval_metric': 1,
                                'seed': self.random_state},
                              dtrain, num_boost_round=self.n_estimators,
                              evals=[(dval, 'val')],
                              obj=weighted_squared,
                              custom_metric=weighted_metric,
                              early_stopping_rounds=self.early_stopping_rounds,
                              verbose_eval=(self.verbosity > 0))
        return self

    def predict(self, X):
        return self.model.predict(xgb.DMatrix(X))

def xgb_reg(random_state=123):
    return MyXGBRegressor(max_depth=2, learning_rate=.05, n_estimators=500,
                          early_stopping_rounds=5, min_child_weight=20,
                          verbosity=0, random_state=random_state)

def xgb_clf(random_state=123):
    return MyXGBClassifier(max_depth=2, learning_rate=.05, n_estimators=500,
                           early_stopping_rounds=5, min_child_weight=20, verbosity=0,
                           random_state=random_state)

def xgb_wreg(random_state=123):
    return MyWeightedΧGBRegressor(max_depth=2, learning_rate=.05, n_estimators=500,
                                  early_stopping_rounds=5, min_child_weight=20, verbosity=0,
                                  random_state=random_state)
