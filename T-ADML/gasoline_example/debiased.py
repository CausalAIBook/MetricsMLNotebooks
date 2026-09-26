import numpy as np
from sklearn.base import clone
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import KFold
from utilities import mean_ci
import scipy

class DebiasedMoment:

    def __init__(self, *, moment_fn, get_reisz_fn, get_reg_fn, n_splits=5):
        self.moment_fn = moment_fn
        self.get_reisz_fn = get_reisz_fn
        self.get_reg_fn = get_reg_fn
        self.n_splits = n_splits
    
    def fit(self, X, y):
        y = y.flatten()
        if self.n_splits == 1:
            splits = [(np.arange(X.shape[0]), np.arange(X.shape[0]))]
        elif isinstance(self.n_splits, int):
            splits = list(KFold(n_splits=self.n_splits).split(X))
        else:
            splits = self.n_splits

        reisz_pred = np.zeros(X.shape[0])
        reg_pred = np.zeros(X.shape[0])
        moment_pred = np.zeros(X.shape[0])
        moment_reisz = np.zeros(X.shape[0])
        self.reisz_models_ = []
        self.reg_models_ = []
        reisz_fn = self.get_reisz_fn(X)
        reg_fn = self.get_reg_fn(X, y)
        for train, test in splits:
            reisz = reisz_fn().fit(X[train])
            reisz_pred[test] = reisz.predict(X[test])
            reg = reg_fn().fit(X[train], y[train])
            reg_pred[test] = reg.predict(X[test])
            moment_pred[test] = self.moment_fn(X[test], reg.predict)
            moment_reisz[test] = self.moment_fn(X[test], reisz.predict)
            self.reisz_models_.append(reisz)
            self.reg_models_.append(reg)

        self.reg_pred_ = reg_pred
        self.reisz_pred_ = reisz_pred
        self.moment_ = moment_pred + reisz_pred * (y - reg_pred)
        self.moment_reisz_ = moment_reisz
        self.moment_pred_ = moment_pred
        self.y_ = y.copy()

        epsilon = LinearRegression(fit_intercept=False).fit(reisz_pred.reshape(-1, 1), y - reg_pred).coef_[0]
        moment_pred = moment_pred + moment_reisz * epsilon
        reg_pred = reg_pred + epsilon * reisz_pred
        self.tmle_moment_ = moment_pred + reisz_pred * (y - reg_pred)
        return self

    def avg_moment(self, alpha=0.05, tmle=False):
        if tmle:
            return mean_ci(self.tmle_moment_, confidence=1-alpha)    
        return mean_ci(self.moment_, confidence=1-alpha)
