from sklearn.base import BaseEstimator
import numpy as np
from sklearn.base import TransformerMixin, BaseEstimator, clone
from econml.utilities import cross_product
from sklearn.preprocessing import PolynomialFeatures, StandardScaler
import scipy
from scipy.optimize import fmin_l_bfgs_b

class Featurizer(TransformerMixin, BaseEstimator):
    ''' Creates features of the form X[:, 1:] * poly(X[:, 0])
    i.e. some polynomial of the treatment, multiplied by some other feature.
    '''
    
    def __init__(self, *, degree=1, normalize=True):
        self.degree = degree
        self.normalize = normalize

    def _transform(self, X):        
        Tfeats = PolynomialFeatures(degree=self.degree, include_bias=True).fit_transform(X[:, [0]])
        feats = cross_product(Tfeats, X[:, 1:])
        return feats

    def fit(self, X, y=None):
        if self.normalize:
            self.scaler_ = StandardScaler().fit(self._transform(X))
        return self

    def transform(self, X):
        if self.normalize:
            return self.scaler_.transform(self._transform(X))
        return self._transform(X)

class SieveRR(BaseEstimator):
    ''' Linear sieve representation learning with some feature map and
    l2 penalty.
    '''
    
    def __init__(self, *, featurizer, moment_fn, alpha=0):
        self.alpha = alpha
        self.featurizer = featurizer
        self.moment_fn = moment_fn
    
    def fit(self, X):
        self.featurizer_ = clone(self.featurizer).fit(X)
        n_samples = X.shape[0]
        Xfeats = self.featurizer_.transform(X)
        Xfeats = np.hstack([np.ones((Xfeats.shape[0], 1)), Xfeats])
        n_feats = Xfeats.shape[1]
        Mfeats = np.zeros((n_samples, n_feats))
        for t in np.arange(n_feats):
            if t == 0:
                feat_fn = lambda x: np.ones(x.shape[0])
            else:
                feat_fn = lambda x: self.featurizer_.transform(x)[:, t - 1]
            Mfeats[:, t] = self.moment_fn(X, feat_fn)
        Sigma = Xfeats.T @ Xfeats / n_samples
        reg = self.alpha * np.eye(n_feats)
        reg[0, 0] = 0
        Sigma += reg
        params = np.linalg.pinv(Sigma) @ np.mean(Mfeats, axis=0)
        self.coef_ = params[1:]
        self.intercept_ = params[0]

        return self
    
    def predict(self, X):
        return self.featurizer_.transform(X) @ self.coef_ + self.intercept_

class SparseSieveRR(BaseEstimator):
    ''' Linear sieve representation learning with some feature map and
    combination of l1 and l2 penalty.
    '''
    
    def __init__(self, *, featurizer, moment_fn, alpha=0.0, l1_ratio=1.0, tol=1e-4):
        self.featurizer = featurizer
        self.moment_fn = moment_fn
        self.alpha = alpha
        self.l1_ratio = l1_ratio
        self.tol = tol
    
    def fit(self, X):
        self.featurizer_ = clone(self.featurizer).fit(X)
        n_samples = X.shape[0]
        Xfeats = self.featurizer_.transform(X)
        Xfeats = np.hstack([np.ones((Xfeats.shape[0], 1)), Xfeats])
        n_feats = Xfeats.shape[1]
        Mfeats = np.zeros((n_samples, n_feats))
        for t in np.arange(n_feats):
            if t == 0:
                feat_fn = lambda x: np.ones(x.shape[0])
            else:
                feat_fn = lambda x: self.featurizer_.transform(x)[:, t - 1]
            Mfeats[:, t] = self.moment_fn(X, feat_fn)
        Sigma = Xfeats.T @ Xfeats / n_samples
        mfeats = np.mean(Mfeats, axis=0)

        alpha_l1 = self.alpha * self.l1_ratio
        alpha_l2 = self.alpha * (1 - self.l1_ratio)
        def loss_and_jac(extended_coef):
            coef = extended_coef[:n_feats] - extended_coef[n_feats:]
            loss = - 2 * mfeats @ coef + coef.T @ Sigma @ coef
            loss += alpha_l1 * np.sum(extended_coef[1:n_feats]) 
            loss += alpha_l1 * np.sum(extended_coef[n_feats + 1:]) 
            loss += 0.5 * alpha_l2 * np.sum(extended_coef[1:n_feats]**2)
            loss += 0.5 * alpha_l2 * np.sum(extended_coef[n_feats + 1:]**2)
            grad = 2 * (Sigma @ coef - mfeats)
            jac = np.concatenate([grad, -grad])
            jac[1:n_feats] += alpha_l1 + alpha_l2 * extended_coef[1:n_feats]
            jac[n_feats + 1:] += alpha_l1 + alpha_l2 * extended_coef[n_feats + 1:]
            return loss, jac
        
        w, _, _ = fmin_l_bfgs_b(loss_and_jac, np.zeros(2*n_feats),
                                bounds=[(0, None)] * 2 * n_feats,
                                pgtol=self.tol)

        params = w[:n_feats] - w[n_feats:]        
        self.coef_ = params[1:]
        self.intercept_ = params[0]


        return self
    
    def predict(self, X):
        return self.featurizer_.transform(X) @ self.coef_ + self.intercept_