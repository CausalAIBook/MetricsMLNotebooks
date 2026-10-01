import numpy as np
import scipy

def moment_estimate(est, inds=None):
    if inds is None:
        inds = np.arange(est.moment_reisz_.shape[0])
    theta = np.mean(est.moment_[inds])
    psi_theta = est.moment_[inds] - theta
    return theta, np.sqrt(np.mean(psi_theta**2) / psi_theta.shape[0])

def sensitivity_bounds_single(est, eta_ysq, eta_asq, alpha=None, inds=None):
    ''' Sensitivity bound based on a single trained doubly robust
    average moment model and for some sensitivity parameters `eta_ysq`
    and `eta_asq`. If `alpha` is float, incorporates sampling uncertainty
    at the `alpha` level.  If `inds` is specified, then only samples in `inds`
    are used for the estimation and reporting. Useful for sub-group analysis.
    '''
    if inds is None:
        inds = np.arange(est.moment_reisz_.shape[0])
    nusq = np.mean(2 * est.moment_reisz_[inds] - est.reisz_pred_[inds]**2)
    sigmasq = np.mean((est.y_[inds] - est.reg_pred_[inds])**2)
    S = np.sqrt(sigmasq * nusq)

    Casq = eta_asq / (1 - eta_asq)
    Cgsq = eta_ysq
    theta = np.mean(est.moment_[inds])
    error = S * np.sqrt(Casq * Cgsq)

    if alpha is None:
        return theta - error, theta + error

    psi_theta = est.moment_[inds] - theta
    psi_sigmasq = (est.y_[inds] - est.reg_pred_[inds])**2 - sigmasq
    psi_nusq = 2 * est.moment_reisz_[inds] - est.reisz_pred_[inds]**2 - nusq

    phi_plus = psi_theta + (np.sqrt(Casq * Cgsq) / (2 * S)) * (sigmasq * psi_nusq + nusq * psi_sigmasq)
    stderr_plus = np.sqrt(np.mean(phi_plus**2) / phi_plus.shape[0])

    phi_minus = psi_theta - (np.sqrt(Casq * Cgsq) / (2 * S)) * (sigmasq * psi_nusq + nusq * psi_sigmasq)
    stderr_minus = np.sqrt(np.mean(phi_minus**2) / phi_minus.shape[0])
    return theta - error, stderr_minus, theta + error, stderr_plus

def sensitivity_bounds(est_list, eta_ysq, eta_asq, alpha=None, inds=None):
    ''' Sensitivity bound based on a many trained doubly robust
    average moment models and for some sensitivity parameters `eta_ysq`
    and `eta_asq`. If `alpha` is float, incorporates sampling uncertainty
    at the `alpha` level. If `inds` is specified, then only samples in `inds`
    are used for the estimation and reporting. Useful for sub-group analysis.
    '''
    if alpha is None:
        lower, upper = zip(*[sensitivity_bounds_single(est, eta_ysq, eta_asq, alpha=alpha, inds=inds) for est in est_list])
        return np.mean(lower), np.mean(upper)
    else:
        lower, std_lower, upper, std_upper = zip(*[sensitivity_bounds_single(est, eta_ysq, eta_asq, alpha=alpha, inds=inds)
                                                   for est in est_list])
        std_lower = np.array(std_lower)
        std_upper = np.array(std_upper)
        lower = np.mean(lower) - scipy.stats.norm.ppf(1 - alpha) * np.sqrt(np.mean(std_lower**2) + np.var(lower))      
        upper = np.mean(upper) + scipy.stats.norm.ppf(1 - alpha) * np.sqrt(np.mean(std_upper**2) + np.var(upper))
        return lower, upper

def tvalue(est_list, value=0, leq=None, alpha=None, inds=None):
    ''' Robustness value based on a many trained doubly robust
    average moment models and for some target `value`. If `alpha` is
    float, incorporates sampling uncertainty at the `alpha` level.
    If `leq=True`, then checks that the bounds interval overlaps with the
    `[-infty, value]` interval, and if `leq=False`, checks if it overlaps
    with the `[value, infty]` interval. Otherwise, if `leq=None` it checks
    if the bounds interval contains `value`. If `inds` is specified, then
    only samples in `inds` are used for the estimation and reporting.
    Useful for sub-group analysis.
    '''
    for t in np.linspace(0, 1, 1000):
        l, u = sensitivity_bounds(est_list, t, t, alpha=alpha, inds=inds)
        if (leq is None) & (value >= l) & (value <= u):
            break
        if (leq == True) & (value >= l):
            break
        if (leq == False) & (value <= u):
            break
    return t

def sensitivity_contours(est_list, a_upper, y_upper, alpha=None, inds=None):
    ''' Sensitivity bounds contour plots based on a many trained doubly robust
    average moment models. If `alpha` is float, incorporates sampling uncertainty
    at the `alpha` level. Sensitivity parameter `eta_ysq` ranges in `[0, y_upper]`
    and parameter `eta_asq` ranges in `[0, a_upper]`.
    '''
    xlist = np.linspace(0, a_upper, 100)
    ylist = np.linspace(0, y_upper, 100)
    X, Y = np.meshgrid(xlist, ylist)
    Zlower = np.zeros(X.shape)
    Zupper = np.zeros(X.shape)
    for itx in np.arange(X.shape[1]):
        for ity in np.arange(X.shape[0]):
            l, u = sensitivity_bounds(est_list, Y[ity, itx], X[ity, itx], alpha=alpha, inds=inds)
            Zlower[ity, itx] = l
            Zupper[ity, itx] = u
    
    return X, Y, Zlower, Zupper

def dml_sensitivity_contours(est_list, a_upper, y_upper, alpha=None, inds=None):
    ''' Specialized for linear DML. Sensitivity bounds contour plots based on a many trained doubly robust
    average moment models. If `alpha` is float, incorporates sampling uncertainty
    at the `alpha` level. Sensitivity parameter `eta_ysq` ranges in `[0, y_upper]`
    and parameter `eta_asq` ranges in `[0, a_upper]`.
    '''
    xlist = np.linspace(0, a_upper, 100)
    ylist = np.linspace(0, y_upper, 100)
    X, Y = np.meshgrid(xlist, ylist)
    Zlower = np.zeros(X.shape)
    Zupper = np.zeros(X.shape)
    for itx in np.arange(X.shape[1]):
        for ity in np.arange(X.shape[0]):
            l, u = dml_sensitivity_bounds(est_list, Y[ity, itx], X[ity, itx], alpha=alpha, inds=inds)
            Zlower[ity, itx] = l
            Zupper[ity, itx] = u
    
    return X, Y, Zlower, Zupper

def dml_estimate(est, inds=None):
    if inds is None:
        inds = np.arange(est.residuals_[0].shape[0])
    yres, Tres, _, _ = est.residuals_
    yres, Tres = yres[inds], Tres[inds]
    nusq = np.mean(Tres ** 2)
    theta = np.mean(yres * Tres) / nusq
    psi_theta = (yres - Tres * theta) * Tres / nusq
    return theta, np.sqrt(np.mean(psi_theta**2)/psi_theta.shape[0])
    
def dml_sensitivity_bounds_single(est, eta_ysq, eta_asq, alpha=None, inds=None):
    ''' Sensitivity analysis, specialized for the partially linear DML moment
    E[(yres - theta * Tres) * Tres]. `est` is a `LinearDML` estimator fitted
    with `cache_values=True` so that residuals are being stored after fitting.
    '''
    if inds is None:
        inds = np.arange(est.residuals_[0].shape[0])
    yres, Tres, _, _ = est.residuals_
    yres, Tres = yres[inds], Tres[inds]
    nusq = np.mean(Tres ** 2)
    theta = np.mean(yres * Tres) / nusq
    sigmasq = np.mean((yres - Tres * theta)**2)
    S = np.sqrt(sigmasq / nusq)
    Casq = eta_asq / (1 - eta_asq)
    Cgsq = eta_ysq
    error = S * np.sqrt(Casq * Cgsq)

    if alpha is None:
        return theta - error, theta + error

    psi_theta = (yres - Tres * theta) * Tres / nusq
    psi_sigmasq = (yres - Tres * theta)**2 - sigmasq
    psi_nusq = Tres**2 - nusq

    phi_plus = psi_theta + (np.sqrt(Casq * Cgsq) / (2 * S)) * (-(sigmasq/(nusq**2)) * psi_nusq + (1/nusq) * psi_sigmasq)
    stderr_plus = np.sqrt(np.mean(phi_plus**2) / phi_plus.shape[0])

    phi_minus = psi_theta - (np.sqrt(Casq * Cgsq) / (2 * S)) * (-(sigmasq/(nusq**2)) * psi_nusq + (1/nusq) * psi_sigmasq)
    stderr_minus = np.sqrt(np.mean(phi_minus**2) / phi_minus.shape[0])
    return theta - error, stderr_minus, theta + error, stderr_plus

def dml_sensitivity_bounds(est_list, eta_ysq, eta_asq, alpha=None, inds=None):
    if alpha is None:
        lower, upper = zip(*[dml_sensitivity_bounds_single(est, eta_ysq, eta_asq, alpha=alpha, inds=inds)
                             for est in est_list])
        return np.mean(lower), np.mean(upper)
    else:
        lower, std_lower, upper, std_upper = zip(*[dml_sensitivity_bounds_single(est, eta_ysq, eta_asq, alpha=alpha, inds=inds)
                                                   for est in est_list])
        std_lower = np.array(std_lower)
        std_upper = np.array(std_upper)
        lower = np.mean(lower) - scipy.stats.norm.ppf(1 - alpha) * np.sqrt(np.mean(std_lower**2) + np.var(lower))        
        upper = np.mean(upper) + scipy.stats.norm.ppf(1 - alpha) * np.sqrt(np.mean(std_upper**2) + np.var(upper))
        return lower, upper

def dml_tvalue(est, value=0, leq=None, alpha=None, inds=None):
    for t in np.linspace(0, 1, 1000):
        l, u = dml_sensitivity_bounds(est, t, t, alpha=alpha, inds=inds)
        if (leq is None) & (value >= l) & (value <= u):
            break
        if (leq == True) & (value >= l):
            break
        if (leq == False) & (value <= u):
            break
    return t
