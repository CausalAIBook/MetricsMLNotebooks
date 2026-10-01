import numpy as np
from sklearn.model_selection import cross_val_predict
from sklearn.base import clone
from sklearn.linear_model import LinearRegression

def cross_val_reisz_and_reg(model_reisz, model_reg, X, y, splits, moment_fn):
    reisz_preds = np.zeros(X.shape[0])
    moment_preds = np.zeros(X.shape[0])
    reg_preds = np.zeros(X.shape[0])
    moment_reg_preds = np.zeros(X.shape[0])
    for train, test in splits:
        est = clone(model_reisz)
        est.fit(X[train])
        reisz_preds[test] = est.predict(X[test])
        moment_preds[test] = moment_fn(X[test], est.predict)
        
        est = clone(model_reg)
        est.fit(X[train], y[train])
        reg_preds[test] = est.predict(X[test])
        moment_reg_preds[test] = moment_fn(X[test], est.predict)
    return reisz_preds, moment_preds, reg_preds, moment_reg_preds

def plr_cross_val_reisz_and_reg(model_y, model_t, X, y, splits, moment_fn):
    res_preds_t = np.zeros(X.shape[0])
    moment_preds = np.zeros(X.shape[0])
    moment_reg_preds = np.zeros(X.shape[0])
    reg_preds_y = np.zeros(X.shape[0])
    reg_preds = np.zeros(X.shape[0])
    for train, test in splits:
        model_t_ = clone(model_t)
        model_t_.fit(X[train, 1:], X[train, 0])
        res_preds_t[test] = X[test, 0] - model_t_.predict(X[test, 1:])
        reisz_fn = lambda x: (x[:, 0] - model_t_.predict(x[:, 1:]))
        moment_preds[test] = moment_fn(X[test], reisz_fn)
        
        model_y_ = clone(model_y)
        model_y_.fit(X[train, 1:], y[train])
        reg_preds_y[test] = model_y_.predict(X[test, 1:])
        partial_reg_fn = lambda x: model_y_.predict(x[:, 1:])
        moment_reg_preds[test] = moment_fn(X[test], partial_reg_fn)

    theta = LinearRegression(fit_intercept=False).fit(res_preds_t.reshape(-1, 1), y - reg_preds_y).coef_[0]
    reg_preds = theta * res_preds_t + reg_preds_y
    moment_reg_preds = theta * moment_preds + moment_reg_preds

    reisz_preds = res_preds_t / np.mean(res_preds_t**2)
    moment_preds = moment_preds / np.mean(res_preds_t**2)

    return reisz_preds, moment_preds, reg_preds, moment_reg_preds


def ablation_inference(y, moment, reisz, reg, moment_reg, ablation_groups, verbose):
    phi_a = 2 * moment[0] - reisz[0]**2 # orthogonal moment for E[a(X)^2]
    ma_sq = np.mean(phi_a) # estimate of E[a(X)^2]
    phi_g = 2*y*reg[0] - reg[0]**2 # orthogonal moment for E[g(X)^2]
    mg_sq = np.mean(phi_g) # estimate of E[g(X)^2]
    all_etasq = []
    all_Cy = []
    all_dtheta = []
    all_rho = []
    for it, (name, group) in enumerate(ablation_groups):
        i = it + 1

        # Estimating eta^2 = (E[a(X)^2] - E[a_s(X)^2]) / E[a(X)^2]
        phi_as = 2 * moment[i] - reisz[i]**2 # orthogonal moment for E[a_s(X)^2]
        mas_sq = np.mean(phi_as) # estimate of E[a_s(X)^2]
        etasq = (ma_sq - mas_sq) / ma_sq # estimate of eta^2
        inf = (phi_as - mas_sq)/ma_sq - mas_sq * (phi_a - ma_sq) / (ma_sq**2) # influence function for eta^2 via delta method
        stderr = np.sqrt(np.mean(inf**2) / inf.shape[0]) # standard error for eta^2
        Cd = etasq / (1 - etasq) # estimate of Cd^2 = eta^2 / (1 - eta^2)
        
        # Estimating Cy=E[g(X)^2 - g_s(X)^2] / E[(Y - g_s(X))^2]
        phi_gs = 2*y*reg[i] - reg[i]**2 # orthogonal moment for E[g_s(X)^2]
        mgs_sq = np.mean(phi_gs) # estimate of E[g_s(X)^2]
        phi_ss = (y - reg[i])**2 # orthogonal moment for E[(Y - g_s(X))^2]
        sigma_sq = np.mean(phi_ss) # estimate of sigma^2 = E[(Y - g_s(X))^2]
        Cy = (mg_sq - mgs_sq)/ sigma_sq # estimate of Cy^2
        # influence function for Cy^2 via delta method
        inf_y = (phi_g - mg_sq)/sigma_sq
        inf_y += - (phi_gs - mgs_sq)/sigma_sq
        inf_y += - (mg_sq - mgs_sq) * (phi_ss - sigma_sq) / (sigma_sq**2)
        # standard error for Cy^2
        stderr_y = np.sqrt(np.mean(inf_y**2) / inf_y.shape[0])

        # Estimating dtheta
        phi_dtheta = (moment_reg[i] + (y - reg[i]) * reisz[i])
        phi_dtheta -= (moment_reg[0] + (y - reg[0]) * reisz[0])
        dtheta = np.mean(phi_dtheta)
        stderr_dtheta = np.sqrt(np.mean((phi_dtheta - dtheta)**2) / phi_dtheta.shape[0])

        # Estimating rho
        denom_g = np.sqrt(np.clip(mg_sq - mgs_sq, 1e-6, np.inf))
        denom_a = np.sqrt(np.clip(ma_sq - mas_sq, 1e-6, np.inf))
        rho = dtheta / (denom_g * denom_a)
        phi_rho = (phi_dtheta - dtheta) / (denom_g * denom_a)
        phi_rho += - .5 * (phi_g - mg_sq - phi_gs + mgs_sq) * dtheta / ((denom_g**(3)) * denom_a)
        phi_rho += - .5 * (phi_a - ma_sq - phi_as + mas_sq) * dtheta / (denom_g * (denom_a**(3)))
        stderr_rho = np.sqrt(np.mean(phi_rho**2) / phi_rho.shape[0])

        if verbose:
            print(f'{name}, etasq={etasq:.4f} ({stderr:.4f}), Cd={Cd:.4f}, '
                  f'Cy={Cy:.4f} ({stderr_y:.4f}), RV={min(etasq, Cy):.4f}',
                  f'rho={rho:.4f} ({stderr_rho:.4f})',
                  f'dtheta={dtheta:.4f} ({stderr_dtheta:.4f})')
        all_etasq += [(etasq, stderr)]
        all_Cy += [(Cy, stderr_y)]
        all_dtheta += [(dtheta, stderr_dtheta)]
        all_rho += [(rho, stderr_rho)]
    return all_etasq, all_Cy, all_dtheta, all_rho

def plr_ablation(seed, ablation_groups, moment_fn, get_data_fn, model_y_gen, model_t_gen, verbose):
    X, y, num_cols, splits, column_names = get_data_fn(seed)
    num_cols_t = [t - 1 for t in num_cols[1:]]
    model_t = model_t_gen(num_cols_t, cv=splits, verbose=verbose)(X[:, 1:], X[:, 0])
    model_y = model_y_gen(num_cols_t, cv=splits, verbose=verbose)(X[:, 1:], y)
    if verbose:
        print('model_t', model_t())
        print('model_y', model_y())
    
    rp, mp, regp, mregp = plr_cross_val_reisz_and_reg(model_y(), model_t(), X, y, splits, moment_fn)
    reisz, moment, reg, moment_reg = [rp], [mp], [regp], [mregp]

    for name, group in ablation_groups:
        if verbose:
            print(name)
        Xmi = X[:, np.setdiff1d(np.arange(len(column_names)), group)]
        rp, mp, regp, mregp = plr_cross_val_reisz_and_reg(model_y(), model_t(), Xmi, y, splits, moment_fn)
        reisz += [rp]
        moment += [mp]
        reg += [regp]
        moment_reg += [mregp]

    return ablation_inference(y, moment, reisz, reg, moment_reg, ablation_groups, verbose)

def ablation(seed, ablation_groups, moment_fn, get_data_fn, reg_fn_gen, rr_fn_gen, verbose):
    X, y, num_cols, splits, column_names = get_data_fn(seed)
    reisz_fn = rr_fn_gen(moment_fn, num_cols, cv=splits, verbose=verbose)(X)
    reg_fn = reg_fn_gen(num_cols, cv=splits, verbose=verbose)(X, y)
    if verbose:
        print('reisz_model', reisz_fn())
        print('reg_model', reg_fn())
    
    rp, mp, regp, mregp = cross_val_reisz_and_reg(reisz_fn(), reg_fn(), X, y, splits, moment_fn)
    reisz, moment, reg, moment_reg = [rp], [mp], [regp], [mregp]

    for name, group in ablation_groups:
        if verbose:
            print(name)
        Xmi = X[:, np.setdiff1d(np.arange(len(column_names)), group)]
        rp, mp, regp, mregp = cross_val_reisz_and_reg(reisz_fn(), reg_fn(), Xmi, y, splits, moment_fn)
        reisz += [rp]
        moment += [mp]
        reg += [regp]
        moment_reg += [mregp]

    return ablation_inference(y, moment, reisz, reg, moment_reg, ablation_groups, verbose)


def aggregate(array, median=True):
    if median:
        agg_point = np.median([r[0] for r in array])
        agg_stderr = np.sqrt(np.median([r[1]**2 + (r[0] - agg_point)**2 for r in array]))
    else:
        agg_point = np.mean([r[0] for r in array])
        agg_stderr = np.sqrt(np.mean([r[1]**2 for r in array]) + np.var([r[0] for r in array]))
    return (agg_point, agg_stderr)

def process_multiseed(ablation_groups, all_etasq_list, all_Cy_list, all_dtheta_list, all_rho_list, median=True):
    all_etasq = list(zip(*all_etasq_list))
    all_Cy = list(zip(*all_Cy_list))
    all_dtheta = list(zip(*all_dtheta_list))
    all_rho = list(zip(*all_rho_list))
    multiseed_etasq = {}
    multiseed_Cy = {}
    multiseed_dtheta = {}
    multiseed_rho = {}
    for it, (name, _) in enumerate(ablation_groups):
        multiseed_etasq[name] = aggregate(all_etasq[it], median=median)
        multiseed_Cy[name] = aggregate(all_Cy[it], median=median)
        multiseed_dtheta[name] = aggregate(all_dtheta[it], median=median)
        multiseed_rho[name] = aggregate(all_rho[it], median=median)
    return multiseed_etasq, multiseed_Cy, multiseed_dtheta, multiseed_rho


def plr_ablation_multiseed(ablation_groups, moment_fn, get_data_fn, model_y_gen, model_t_gen, verbose, median=True):
    all_etasq_list, all_Cy_list, all_dtheta_list, all_rho_list = [], [], [], []
    for seed in np.arange(123, 128):
        all_etasq, all_Cy, all_dtheta, all_rho = plr_ablation(seed, ablation_groups, moment_fn, get_data_fn, model_y_gen, model_t_gen, verbose)
        all_etasq_list.append(all_etasq)
        all_Cy_list.append(all_Cy)
        all_dtheta_list.append(all_dtheta)
        all_rho_list.append(all_rho)
    return process_multiseed(ablation_groups, all_etasq_list, all_Cy_list, all_dtheta_list, all_rho_list, median=median)


def ablation_multiseed(ablation_groups, moment_fn, get_data_fn, reg_fn_gen, rr_fn_gen, verbose, median=True):
    all_etasq_list, all_Cy_list, all_dtheta_list, all_rho_list = [], [], [], []
    for seed in np.arange(123, 128):
        all_etasq, all_Cy, all_dtheta, all_rho = ablation(seed, ablation_groups, moment_fn, get_data_fn, reg_fn_gen, rr_fn_gen, verbose)
        all_etasq_list.append(all_etasq)
        all_Cy_list.append(all_Cy)
        all_dtheta_list.append(all_dtheta)
        all_rho_list.append(all_rho)
    return process_multiseed(ablation_groups, all_etasq_list, all_Cy_list, all_dtheta_list, all_rho_list, median=median)