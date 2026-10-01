"""PM-4 figures using the same neural-network learner for every nuisance fit.

Run `python notebooks/dml_illustration_sim.py` to simulate and draw both figures.
All figures show raw estimation errors. No standard errors are estimated.
See README_PM4_uniform.md for the design and the nonorthogonal moment equation.
"""
from __future__ import annotations

import argparse
from dataclasses import asdict, dataclass
import csv
import json
import os
from pathlib import Path
import platform
import time
import warnings

import numpy as np
import sklearn
from joblib import Parallel, delayed, parallel_config
from sklearn.exceptions import ConvergenceWarning
from sklearn.neural_network import MLPRegressor
from sklearn.preprocessing import StandardScaler
from threadpoolctl import threadpool_limits

DEFAULT_OUTPUT = Path(__file__).resolve().parent / "figures" / "pm4_uniform"
ESTIMATORS = ("nonorthogonal_crossfit", "orthogonal_insample", "orthogonal_crossfit")
COLUMNS = ESTIMATORS + (
    "m_train_mse", "ell_train_mse", "m_heldout_mse", "ell_heldout_mse",
    "m_function_mse", "ell_function_mse", "full_m_train_mse",
    "full_ell_train_mse", "nonorthogonal_denominator", "orthogonal_denominator",
    "full_orthogonal_denominator", "training_iteration_limits",
)


@dataclass(frozen=True)
class Config:
    n: int = 1000
    p: int = 10
    beta: float = 0.5
    seed: int = 726031
    width: int = 128
    epochs: int = 100
    alpha: float = 0.01
    learning_rate: float = 0.001
    treatment_curvature: float = 0.2
    outcome_curvature: float = 2.0


def generate_data(config, rng):
    """Independent Gaussian controls, one weak nonlinear confounding component."""
    x = rng.standard_normal((config.n, config.p))
    q = x[:, 1] ** 2 - 1
    m = x[:, 0] + config.treatment_curvature * q
    g = config.outcome_curvature * q
    d = m + rng.standard_normal(config.n)
    y = config.beta * d + g + rng.standard_normal(config.n)
    return x, d, y, m, config.beta * m + g


def fit_nuisances(x_train, d_train, y_train, config, seed):
    """Two separate scalar-output fits of exactly the same learning algorithm.

    Inputs are standardized using this training sample only. Targets are
    centered on training means, but not rescaled: both networks minimize squared
    error in the variables' original units with the same L2 penalty. The shared
    random_state makes initialization and minibatch order reproducible and equal.
    No coefficients, weights, or predictions are passed between the networks.
    """
    scaler = StandardScaler().fit(x_train)
    scaled = scaler.transform(x_train)
    models, means = [], []
    limits = 0
    for target in (d_train, y_train):
        model = MLPRegressor(
            hidden_layer_sizes=(config.width, config.width), activation="relu",
            solver="adam", alpha=config.alpha, batch_size=min(128, len(x_train)),
            learning_rate_init=config.learning_rate, max_iter=config.epochs,
            early_stopping=False, tol=0, n_iter_no_change=config.epochs + 1,
            random_state=seed,
        )
        mean = float(target.mean())
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always", ConvergenceWarning)
            model.fit(scaled, target - mean)
        limits += sum(issubclass(w.category, ConvergenceWarning) for w in caught)
        models.append(model)
        means.append(mean)

    def predict(x):
        z = scaler.transform(x)
        return tuple(model.predict(z) + mean for model, mean in zip(models, means))

    return predict, limits


def score_estimates(y, d, m_hat, ell_hat):
    """Valid nonorthogonal IV score and orthogonal partialling-out score.

    At the population m0, V = D-m0(X) has conditional mean zero and is a valid
    instrument for D in Y = beta D + g0(X) + epsilon. Thus E[V(Y-beta D)]=0.
    Its derivative with respect to m in direction h is -E[h(X)g0(X)], generally
    nonzero. Partialling out Y as well gives the Neyman-orthogonal score.
    """
    v = d - m_hat
    nonorth_denom = float(v @ d)
    orth_denom = float(v @ v)
    if abs(nonorth_denom) <= np.finfo(float).eps or orth_denom <= np.finfo(float).eps:
        raise ValueError("Numerically singular score denominator; no repetition was discarded.")
    return (
        float(v @ y / nonorth_denom),
        float(v @ (y - ell_hat) / orth_denom),
        nonorth_denom / len(d), orth_denom / len(d),
    )


def mse(a, b):
    return float(np.mean((a - b) ** 2))


def one_replication(rep, config):
    # Separate deterministic streams for data and model fitting; independent of
    # worker scheduling. This reproduces the pilot stream layout on a fresh seed.
    seeds = np.random.SeedSequence([config.seed, rep]).spawn(10)
    rng = np.random.default_rng(seeds[0])
    model_seeds = [int(s.generate_state(1)[0]) for s in seeds[1:]]
    with threadpool_limits(limits=1):
        x, d, y, m0, ell0 = generate_data(config, rng)
        folds = np.array_split(rng.permutation(config.n), 2)
        m_hat, ell_hat = np.empty(config.n), np.empty(config.n)
        training_mse = np.zeros(2)
        limits = 0
        for k in range(2):
            test, train = folds[k], folds[1 - k]
            predict, count = fit_nuisances(x[train], d[train], y[train], config, model_seeds[2 * k])
            m_hat[test], ell_hat[test] = predict(x[test])
            mt, lt = predict(x[train])
            training_mse += 0.5 * np.array([mse(d[train], mt), mse(y[train], lt)])
            limits += count

        naive, cf, naive_denom, cf_denom = score_estimates(y, d, m_hat, ell_hat)
        predict, count = fit_nuisances(x, d, y, config, model_seeds[4])
        full_m, full_l = predict(x)
        full_v = d - full_m
        full_denom = float(full_v @ full_v)
        if full_denom <= np.finfo(float).eps:
            raise ValueError("Numerically zero training residual variation; no repetition was discarded.")
        full = float(full_v @ (y - full_l) / full_denom)
        row = np.array([
            naive, full, cf, *training_mse, mse(d, m_hat), mse(y, ell_hat),
            mse(m_hat, m0), mse(ell_hat, ell0), mse(d, full_m), mse(y, full_l),
            naive_denom, cf_denom, full_denom / config.n, limits + count,
        ])
        if row.shape != (len(COLUMNS),) or not np.isfinite(row).all():
            raise ValueError(f"Invalid result in repetition {rep}; no repetition was discarded.")
        return rep, row


def metadata(config):
    return {
        "simulation_version": 1, "config": asdict(config), "columns": list(COLUMNS),
        "python_version": platform.python_version(), "numpy_version": np.__version__,
        "sklearn_version": sklearn.__version__,
    }


def save_cache(path, meta, rows):
    temporary = path.with_suffix(".tmp.npz")
    np.savez_compressed(temporary, metadata=json.dumps(meta, sort_keys=True), results=rows)
    for attempt in range(10):
        try:
            os.replace(temporary, path)
            return
        except PermissionError:
            if attempt == 9:
                raise
            time.sleep(min(0.2 * 2 ** attempt, 2.0))


def simulate(config, reps, jobs, directory, plot_only=False):
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / "simulation.npz"
    meta, saved_meta = metadata(config), None
    rows = np.full((reps, len(COLUMNS)), np.nan)
    if path.exists():
        with np.load(path, allow_pickle=False) as saved:
            saved_meta = json.loads(str(saved["metadata"]))
            if any(saved_meta[k] != meta[k] for k in ("simulation_version", "config", "columns")):
                raise ValueError("Different cached configuration; choose a new --output-dir.")
            previous = saved["results"]
            if len(previous) > reps:
                raise ValueError(f"Cache has {len(previous)} repetitions; increase --reps.")
            rows[:len(previous)] = previous
    pending = np.flatnonzero(~np.isfinite(rows).all(axis=1))
    if saved_meta is not None:
        if len(pending) and meta != saved_meta:
            raise ValueError("Software versions differ; use a new --output-dir to simulate.")
        meta = saved_meta
    if plot_only and len(pending):
        raise ValueError("The simulation is incomplete; omit --plot-only to compute missing repetitions.")
    if len(pending):
        done, start = reps - len(pending), time.monotonic()
        print(f"Running {len(pending)} repetitions ({done} cached).", flush=True)
        try:
            with parallel_config(backend="loky", inner_max_num_threads=1), Parallel(
                n_jobs=jobs, return_as="generator_unordered"
            ) as pool:
                for rep, row in pool(delayed(one_replication)(int(r), config) for r in pending):
                    rows[rep] = row
                    done += 1
                    if done % 25 == 0 or done == reps:
                        save_cache(path, meta, rows)
                        print(f"  {done}/{reps} complete ({time.monotonic()-start:.0f}s)", flush=True)
        finally:
            save_cache(path, meta, rows)
    return rows, meta


def write_results(rows, meta, directory):
    with (directory / "simulation.csv").open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(("replication",) + COLUMNS)
        writer.writerows((r, *row) for r, row in enumerate(rows))
    summary = {
        "metadata": meta, "repetitions": len(rows),
        "estimators": {
            name: {"mean": float(rows[:, k].mean()),
                   "bias": float(rows[:, k].mean() - meta["config"]["beta"]),
                   "monte_carlo_sd": float(rows[:, k].std(ddof=1))}
            for k, name in enumerate(ESTIMATORS)
        },
        "mean_diagnostics": {name: float(rows[:, k].mean()) for k, name in enumerate(COLUMNS) if k >= 3},
    }
    (directory / "summary.json").write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
    return summary


def plot_figures(rows, config, directory, figures=(1, 2), bins=40):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import MaxNLocator
    error = rows[:, :3] - config.beta
    lo, hi = min(float(error.min()), 0.), max(float(error.max()), 0.)
    pad = max(.035 * (hi - lo), .01)
    edges = np.linspace(lo - pad, hi + pad, bins + 1)
    ymax = 1.12 * max(np.histogram(error[:, j], edges, density=True)[0].max() for j in range(3))
    plot_cases = {
        1: (0, "Nonorthogonal, cross-fitted", "SimOrthogonality"),
        2: (1, "Orthogonal, without cross-fitting", "SimCrossFitting"),
    }
    style = {"font.family": "sans-serif", "font.size": 12, "axes.labelsize": 14,
             "axes.spines.top": False, "axes.spines.right": False, "axes.linewidth": .9,
             "xtick.labelsize": 11, "ytick.labelsize": 11, "pdf.fonttype": 42,
             "savefig.facecolor": "white", "figure.facecolor": "white"}
    with plt.rc_context(style):
        for number in figures:
            j, label, filename = plot_cases[number]
            fig, ax = plt.subplots(figsize=(8., 4.6))
            fig.subplots_adjust(left=.10, right=.985, bottom=.17, top=.79)
            for col, color, name in ((j, "#E69F69", label), (2, "#6BAED0", "Orthogonal, cross-fitted")):
                ax.hist(error[:, col], bins=edges, density=True, alpha=.65, color=color,
                        edgecolor="white", linewidth=.4, label=name)
            ax.axvline(0, color="#4A4A4A", linestyle=(0, (5, 4)), linewidth=1.25, zorder=3)
            ax.set(xlim=(edges[0], edges[-1]), ylim=(0, ymax), ylabel="Density",
                   xlabel=r"Estimation error, $\widehat{\beta}-\beta$")
            ax.xaxis.set_major_locator(MaxNLocator(nbins=7))
            ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
            ax.tick_params(direction="out", length=4, width=.9)
            ax.legend(loc="lower center", bbox_to_anchor=(.5, 1.025), frameon=False,
                      fontsize=11, handlelength=1.6, labelspacing=.5)
            for suffix in ("pdf", "png"):
                fig.savefig(directory / f"{filename}.{suffix}", dpi=220)
            plt.close(fig)
    (directory / "plot_settings.json").write_text(json.dumps({
        "quantity": "estimate minus true beta; no studentization",
        "bin_edges": edges.tolist(), "y_limits": [0, float(ymax)],
        "all_repetitions_included": True,
    }, indent=2) + "\n", encoding="utf-8")


def main(figures=(1, 2)):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reps", type=int, default=500)
    parser.add_argument("--jobs", type=int, default=min(8, os.cpu_count() or 1))
    for name in ("n", "p", "seed", "width", "epochs"):
        parser.add_argument(f"--{name}", type=int, default=getattr(Config(), name))
    for name in ("alpha", "learning_rate", "treatment_curvature", "outcome_curvature"):
        parser.add_argument(f"--{name.replace('_', '-')}", type=float, default=getattr(Config(), name))
    parser.add_argument("--bins", type=int, default=40)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--plot-only", action="store_true")
    args = parser.parse_args()
    if min(args.reps, args.n, args.p) < 2 or min(args.width, args.epochs, args.bins) < 1 or args.jobs == 0:
        parser.error("Invalid sample, model, bin, or process count.")
    if args.seed < 0 or args.alpha < 0 or args.learning_rate <= 0 or not all(np.isfinite(getattr(args, k)) for k in (
        "alpha", "learning_rate", "treatment_curvature", "outcome_curvature"
    )):
        parser.error("Invalid seed, regularization, learning rate, or DGP coefficient.")
    config = Config(**{k: getattr(args, k) for k in asdict(Config()) if k != "beta"})
    rows, meta = simulate(config, args.reps, args.jobs, args.output_dir, args.plot_only)
    summary = write_results(rows, meta, args.output_dir)
    plot_figures(rows, config, args.output_dir, figures, args.bins)
    for name, stats in summary["estimators"].items():
        print(f"{name}: mean={stats['mean']:.4f}, bias={stats['bias']:+.4f}")
    print(f"Saved figures and results to {args.output_dir.resolve()}")


if __name__ == "__main__":
    main()
