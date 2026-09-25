# PM-4 figures: one learner throughout

`dml_illustration_sim.py` fits **the same neural-network regressor
in every nuisance regression**, with identical architecture and hyperparameters.

## Run

```sh
python -m pip install -r notebooks/requirements-pm4.txt
python notebooks/dml_illustration_sim.py
```

`python notebooks/SimOrthogonality.py` and `python notebooks/SimCrossFitting.py` are equivalent
entry points for making just one figure; both reuse the same simulation cache.

Defaults are 500 repetitions, n=1,000, p=10, and up to eight parallel processes.
Use `--reps 1000` to extend the saved run, or `--jobs 2` to reduce CPU use.
The simulation checkpoints every 25 completed repetitions and resumes on a
subsequent invocation. Replot cached results with `--plot-only`.

Results are saved in `notebooks/figures/pm4_uniform/` as:

- `SimOrthogonality.pdf` and `.png`;
- `SimCrossFitting.pdf` and `.png`;
- `simulation.npz`, `simulation.csv`, `summary.json`, and `plot_settings.json`.

The uploaded figures in `notebooks/figures/pm4/` are not replaced by this script.
Changing simulation parameters requires a different `--output-dir`. Replotting
is possible with newer library versions; resuming a simulation requires the
versions recorded in its metadata so one run cannot mix implementations.

## Shared design

In both figures, each repetition draws independent controls X1,...,X10 and
independent errors V and epsilon, all standard normal. The partially linear
model is

```text
Q = X2^2 - 1
D = X1 + 0.2 Q + V
Y = 0.5 D + 2 Q + epsilon.
```

Thus the true treatment effect is 0.5. The nonlinear component is relatively
weak in treatment prediction but important in outcome prediction. This is a
standard weak-confounding-signal example: small errors in predicting treatment
can matter for an unprotected estimating equation. The population nuisance
functions enter only data generation and optional prediction-error diagnostics.

## One learning algorithm

Every fitted nuisance function uses `sklearn.neural_network.MLPRegressor`:

- two hidden layers, each with 128 ReLU units;
- Adam, learning rate 0.001, minibatches of 128 observations;
- L2 penalty `alpha=0.01`;
- 100 training epochs, no validation-based early stopping;
- feature standardization and target centering learned on the training set only;
- targets are not divided by their standard deviations, so the same penalty and
  squared-error objective apply in original outcome/treatment units.

The network has 18,049 parameters for ten inputs, compared with 500 observations
in each training fold. Treatment and outcome are fitted in **separate scalar-output
networks**; there is no special multi-output architecture. Within a training set,
the two fits use the same random seed, giving matched initialization and batch
order. They do not pass fitted weights or predictions to one another. All these
settings are also used for the full-sample fits in Figure 2.

## Estimators and why the nonorthogonal estimator is defensible

Let m0(X) = E[D|X], ell0(X) = E[Y|X], and V = D - m0(X). In a partially linear
model, V is a valid instrument for D in the outcome equation:

```text
E[(D - m0(X)) (Y - beta D)] = 0.
```

This moment identifies beta because E[V g0(X)] = E[V epsilon] = 0 and
E[V D] = E[V^2] > 0. Its directional derivative with respect to m is
`-E[h(X) g0(X)]`, which generally does not vanish. It is therefore a valid,
nonorthogonal moment, rather than an inconsistent estimator constructed to fail.
It is also the treatment-residual IV moment already computed in the original
`Figure1.m` (`rfsss`), although that script's main plotted comparison used its
other nonorthogonal estimator.

For the two cross-fitted estimators, randomly divide the observations into two
folds. Train the same network procedure for D and Y on one fold, predict on the
other, reverse the roles, and pool all held-out predictions. Write v = D - m_hat.

1. **Nonorthogonal, cross-fitted:** `sum(v * Y) / sum(v * D)`.
   This uses the learned treatment residual as an instrument and does not
   residualize the outcome. It needs no g_hat or preliminary coefficient.
2. **Orthogonal, cross-fitted:** `sum(v * (Y - ell_hat)) / sum(v**2)`.
   This is the chapter's partialling-out estimator.
3. **Orthogonal, without cross-fitting:** use formula 2 with nuisance predictions
   made on the same observations used to train both networks.

Figure 1 compares estimators 1 and 2, using **exactly the same treatment
predictions**. Figure 2 compares estimators 3 and 2. The blue distribution is
identical in both figures. No extra learner is used in any stage.

The nonorthogonal estimator also has higher variance even with oracle m0, since
it leaves g0(X) in the outcome. Its broader histogram is not exclusively an
effect of nuisance estimation. The visible displacement from zero is the
regularization-bias contrast; the narrower blue distribution is an additional
benefit of partialling out the outcome.

## Plotting and reproducibility

The completed validation run used 500 fresh-seed repetitions. With true effect
0.5, it produced:

| Estimator | Mean estimate | Mean estimation error |
| --- | ---: | ---: |
| Nonorthogonal, cross-fitted | 0.5824 | +0.0824 |
| Orthogonal, without cross-fitting | 0.6245 | +0.1245 |
| Orthogonal, cross-fitted | 0.4864 | -0.0136 |

Both comparisons show a rightward displacement relative to the cross-fitted
orthogonal estimate. The latter retains modest finite-sample bias; the plot does
not correct it. All score denominators stayed away from zero (minimum normalized
nonorthogonal denominator 0.814; minimum full-sample orthogonal denominator 0.108).

Both plots use raw estimation error (estimate minus 0.5), the same bins and both
axis limits, a zero reference line, and the same blue cross-fitted estimates.

Each repetition has its own seed, independent of worker scheduling. The final
validation seed 726031 is different from the exploration seed 395721. All
500 validation repetitions are retained. Network iteration-limit warnings are
recorded in the diagnostics because a fixed epoch budget is intentional.

Prediction diagnostics distinguish training error, held-out error including
outcome noise, and error relative to the true conditional means. Score
denominators are saved to make numerical instability visible rather than
silently filtering repetitions. Monte Carlo standard deviations in the summary
describe the saved distributions; they are not estimated sampling standard
errors and never enter the plots.

## Exploration record

Pilots compared random forests, boosted trees, and neural networks across
linear, smooth nonlinear, interaction, and weak-confounding designs. Uniform
tree learners often made the cross-fitting comparison small; several network
configurations left appreciable bias after cross-fitting. A joint-output network
gave a strong cross-fitting example, but is not needed in the selected design.

The selected weak quadratic design gave both contrasts using two ordinary
scalar-output networks and identical settings everywhere. A clean standalone
implementation was checked against the corresponding pilot before running the
fresh-seed validation. 
