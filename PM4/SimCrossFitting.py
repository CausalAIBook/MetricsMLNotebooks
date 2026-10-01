"""PM-4 Figure 2: orthogonal estimation with and without cross-fitting.

Arguments are shared with dml_illustration_sim.py. All nuisance predictions come from
feasible fitted learners; no oracle predictions or manufactured fitting errors.
"""
from dml_illustration_sim import main

if __name__ == "__main__":
    main(figures=(2,))
