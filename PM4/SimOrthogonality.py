"""PM-4 Figure 1: nonorthogonal versus orthogonal, both cross-fitted.

Arguments are shared with dml_illustration_sim.py. The simulation cache is also shared
with SimCrossFitting.py, so making the second figure does not repeat the experiment.
"""
from dml_illustration_sim import main

if __name__ == "__main__":
    main(figures=(1,))
