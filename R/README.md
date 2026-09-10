# R/

Function definitions for the seabird OWED prioritization pipeline. Each file
is sourced by one or more scripts in `scripts/`; none of these files are run
directly.

| File                 | Sourced by                          | Defines |
|----------------------|--------------------------------------|---------|
| `exposure.R`         | `01_exposure.R`                     | Cleans expert weights, flags/drops outlier bootstrap iterations, combines seasonal bootstraps into annual distributions, builds elicited-species distributions, assembles WEA polygons, calculates and rescales proportional overlap |
| `sensitivity.R`      | `02_sensitivity.R`                  | Cleans and combines collision/displacement vulnerability scores (Kelsey et al. 2025), rescales to the framework's [0.5, 2.0] range |
| `status.R`           | `03_status.R`                       | Maps IUCN Red List status to the framework's [0.5, 2.0] range |
| `priority.R`         | `04_multiply.R`                     | Combines rescaled exposure, sensitivity, and status into vulnerability (priority) scores |
| `results.R`          | `05_plot.R`, `06_sensitivity_analysis.R` | Monte Carlo rank resampling (propagates exposure uncertainty into rank space), rank summaries, and the manuscript results table |
| `visualizations.R`   | `05_plot.R`, `06_sensitivity_analysis.R` | Priority-score boxplot, vulnerability scatter plot, rank ridge plot, and shared color-mapping helpers |

## Pipeline order

Functions are written to be sourced in the order the pipeline runs
(`01` → `07`; see `scripts/README.md`), but each file is self-contained and
can be sourced independently once its required inputs exist in `output/`.

## Processing parameters

Two parameters govern the main exposure pipeline and are exposed as function
arguments (default = the value used in the main analysis):

- `k` (`build_keep_index()`, `exposure.R`): outlier cutoff multiplier for
  excluding bootstrap iterations with biologically implausible predicted
  densities. Default 1000.
- `anchor` (`rescale_overlap()`, `exposure.R`): central quantile span used to
  anchor the [0.5, 2.0] rescaling. Default 0.99.

Both are varied in the sensitivity analyses reported in the supplementary
appendices; see `scripts/06_sensitivity_analysis.R`.