# scripts/

Analysis pipeline for the seabird OWED prioritization framework. Scripts are
numbered in run order; each sources its required function definitions from
`R/` (see `R/README.md`) and reads/writes the paths listed in its own header.

| Script                        | Produces |
|--------------------------------|----------|
| `01_exposure.R`                | Exposure: annual bootstrap distributions, proportional WEA overlap, rescaled exposure (`output/exposure_values/`) |
| `02_sensitivity.R`             | Sensitivity: collision/displacement scores combined and rescaled (`output/sensitivity_values/`) |
| `03_status.R`                  | Status: IUCN Red List status rescaled to [0.5, 2.0] (`output/status_values/`) |
| `04_multiply.R`                | Priority/vulnerability scores for all five weighting schemes (`output/priority_values/`) |
| `05_plot.R`                    | Main-manuscript outputs: results tables (Table 1, Appendix K), priority boxplot and vulnerability scatter (Figure 4), rank ridge plot (Figure 5); also generates the Monte Carlo rank distributions used by `06` |
| `06_sensitivity_analysis.R`    | Weighting-scheme sensitivity (Appendix J): Spearman concordance, median rank by scheme, per-scheme ridge plots |
| `07_appendix_D.R`              | Bootstrap iterations retained per species, by season and annual (Appendix D) |

## Running the pipeline

Scripts must be run in order — each depends on outputs from earlier scripts
(all read from `output/`, tracked in this repo; see the top-level
`.gitignore`). `01_exposure.R` additionally requires the raw Leirness et al.
(2021) bootstrap rasters (~36 GB, not redistributed with this repository;
see that script's header) and writes intermediate annual rasters to an
external drive path that must be updated to a local path before rerunning.

`05_plot.R` and `06_sensitivity_analysis.R` both read from
`output/rank_mc/`; `05_plot.R` must be run first (or that folder must
already be populated) before running `06`.

## Outputs

Final manuscript figures, tables, and supplementary appendix materials are
written to `paper/`, organized by appendix (`paper/appendix_D/`,
`paper/appendix_J/`, `paper/appendix_K/`). Intermediate analysis objects
(rescaled factors, priority scores, rank distributions) are written to
`output/`, organized by pipeline stage.