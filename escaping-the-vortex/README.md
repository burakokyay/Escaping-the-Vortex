# Escaping the Vortex? Forking, Redundancy of Potentialities and Deliberation in Platform Socio-Technical Systems

Replication code and data for the article of the same title by Burak Okyay (Mudanya University) and Mehlika Sarac (Bursa Uludag University), submitted to *Systems Research and Behavioral Science*.

The pipeline downloads the public GitHub deliberation records (issues, pull requests, comments, reviews and commits) of ten open-source infrastructure platforms. It then builds the measures and estimates the four hypotheses:

| | Platforms |
|---|---|
| Treated (licence change) | Elasticsearch (14 Jan 2021), Terraform (10 Aug 2023), Redis (20 Mar 2024) |
| Community forks | OpenSearch, OpenTofu, Valkey |
| Controls | Prometheus, etcd, Envoy, containerd |

## Requirements
- R 4.3 or later (the article used R 4.6) and RStudio.
- Packages are installed on first run by `R/00_setup.R`: httr2, quanteda, stm, igraph, fixest, did, survival, glmmTMB, among others.
- A free GitHub personal access token. Public data needs no scopes.

## How to reproduce

1. Open `escaping-the-vortex.Rproj` in RStudio.
2. **Test run (offline, about 7 minutes).** `R/00_config.R` ships with `SIMULATE <- TRUE`. Source `run_all.R`. This runs every step on synthetic data.
3. **Token.** Run `usethis::edit_r_environ()`, add `GITHUB_PAT=<your token>`, save, and restart R. The `.Renviron` file is git-ignored. Never commit it.
4. **Real run.** Set `SIMULATE <- FALSE` in `R/00_config.R` and source `run_all.R`.
   - Data collection is resumable and respects the API rate limit (5,000 requests per hour). The full collection takes many hours.
   - If it stops, source `run_all.R` again. It continues from the last saved page.
   - Each analysis step is cached in `data/derived/`. Delete a cached file to re-run that step.

Because the data are retrieved live from the GitHub API, re-collection at a later date can differ slightly (deleted accounts, edited or removed comments). The article's data were collected in November 2026: 115,844 threads, 682,299 contributions and 24,490 contributors.

## Outputs
| File | Content |
|---|---|
| `output/tables/Results_tables.docx` | Tables 3–7 and S1 |
| `output/tables/Robustness_tables.docx` | Tables S2–S3 |
| `output/tables/*.csv` | The same results as CSV, plus topic labels |
| `output/figures/Figure2_event_study.png`, `Figure3_fork_vitality.png` | Figures 2 and 3 |
| `output/tables/results_sentences.md`, `robustness_sentences.md` | Results in prose |

## Files
| File | Purpose |
|---|---|
| `R/00_config.R` | Sample, date windows and all parameters |
| `R/00_setup.R` | Package installation and token lookup |
| `R/01_collect.R` | Resumable GitHub API collection |
| `R/01b_simulate.R` | Synthetic data for the test run |
| `R/02_clean.R` | Bot removal, windows, threads/contributions/affiliations tables |
| `R/03_measures.R` | Structural topic model, coalition measures, monthly networks, redundancy of potentialities, vortex index |
| `R/04_models.R` | H1–H2 (staggered DiD, Callaway–Sant'Anna, two-way FE with wild cluster bootstrap), H3 (Cox, landmark design), H4 (beta-binomial) |
| `R/05_outputs.R` | Tables and figures |
| `R/07_robustness.R` | Leave-one-out, placebo shocks, alternative thresholds and landmarks |
| `R/08_export_data.R` | Exports the pseudonymised replication data to `data/replication/` |
| `run_all.R` | Runs everything in order |

## Data
- **Included:** `data/replication/` holds the derived, pseudonymised data behind every table and figure (see its README). The models can be re-estimated from these files without downloading anything.
- **Not included:** raw and cached data (`data/raw/`, `data/derived/`), because they contain contributor usernames and comment texts. `run_all.R` re-creates them from the public GitHub API.

## Licence
Code: MIT (see `LICENSE`).
