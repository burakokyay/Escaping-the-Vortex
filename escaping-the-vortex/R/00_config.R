# =============================================================================
# 00_config.R  -  every setting you might want to change lives here
# =============================================================================

# TRUE  = run the whole pipeline on simulated data (no internet needed; ~5 min).
#         Use this first to check that everything installs and runs.
# FALSE = use the real GitHub data collected by 01_collect.R.
SIMULATE <- TRUE

# ---- Sample ------------------------------------------------------------------
# role: treated = licence change; fork = community fork of a treated platform;
#       control = comparable platform with no licence change in the window.
# shock: public announcement date of the licence change.
#   >>> VERIFY these three dates against the original announcements <<<
# window_start / window_end: the period of data to download for each repo.
sample_frame <- tibble::tribble(
  ~platform,       ~owner,               ~repo,           ~role,     ~shock,       ~parent,         ~window_start, ~window_end,
  "elasticsearch", "elastic",            "elasticsearch", "treated", "2021-01-14", NA,              "2019-07-01",  "2022-07-31",
  "terraform",     "hashicorp",          "terraform",     "treated", "2023-08-10", NA,              "2022-02-01",  "2025-02-28",
  "redis",         "redis",              "redis",         "treated", "2024-03-20", NA,              "2022-09-01",  "2025-03-31",
  "opensearch",    "opensearch-project", "OpenSearch",    "fork",    NA,           "elasticsearch", "2021-01-01",  "2023-06-30",
  "opentofu",      "opentofu",           "opentofu",      "fork",    NA,           "terraform",     "2023-08-01",  "2025-08-31",
  "valkey",        "valkey-io",          "valkey",        "fork",    NA,           "redis",         "2024-03-01",  "2026-03-31",
  "prometheus",    "prometheus",         "prometheus",    "control", NA,           NA,              "2019-07-01",  "2025-03-31",
  "etcd",          "etcd-io",            "etcd",          "control", NA,           NA,              "2019-07-01",  "2025-03-31",
  "envoy",         "envoyproxy",         "envoy",         "control", NA,           NA,              "2019-07-01",  "2025-03-31",
  "containerd",    "containerd",         "containerd",    "control", NA,           NA,              "2019-07-01",  "2025-03-31"
) |>
  dplyr::mutate(dplyr::across(c(shock, window_start, window_end), as.Date))
# Note: Redis re-added an open-source (AGPL) licence option in 2025. The Redis
# window above ends before that, so the second change does not contaminate H1.

# ---- Analysis parameters -----------------------------------------------------
params <- list(
  event_window      = 12,     # months before/after the shock in the event study
  stalemate_days    = 90,     # a deliberation is "stalled" if unresolved after this
  aa_threshold      = 1,      # Adamic-Adar threshold for a latent tie (robustness: 0.5, 2)
  max_nodes_network = 3000,   # cap on contributors per monthly network (largest by activity)
  min_edges_network = 10,     # months with fewer co-deliberation ties get NA network measures
  K_topics          = 30,     # topics in the structural topic model
  stm_max_docs      = 40000,  # fit STM on at most this many threads; the rest are projected
  pre_rop_months    = 12,     # months before the shock used for pre-shock RoP
  fork_early_months = 6,      # "early contributors" of a fork = active in its first N months
  fork_follow_months = 18,    # months after the early period over which retention is measured
  boot_reps         = 999,    # wild cluster bootstrap replications (H2)
  seed              = 2026,
  profile_lookups   = 300     # per repo: look up company field for the N most active users
                              # with no affiliation from commit e-mails (1 API call each)
)

# Closure ("dogmatism") dictionary: expressions that end a topic without engaging it.
closure_terms <- c(
  "won't fix", "wont fix", "wontfix", "will not fix", "by design", "working as intended",
  "works as intended", "out of scope", "not going to", "we will not", "we won't",
  "not planned", "as decided", "already decided", "final decision", "no plans to",
  "not something we", "closing this", "locking this", "this is not a bug",
  "not interested in", "policy is", "non-negotiable"
)

# Accounts treated as bots (regex on login, case-insensitive) in addition to [bot] suffix.
bot_regex <- "\\[bot\\]$|-bot$|^bot-|^k8s-ci|^codecov|^dependabot|^renovate|^mergify|^github-actions|^elasticmachine|^elasticsearchmachine|^hashicorp-cla|^hashicorp-tsccr|^stale|^netlify|^linux-foundation|^CLAassistant|^cla-bot|^google-cla|^envoy-repokitteh|^repokitteh|^prombot|^opensearch-trigger-bot|^opensearch-ci-bot"

# Paths
paths <- list(raw = "data/raw", derived = "data/derived",
              tables = "output/tables", figures = "output/figures")
invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
