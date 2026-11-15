# =============================================================================
#  Escaping the Vortex? Platform socio-technical systems - full pipeline
#
#  Open PlatformSTS.Rproj in RStudio, then run this file (Ctrl/Cmd + Shift + S).
#  First time: set SIMULATE <- TRUE in R/00_config.R to test everything offline.
#  Real run:   set SIMULATE <- FALSE and put GITHUB_PAT in your .Renviron.
#  Every step saves its result in data/derived/, so a re-run skips finished work.
#  Delete a file in data/derived/ to force that step to run again.
# =============================================================================

source("R/00_setup.R")
source("R/00_config.R")
for (f in c("R/01_collect.R", "R/01b_simulate.R", "R/02_clean.R", "R/03_measures.R",
            "R/04_models.R", "R/05_outputs.R", "R/07_robustness.R")) source(f)

tag <- if (SIMULATE) "sim" else "real"
cached <- function(name, expr) {
  f <- file.path(paths$derived, paste0(name, "_", tag, ".rds"))
  if (file.exists(f)) { message("[cached] ", name); return(readRDS(f)) }
  message("[running] ", name); t0 <- Sys.time()
  out <- force(expr); saveRDS(out, f)
  message(sprintf("[done] %s in %.1f min", name, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out
}

# 1. Data -----------------------------------------------------------------------
if (!SIMULATE) run_collection()          # resumable; hours on first run (see README)
clean <- cached("clean", if (SIMULATE) simulate_data() else build_clean_data())
message(sprintf("Data: %s threads, %s contributions, %s contributors",
                format(nrow(clean$threads), big.mark = ","), format(nrow(clean$events), big.mark = ","),
                format(dplyr::n_distinct(clean$events$author), big.mark = ",")))

# 2. Measures -------------------------------------------------------------------
topics <- cached("topics", fit_topics(clean$threads))
coal   <- cached("coalitions", coalition_table(clean$threads, clean$events, clean$aff))
panel  <- cached("panel", build_panel(clean$threads, clean$events, coal, topics))

# 3. Hypothesis tests -----------------------------------------------------------
pd <- prep_did_panel(panel)
h1 <- cached("h1", test_h1(pd))
h2 <- cached("h2", test_h2(pd, "rop"))           # all latent ties (as in the manuscript)
h2b <- cached("h2b", test_h2(pd, "rop_bridge"))  # bridging latent ties only
coal_lm <- cached("coalitions_lm14", coalition_table_landmark(clean$threads, clean$events, clean$aff, days = 14))
h3 <- cached("h3_lm14", test_h3(coal_lm))         # main H3: coalitions in first 14 days
h3_full <- cached("h3", test_h3(coal))           # supplementary: whole-life coalitions
h4 <- cached("h4", test_h4(clean$events, clean$meta))

# 4. Outputs --------------------------------------------------------------------
make_outputs(panel, coal, topics, h1, h2, h2b, h3, h4, n_events = nrow(clean$events), h3_full = h3_full)

# 5. Robustness checks (about 10-20 minutes) -------------------------------------
rb <- cached("robustness", run_robustness(clean, coal, topics, panel))
write_robustness_outputs(rb)
print(rb$table)

print(h1$twfe); print(h2$main); print(h2b$main); print(h3$table); print(h3_full$table); print(h4$table)
message("\nFinished. See output/tables (Results_tables.docx, results_sentences.md), ",
        "output/figures.")

# 6. Export pseudonymised replication data (CSV) to data/replication/ ---------------
source("R/08_export_data.R")
export_replication_data()
