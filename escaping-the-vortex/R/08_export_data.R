# =============================================================================
#  Export the derived, pseudonymised replication data as CSV files.
#  Run after run_all.R has finished (it reads the cached results in data/derived/).
#  Writes to data/replication/. No usernames, texts or titles are exported.
# =============================================================================

export_replication_data <- function(tag = if (exists("SIMULATE") && SIMULATE) "sim" else "real",
                                    out = "data/replication") {
  rd <- function(x) readRDS(file.path("data/derived", sprintf("%s_%s.rds", x, tag)))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  w <- function(df, f) {
    readr::write_csv(df, file.path(out, f), na = "")
    message("  wrote ", f, " (", nrow(df), " rows)")
  }

  # 1. Platform-month panel (H1, H2)
  w(rd("panel"), "panel_platform_month.csv")

  # 2. Deliberation-level coalitions (H3): landmark (14 days) and whole-life
  keep <- c("platform", "number", "is_pr", "created", "closed", "month", "size", "parsimony",
            "necessity", "bypass", "variety", "variety_missing", "variety0",
            "known_org_share", "resolved", "duration")
  lm <- rd("coalitions_lm14"); wl <- rd("coalitions")
  w(dplyr::select(lm, dplyr::any_of(keep)), "coalitions_landmark14.csv")
  w(dplyr::select(wl, dplyr::any_of(keep)), "coalitions_wholelife.csv")

  # 3. Fork contributors (H4), with usernames replaced by random IDs
  h4 <- rd("h4")$data
  set.seed(params$seed)
  ids <- unique(h4$author)
  key <- setNames(sprintf("C%05d", sample(seq_along(ids))), ids)
  h4$contributor_id <- unname(key[h4$author])
  h4$author <- NULL
  w(dplyr::relocate(h4, contributor_id, .after = fork), "fork_contributors.csv")

  # 4. Topic labels
  tp <- rd("topics")
  if (!is.null(tp$labels)) w(tp$labels, "topic_labels.csv")

  message("Replication data written to ", normalizePath(out))
  invisible(out)
}
