# =============================================================================
# 07_robustness.R  -  robustness checks (Section 3.6 of the manuscript)
#   1. Leave one treated platform out (H1)
#   2. Placebo shocks: real shock dates assigned to control platforms (H1)
#   3. Alternative stalemate thresholds: 60 and 180 days (H1)
#   4. Alternative latent-tie thresholds: 0.5 and 2 (H2, both RoP indices)
#   5. Alternative landmark days for coalitions: 7 and 30 (H3)
# Writes output/tables/robustness.csv, Robustness_tables.docx and adds
# sentences to output/tables/robustness_sentences.md
# =============================================================================

h1_estimate <- function(pd, y = "vortex", B = 499) {
  m <- fixest::feols(stats::as.formula(paste(y, "~ post | id + t")), pd, cluster = ~ id)
  tibble::tibble(estimate = stats::coef(m)[["post"]], se = fixest::se(m)[["post"]],
                 p_wild = wild_boot_p(pd, y, "post", "1", "post", B = B), n = stats::nobs(m))
}

# rebuild the panel with temporarily changed parameters
panel_with <- function(changes, clean, coal, topics) {
  old <- params
  on.exit(params <<- old, add = TRUE)
  for (k in names(changes)) params[[k]] <<- changes[[k]]
  build_panel(clean$threads, clean$events, coal, topics)
}

run_robustness <- function(clean, coal, topics, panel, frame = sample_frame, B = 499) {
  rows <- list()
  add <- function(check, spec, est) rows[[length(rows) + 1]] <<- tibble::tibble(check = check, specification = spec, est)
  pd <- prep_did_panel(panel)

  # 0. baseline
  add("Baseline", "All platforms, as in Table 4", h1_estimate(pd, B = B))

  # 1. leave one treated platform out
  for (p in frame$platform[frame$role == "treated"]) {
    message("  leave-one-out: ", p)
    add("Leave one treated platform out", paste("Without", p), h1_estimate(dplyr::filter(pd, platform != p), B = B))
  }

  # 2. placebo shocks on control platforms (treated platforms dropped)
  ctrl <- frame$platform[frame$role == "control"]
  shocks <- frame$shock[frame$role == "treated"]
  placebo <- purrr::map_dfr(ctrl, function(c) purrr::map_dfr(shocks, function(s) {
    pp <- panel |>
      dplyr::filter(role == "control") |>
      dplyr::mutate(role = ifelse(platform == c, "treated", "control"),
                    shock = as.Date(ifelse(platform == c, as.character(s), NA)))
    message("  placebo: ", c, " at ", s)
    h1_estimate(prep_did_panel(pp), B = B) |> dplyr::mutate(platform = c, date = s)
  }))
  readr::write_csv(placebo, file.path(paths$tables, "robustness_placebo_detail.csv"))
  base_est <- rows[[1]]$estimate
  add("Placebo shocks on controls",
      sprintf("%d placebo estimates: mean %.2f, range %.2f to %.2f; %d of %d significant at 5%%",
              nrow(placebo), mean(placebo$estimate), min(placebo$estimate), max(placebo$estimate),
              sum(placebo$p_wild < .05, na.rm = TRUE), nrow(placebo)),
      tibble::tibble(estimate = mean(placebo$estimate), se = stats::sd(placebo$estimate),
                     p_wild = mean(abs(placebo$estimate) >= abs(base_est)), n = NA_integer_))

  # 3. stalemate thresholds
  for (d in c(60, 180)) {
    message("  stalemate threshold: ", d)
    pd_d <- prep_did_panel(panel_with(list(stalemate_days = d), clean, coal, topics))
    add("Stalemate threshold", paste(d, "days (vortex index)"), h1_estimate(pd_d, B = B))
  }

  # 4. latent-tie thresholds (H2 interaction)
  for (a in c(0.5, 2)) {
    message("  latent-tie threshold: ", a)
    pd_a <- prep_did_panel(panel_with(list(aa_threshold = a), clean, coal, topics))
    for (rv in c("rop", "rop_bridge")) {
      h <- test_h2(pd_a, rv)$main |> dplyr::filter(outcome == "vortex")
      add("Latent-tie threshold (H2)", sprintf("Adamic-Adar >= %s, %s", a, ifelse(rv == "rop", "all latent ties", "bridging ties")),
          tibble::tibble(estimate = h$b_int, se = h$se_int, p_wild = h$p_int_wild, n = h$n))
    }
  }

  # 5. landmark days (H3)
  h3_alt <- purrr::map_dfr(c(7, 30), function(d) {
    message("  landmark: ", d, " days")
    cl <- coalition_table_landmark(clean$threads, clean$events, clean$aff, days = d, frame = frame)
    test_h3(cl)$table |> dplyr::mutate(landmark_days = d)
  })
  readr::write_csv(h3_alt, file.path(paths$tables, "robustness_h3_landmarks.csv"))

  tab <- dplyr::bind_rows(rows)
  readr::write_csv(tab, file.path(paths$tables, "robustness.csv"))
  list(table = tab, placebo = placebo, h3_alt = h3_alt)
}

write_robustness_outputs <- function(rb) {
  t1 <- rb$table |> dplyr::transmute(Check = check, Specification = specification,
                                    Estimate = fmt(estimate), SE = fmt(se),
                                    p = fmtp(p_wild), N = ifelse(is.na(n), "–", as.character(n)))
  t2 <- rb$h3_alt |> dplyr::filter(term %in% c("parsimony", "necessity", "bypass", "variety0", "log_size")) |>
    dplyr::transmute(Landmark = paste(landmark_days, "days"), Term = pretty_term[term],
                     `Hazard ratio` = paste0(fmt(hr), stars(p)),
                     `95% CI` = paste0("[", fmt(lo), ", ", fmt(hi), "]"), p = fmtp(p))
  ft <- function(df, caption, note) flextable::flextable(df) |> flextable::theme_booktabs() |>
    flextable::add_footer_lines(note) |> flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 10, part = "all") |> flextable::set_caption(caption) |>
    flextable::set_table_properties(layout = "autofit", width = 1)
  doc <- officer::read_docx() |>
    flextable::body_add_flextable(ft(t1, "TABLE S2 Robustness of the H1 and H2 results",
      "Estimates are the post-shock effect on the vortex index (H1 rows) or the post × pre-shock RoP interaction (H2 rows). p-values from the wild cluster bootstrap (499 replications). For placebo shocks, the estimate is the mean placebo effect, SE its standard deviation, and p the share of placebo estimates at least as large in absolute value as the baseline estimate.")) |>
    officer::body_add_par("") |>
    flextable::body_add_flextable(ft(t2, "TABLE S3 H3 with alternative landmark days",
      "Cox models as in Table 6, with coalitions measured over the first 7 or 30 days. † p < .10, * p < .05, ** p < .01."))
  print(doc, target = file.path(paths$tables, "Robustness_tables.docx"))

  tb <- rb$table
  g <- function(chk) dplyr::filter(tb, check == chk)
  loo <- g("Leave one treated platform out"); st <- g("Stalemate threshold"); lt <- g("Latent-tie threshold (H2)")
  pl <- g("Placebo shocks on controls")
  txt <- c("# Robustness sentences (check against Robustness_tables.docx)", "",
    sprintf("Leaving out one treated platform at a time, the H1 estimate ranged from %s to %s (wild bootstrap p from %s to %s).",
            fmt(min(loo$estimate)), fmt(max(loo$estimate)), fmtp(min(loo$p_wild)), fmtp(max(loo$p_wild))), "",
    sprintf("Placebo shocks: %s", pl$specification), "",
    sprintf("With stalemate defined at 60 or 180 days, the H1 estimate was %s (p = %s) and %s (p = %s).",
            fmt(st$estimate[1]), fmtp(st$p_wild[1]), fmt(st$estimate[2]), fmtp(st$p_wild[2])), "",
    if (all(is.na(lt$estimate))) "Latent-tie thresholds: the H2 interaction could not be estimated." else
      sprintf("With latent-tie thresholds of 0.5 and 2, the H2 interactions ranged from %s to %s (smallest p = %s; %d of %d specifications estimable).",
              fmt(min(lt$estimate, na.rm = TRUE)), fmt(max(lt$estimate, na.rm = TRUE)), fmtp(min(lt$p_wild, na.rm = TRUE)),
              sum(!is.na(lt$estimate)), nrow(lt)), "",
    "H3 with 7- and 30-day landmarks: see Table S3.")
  writeLines(txt, file.path(paths$tables, "robustness_sentences.md"))
}
