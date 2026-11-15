# =============================================================================
# 05_outputs.R  -  tables (Word + CSV), figures (PNG, 300 dpi) and a text file
# of results sentences to paste into the manuscript
# =============================================================================

fmt <- function(x, d = 2) ifelse(is.na(x), "\u2013", formatC(x, format = "f", digits = d))
fmtp <- function(p) ifelse(is.na(p), "\u2013", ifelse(p < .001, "< .001", formatC(p, format = "f", digits = 3)))
peq <- function(p) ifelse(!is.na(p) & p < .001, "p < .001", paste("p =", fmtp(p)))
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "\u2020", ""))))
pretty_outcome <- c(vortex = "Vortex index", z_stalemate = "Stalemate", z_polarisation = "Polarisation",
                    z_closure = "Closure language", z_monotheme = "Monothematic (\u2212entropy)")
pretty_term <- c(parsimony = "Parsimony", necessity = "Necessity (maintainer present)",
                 bypass = "Hierarchy bypass", variety0 = "Variety (Blau index)",
                 variety_missing = "Variety unknown (indicator)", log_size = "log(coalition size)",
                 is_pr = "Pull request (ref. issue)", `(Intercept)` = "Intercept",
                 polar_origin = "Polarised-camp origin (H4a)", latent_origin = "Latent-tie origin (H4b)",
                 from_parent = "Active in parent platform", log_early = "log(early activity)")

make_outputs <- function(panel, coal, topics, h1, h2, h2b, h3, h4, n_events, h3_full = NULL, frame = sample_frame) {
  T <- list()

  # Table 3: descriptives
  pre_post <- panel |>
    dplyr::mutate(rel = ifelse(is.na(shock), NA, lubridate::interval(lubridate::floor_date(shock, "month"), month) %/% months(1))) |>
    dplyr::group_by(platform) |>
    dplyr::summarise(rop_pre = mean(rop[!is.na(rel) & rel < 0 & rel >= -params$pre_rop_months], na.rm = TRUE),
                     vortex_pre = mean(vortex[!is.na(rel) & rel < 0], na.rm = TRUE),
                     vortex_post = mean(vortex[!is.na(rel) & rel >= 0 & rel <= params$event_window], na.rm = TRUE),
                     rop_all = mean(rop, na.rm = TRUE), .groups = "drop")
  desc <- coal |> dplyr::group_by(platform) |>
    dplyr::summarise(threads = dplyr::n(), resolved = mean(resolved), .groups = "drop") |>
    dplyr::left_join(panel |> dplyr::group_by(platform) |>
                       dplyr::summarise(contributors_month = mean(n_contributors, na.rm = TRUE), .groups = "drop"), by = "platform") |>
    dplyr::left_join(pre_post, by = "platform") |>
    dplyr::left_join(frame |> dplyr::select(platform, role), by = "platform") |>
    dplyr::arrange(factor(role, c("treated", "fork", "control")), platform)
  T$table3 <- desc |> dplyr::transmute(Platform = platform, Role = role, Threads = format(threads, big.mark = ","),
                                       `Resolved (%)` = fmt(100 * resolved, 1),
                                       `Contributors / month` = fmt(contributors_month, 0),
                                       `RoP (pre-shock or mean)` = fmt(ifelse(role == "treated", rop_pre, rop_all), 3),
                                       `Vortex pre` = ifelse(role == "treated", fmt(vortex_pre), "\u2013"),
                                       `Vortex post` = ifelse(role == "treated", fmt(vortex_post), "\u2013"))

  # H1 table
  T$table_h1 <- h1$twfe |> dplyr::transmute(Outcome = pretty_outcome[outcome], `Post-shock effect` = paste0(fmt(estimate), stars(p_wild)),
                                           SE = fmt(se), `p (wild bootstrap)` = fmtp(p_wild), N = n)
  # H2 table
  T$table4 <- h2$main |> dplyr::transmute(Outcome = pretty_outcome[outcome],
                                         `Post-shock` = paste0(fmt(b_post), " (", fmt(se_post), ")"),
                                         post_x =  paste0(fmt(b_int), stars(p_int_wild), " (", fmt(se_int), ")"),
                                         `p (wild bootstrap)` = fmtp(p_int_wild), N = n)
  T$table4b <- h2b$main |> dplyr::transmute(Outcome = pretty_outcome[outcome],
                                         `Post-shock` = paste0(fmt(b_post), " (", fmt(se_post), ")"),
                                         post_xb =  paste0(fmt(b_int), stars(p_int_wild), " (", fmt(se_int), ")"),
                                         `p (wild bootstrap)` = fmtp(p_int_wild), N = n)
  names(T$table4)[names(T$table4) == "post_x"] <- "Post \u00d7 pre-shock RoP"
  names(T$table4b)[names(T$table4b) == "post_xb"] <- "Post \u00d7 pre-shock bridging RoP"
  # H3 table
  T$table5 <- h3$table |> dplyr::transmute(Term = pretty_term[term], `Hazard ratio` = paste0(fmt(hr), stars(p)),
                                          `95% CI` = paste0("[", fmt(lo), ", ", fmt(hi), "]"), p = fmtp(p))
  if (!is.null(h3_full)) T$table5_full <- h3_full$table |> dplyr::transmute(Term = pretty_term[term], `Hazard ratio` = paste0(fmt(hr), stars(p)),
                                          `95% CI` = paste0("[", fmt(lo), ", ", fmt(hi), "]"), p = fmtp(p))
  # H4 table
  T$table6 <- h4$table |> dplyr::transmute(Term = pretty_term[term], `Odds ratio` = paste0(fmt(or), stars(p)),
                                          `95% CI` = paste0("[", fmt(lo), ", ", fmt(hi), "]"), p = fmtp(p))
  T$topics <- topics$labels
  purrr::iwalk(T, ~ readr::write_csv(.x, file.path(paths$tables, paste0(.y, ".csv"))))

  # ---- Figure 2: event study ----
  if (!is.null(h1$es)) {
    es <- tibble::tibble(e = h1$es$egt, att = h1$es$att.egt, se = h1$es$se.egt) |>
      dplyr::mutate(lo = att - 1.96 * se, hi = att + 1.96 * se)
    p2 <- ggplot(es, aes(e, att)) +
      geom_hline(yintercept = 0, colour = "grey50") + geom_vline(xintercept = -0.5, linetype = 2, colour = "grey50") +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = "grey40") + geom_point(size = 1.8) +
      labs(x = "Months relative to licence change", y = "Effect on vortex index (SD)") +
      theme_classic(base_size = 11, base_family = "serif")
    ggsave(file.path(paths$figures, "Figure2_event_study.png"), p2, width = 6.5, height = 3.8, dpi = 300)
  }
  # ---- Figure 3: fork vitality ----
  p3 <- ggplot(h4$vitality, aes(month, active, linetype = platform)) + geom_line() +
    labs(x = NULL, y = "Active contributors per month", linetype = "Fork") +
    theme_classic(base_size = 11, base_family = "serif") + theme(legend.position = "bottom")
  ggsave(file.path(paths$figures, "Figure3_fork_vitality.png"), p3, width = 6.5, height = 3.5, dpi = 300)

  # ---- Word file with all tables ----
  ft <- function(df, caption, note) {
    flextable::flextable(df) |> flextable::theme_booktabs() |> flextable::add_footer_lines(note) |>
      flextable::font(fontname = "Times New Roman", part = "all") |> flextable::fontsize(size = 10, part = "all") |>
      flextable::set_caption(caption) |>
      flextable::set_table_properties(layout = "autofit", width = 1)
  }
  note_sig <- "\u2020 p < .10, * p < .05, ** p < .01 (wild cluster bootstrap, Webb weights)."
  doc <- officer::read_docx() |>
    flextable::body_add_flextable(ft(T$table3, "TABLE 3 Descriptive statistics by platform",
      "RoP = redundancy of potentialities (latent-tie share): pre-shock 12-month mean for treated platforms, whole-window mean otherwise. Vortex index in within-platform SD units.")) |>
    officer::body_add_par("") |>
    flextable::body_add_flextable(ft(T$table_h1, "TABLE H1 Effect of the licence change on vortical drift (two-way fixed effects)",
      paste("Platform and month fixed effects; standard errors clustered by platform.", note_sig))) |>
    officer::body_add_par("") |>
    flextable::body_add_flextable(ft(T$table4, "TABLE 4 Moderation of vortical drift by pre-shock redundancy of potentialities",
      paste("Coefficients with cluster-robust SE in parentheses. Pre-shock RoP standardised across treated platforms.", note_sig))) |>
    officer::body_add_par("") |>
    flextable::body_add_flextable(ft(T$table4b, "TABLE 4b Moderation by pre-shock bridging redundancy of potentialities",
      paste("As Table 4, counting only latent ties that would join different network communities.", note_sig))) |>
    officer::body_add_par("") |>
    flextable::body_add_flextable(ft(T$table5, "TABLE 5 Cox model of time to deliberation resolution (coalitions measured in the first 14 days)",
      sprintf("Landmark design: coalition composition measured from contributions in each deliberation's first 14 days; N = %s deliberations still open on day 14 with two or more participants by then; %s later resolved. Time counted from day 14. Hazard ratios above 1 = faster resolution. Platform frailty (or strata for very large samples). \u2020 p < .10, * p < .05, ** p < .01.",
              format(h3$n, big.mark = ","), format(h3$events, big.mark = ",")))) |>
    officer::body_add_par("") |>
    (\(d) if (is.null(T$table5_full)) d else flextable::body_add_flextable(d, ft(T$table5_full,
      "TABLE S1 Cox model with whole-life coalitions (for comparison)",
      "Coalition composition measured over each deliberation's whole life; susceptible to reverse causality because longer-open deliberations accumulate more participants.")))() |>
    officer::body_add_par("") |>
    flextable::body_add_flextable(ft(T$table6, "TABLE 6 Fork contributor retention by origin (beta-binomial mixed model)",
      sprintf("N = %d early fork contributors in %d forks; outcome = months active out of %d; random intercept for fork. \u2020 p < .10, * p < .05, ** p < .01.",
              nrow(h4$data), dplyr::n_distinct(h4$data$fork), params$fork_follow_months)))
  print(doc, target = file.path(paths$tables, "Results_tables.docx"))

  # ---- Sentences for the manuscript ----
  v <- h1$twfe |> dplyr::filter(outcome == "vortex")
  i2 <- h2$main |> dplyr::filter(outcome == "vortex")
  hr <- function(t) h3$table |> dplyr::filter(term == t)
  o4 <- function(t) h4$table |> dplyr::filter(term == t)
  simple_txt <- if (!is.null(h1$simple)) sprintf(" The Callaway\u2013Sant'Anna aggregate ATT is %s (SE %s).", fmt(h1$simple$overall.att), fmt(h1$simple$overall.se)) else ""
  txt <- c(
    "# Results sentences (check every number against the tables before pasting)", "",
    sprintf("Corpus: %s deliberations, %s contributions, %s platform-months.",
            format(nrow(coal), big.mark = ","), format(n_events, big.mark = ","), nrow(panel)), "",
    sprintf("H1. After the licence change, the vortex index of treated platforms rose by %s within-platform SD relative to controls (wild bootstrap %s).%s",
            fmt(v$estimate), peq(v$p_wild), simple_txt), "",
    sprintf("H2. The interaction of the post-shock period with pre-shock redundancy of potentialities is %s (wild bootstrap %s): a one-SD higher pre-shock RoP %s the post-shock rise in the vortex index by %s SD. With time-varying lagged RoP the interaction is %s (wild bootstrap %s).",
            fmt(i2$b_int), peq(i2$p_int_wild), ifelse(i2$b_int < 0, "reduced", "increased"), fmt(abs(i2$b_int)),
            fmt(if (is.null(h2$lagged)) NA else stats::coef(h2$lagged)["post_x_roplag"]), peq(h2$p_lagged_wild)), "",
    { b <- h2b$main |> dplyr::filter(outcome == "vortex")
      sprintf("H2 (bridging RoP). Counting only latent ties between communities, the interaction is %s (wild bootstrap %s).", fmt(b$b_int), peq(b$p_int_wild)) }, "",
    sprintf("H3 (coalitions in the first 14 days). Deliberations were resolved faster when their coalitions were more parsimonious (HR = %s, %s), included a maintainer (HR = %s, %s), had more hierarchy bypass (HR = %s, %s) and more organisational variety (HR = %s, %s). [Rephrase for any HR below 1.]",
            fmt(hr("parsimony")$hr), peq(hr("parsimony")$p), fmt(hr("necessity")$hr), peq(hr("necessity")$p),
            fmt(hr("bypass")$hr), peq(hr("bypass")$p), fmt(hr("variety0")$hr), peq(hr("variety0")$p)), "",
    sprintf("H4. Among %d early fork contributors, those from the parent's polarised camp had an odds ratio of %s for monthly retention (%s; H4a), and those holding pre-shock latent ties an odds ratio of %s (%s; H4b).",
            nrow(h4$data), fmt(o4("polar_origin")$or), peq(o4("polar_origin")$p),
            fmt(o4("latent_origin")$or), peq(o4("latent_origin")$p)))
  writeLines(txt, file.path(paths$tables, "results_sentences.md"))
  invisible(T)
}
