# =============================================================================
# 04_models.R  -  hypothesis tests H1-H4
# =============================================================================

# ---- Wild cluster bootstrap (restricted, Webb weights) -----------------------
# With only ~7 platforms, cluster-robust standard errors are unreliable; this
# gives a p-value for one coefficient that holds up with few clusters.
wild_boot_p <- function(data, y, rhs_full, rhs_restricted, param, fe = "id + t",
                        cluster = "id", B = params$boot_reps, seed = params$seed) {
  vars <- unique(c(y, all.vars(stats::as.formula(paste("~", rhs_full))), cluster, "t"))
  d <- stats::na.omit(data[, vars])
  f_full <- stats::as.formula(paste(y, "~", rhs_full, "|", fe))
  f_res  <- stats::as.formula(paste(y, "~", rhs_restricted, "|", fe))
  m_full <- fixest::feols(f_full, d, cluster = stats::as.formula(paste("~", cluster)))
  m_res  <- fixest::feols(f_res, d)
  if (!param %in% names(stats::coef(m_full))) return(NA_real_)
  t_obs <- fixest::tstat(m_full)[[param]]
  fit_r <- stats::fitted(m_res); e_r <- stats::resid(m_res)
  cl <- as.integer(factor(d[[cluster]])); G <- max(cl)
  webb <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
  set.seed(seed)
  t_b <- replicate(B, {
    d$.yb <- fit_r + sample(webb, G, TRUE)[cl] * e_r
    m <- fixest::feols(stats::as.formula(paste(".yb ~", rhs_full, "|", fe)), d,
                       cluster = stats::as.formula(paste("~", cluster)), notes = FALSE, warn = FALSE)
    tryCatch(fixest::tstat(m)[[param]], error = function(e) NA_real_)
  })
  mean(abs(t_b) >= abs(t_obs), na.rm = TRUE)
}

prep_did_panel <- function(panel) {
  months <- sort(unique(panel$month))
  panel |>
    dplyr::filter(role %in% c("treated", "control")) |>
    dplyr::mutate(id = as.integer(factor(platform)),
                  t = match(month, months),
                  shock_month = lubridate::floor_date(shock, "month"),
                  g = ifelse(is.na(shock), 0L, match(shock_month, months)),
                  post = as.integer(g > 0 & t >= g),
                  rel = ifelse(g > 0, t - g, NA_integer_)) |>
    dplyr::filter(is.na(rel) | abs(rel) <= params$event_window)
}

# ---- H1: licence shock -> vortical drift -------------------------------------
test_h1 <- function(pd) {
  outcomes <- c("vortex", "z_stalemate", "z_polarisation", "z_closure", "z_monotheme")
  cs <- tryCatch(did::att_gt(yname = "vortex", tname = "t", idname = "id", gname = "g",
                             data = as.data.frame(dplyr::filter(pd, !is.na(vortex))),
                             control_group = "notyettreated", allow_unbalanced_panel = TRUE,
                             base_period = "universal", bstrap = TRUE, cband = FALSE),
                 error = function(e) { warning("did::att_gt failed: ", conditionMessage(e)); NULL })
  es <- if (!is.null(cs)) tryCatch(did::aggte(cs, type = "dynamic", min_e = -params$event_window,
                                              max_e = params$event_window, na.rm = TRUE), error = function(e) NULL)
  simple <- if (!is.null(cs)) tryCatch(did::aggte(cs, type = "simple", na.rm = TRUE), error = function(e) NULL)
  sa <- tryCatch(fixest::feols(vortex ~ sunab(g_sa, t, ref.p = -1) | id + t,
                               data = dplyr::mutate(pd, g_sa = ifelse(g == 0, 10000L, g)), cluster = ~ id),
                 error = function(e) NULL)
  twfe <- purrr::map_dfr(outcomes, function(y) {
    m <- fixest::feols(stats::as.formula(paste(y, "~ post | id + t")), pd, cluster = ~ id)
    tibble::tibble(outcome = y, estimate = stats::coef(m)[["post"]], se = fixest::se(m)[["post"]],
                   p_cluster = fixest::pvalue(m)[["post"]],
                   p_wild = wild_boot_p(pd, y, "post", "1", "post"), n = stats::nobs(m))
  })
  list(cs = cs, es = es, simple = simple, sunab = sa, twfe = twfe)
}

# ---- H2: pre-shock redundancy of potentialities moderates drift ---------------
test_h2 <- function(pd, rop_var = "rop") {
  pd$rop <- pd[[rop_var]]
  pre <- pd |>
    dplyr::filter(g > 0, rel < 0, rel >= -params$pre_rop_months) |>
    dplyr::group_by(id) |> dplyr::summarise(rop_pre = mean(rop, na.rm = TRUE), .groups = "drop")
  d <- pd |>
    dplyr::left_join(pre, by = "id") |>
    dplyr::mutate(rop_pre_z = (rop_pre - mean(pre$rop_pre, na.rm = TRUE)) / stats::sd(pre$rop_pre, na.rm = TRUE),
                  rop_pre_z = ifelse(is.na(rop_pre_z), 0, rop_pre_z),     # controls: never post, term drops out
                  post_x_rop = post * rop_pre_z) |>
    dplyr::group_by(id) |> dplyr::arrange(t, .by_group = TRUE) |>
    dplyr::mutate(rop_lag_z = zscore(dplyr::lag(rop)), post_x_roplag = post * rop_lag_z) |>
    dplyr::ungroup()
  outcomes <- c("vortex", "z_stalemate", "z_polarisation", "z_closure", "z_monotheme")
  main <- purrr::map_dfr(outcomes, function(y) {
    m <- fixest::feols(stats::as.formula(paste(y, "~ post + post_x_rop | id + t")), d, cluster = ~ id)
    cf <- function(v, k) { x <- v[k]; if (is.na(names(x))) NA_real_ else unname(x) }
    tibble::tibble(outcome = y,
                   b_post = cf(stats::coef(m), "post"), se_post = cf(fixest::se(m), "post"),
                   b_int = cf(stats::coef(m), "post_x_rop"), se_int = cf(fixest::se(m), "post_x_rop"),
                   p_int_wild = wild_boot_p(d, y, "post + post_x_rop", "post", "post_x_rop"),
                   n = stats::nobs(m))
  })
  # Supplementary: time-varying (lagged) RoP within platforms
  lagged <- tryCatch(fixest::feols(vortex ~ post + rop_lag_z + post_x_roplag | id + t, d, cluster = ~ id),
                     error = function(e) NULL)
  list(rop_var = rop_var, data = d, rop_pre = pre, main = main, lagged = lagged,
       p_lagged_wild = wild_boot_p(d, "vortex", "post + rop_lag_z + post_x_roplag",
                                   "post + rop_lag_z", "post_x_roplag"))
}

# ---- H3: coalition fit -> faster resolution -----------------------------------
test_h3 <- function(coal) {
  d <- coal |> dplyr::filter(size >= 2) |>
    dplyr::mutate(log_size = log1p(size), is_pr = as.integer(is_pr), platform = factor(platform))
  f <- survival::Surv(duration, resolved) ~ parsimony + necessity + bypass + variety0 +
    variety_missing + log_size + is_pr
  m <- if (nrow(d) <= 250000) survival::coxph(update(f, . ~ . + survival::frailty(platform)), data = d) else
    survival::coxph(update(f, . ~ . + strata(platform)), data = d)
  s <- summary(m)$coefficients
  ci <- summary(m)$conf.int
  rows <- intersect(rownames(ci), c("parsimony", "necessity", "bypass", "variety0", "variety_missing", "log_size", "is_pr"))
  list(model = m, n = m$n, events = m$nevent,
       table = tibble::tibble(term = rows, hr = ci[rows, "exp(coef)"], lo = ci[rows, "lower .95"],
                              hi = ci[rows, "upper .95"], p = s[rows, ncol(s)]))
}

# ---- H4: fork origins -> contributor retention --------------------------------
period_network <- function(ev) if (nrow(ev)) month_network(ev) else igraph::make_empty_graph(directed = FALSE)

test_h4 <- function(events, meta, frame = sample_frame) {
  forks <- frame |> dplyr::filter(role == "fork")
  rows <- purrr::map_dfr(seq_len(nrow(forks)), function(i) {
    f <- forks[i, ]; par <- frame |> dplyr::filter(platform == f$parent)
    ev_f <- events |> dplyr::filter(platform == f$platform)
    if (!nrow(ev_f)) return(NULL)
    start <- lubridate::floor_date(min(as.Date(ev_f$time)), "month")
    early_end <- start %m+% months(params$fork_early_months)
    follow_end <- early_end %m+% months(params$fork_follow_months)
    early <- unique(ev_f$author[as.Date(ev_f$time) < early_end])
    ev_p <- events |> dplyr::filter(platform == f$parent)
    s <- par$shock
    pre <- ev_p |> dplyr::filter(as.Date(time) < s, as.Date(time) >= s %m-% months(params$pre_rop_months))
    post <- ev_p |> dplyr::filter(as.Date(time) >= s, as.Date(time) < max(s %m+% months(6), early_end))
    # latent ties among early fork contributors in the parent's pre-shock network
    g_pre <- period_network(pre)
    latent_holder <- character()
    if (igraph::vcount(g_pre) > 2) {
      aa <- igraph::similarity(g_pre, method = "invlogweighted")
      adj <- as.matrix(igraph::as_adjacency_matrix(g_pre, sparse = TRUE)) > 0
      nm <- igraph::V(g_pre)$name; dimnames(aa) <- dimnames(adj) <- list(nm, nm)
      e_in <- intersect(early, nm)
      if (length(e_in) > 1) {
        L <- aa[e_in, e_in, drop = FALSE] >= params$aa_threshold & !adj[e_in, e_in, drop = FALSE]
        diag(L) <- FALSE
        latent_holder <- e_in[rowSums(L) > 0]
      }
    }
    # polarised camp: communities in the parent's post-shock network other than the maintainers' camp
    g_post <- period_network(post)
    polar <- character()
    if (igraph::ecount(g_post) > 0) {
      set.seed(params$seed)
      cl <- igraph::cluster_louvain(g_post, weights = igraph::E(g_post)$weight)
      memb <- igraph::membership(cl)
      maint <- unique(post$author[post$author_assoc %in% MAINT])
      inc <- as.integer(names(which.max(table(memb[names(memb) %in% maint]))))
      if (length(inc)) polar <- names(memb)[memb != inc]
    }
    in_parent <- unique(c(pre$author, post$author))
    act <- ev_f |>
      dplyr::filter(as.Date(time) >= early_end, as.Date(time) < follow_end) |>
      dplyr::mutate(m = lubridate::floor_date(as.Date(time), "month")) |>
      dplyr::distinct(author, m) |> dplyr::count(author, name = "active_months")
    early_act <- ev_f |> dplyr::filter(as.Date(time) < early_end) |> dplyr::count(author, name = "early_events")
    tibble::tibble(fork = f$platform, author = early) |>
      dplyr::left_join(act, by = "author") |> dplyr::left_join(early_act, by = "author") |>
      dplyr::mutate(active_months = dplyr::coalesce(active_months, 0L),
                    follow = params$fork_follow_months,
                    from_parent = as.integer(author %in% in_parent),
                    polar_origin = as.integer(author %in% polar),
                    latent_origin = as.integer(author %in% latent_holder),
                    log_early = log1p(early_events))
  })
  m <- glmmTMB::glmmTMB(cbind(active_months, follow - active_months) ~ polar_origin + latent_origin +
                          from_parent + log_early + (1 | fork),
                        family = glmmTMB::betabinomial(), data = rows)
  cf <- summary(m)$coefficients$cond
  vit <- events |> dplyr::filter(platform %in% forks$platform) |>
    dplyr::mutate(month = lubridate::floor_date(as.Date(time), "month")) |>
    dplyr::group_by(platform, month) |> dplyr::summarise(active = dplyr::n_distinct(author), .groups = "drop")
  list(data = rows, model = m, vitality = vit,
       table = tibble::tibble(term = rownames(cf), or = exp(cf[, 1]),
                              lo = exp(cf[, 1] - 1.96 * cf[, 2]), hi = exp(cf[, 1] + 1.96 * cf[, 2]),
                              p = cf[, 4]))
}
