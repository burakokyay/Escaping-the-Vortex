# =============================================================================
# 03_measures.R  -  construct measures (Table 2 of the manuscript)
# =============================================================================

MAINT <- c("OWNER", "MEMBER", "COLLABORATOR")
blau <- function(x) { x <- x[!is.na(x)]; if (length(x) < 2) return(NA_real_); p <- prop.table(table(x)); 1 - sum(p^2) }
zscore <- function(x) { s <- sd(x, na.rm = TRUE); if (is.na(s) || s == 0) return(x * 0); (x - mean(x, na.rm = TRUE)) / s }

# ---- Deliberation topics (structural topic model) ----------------------------
clean_text <- function(x) {
  x <- gsub("```[\\s\\S]*?```", " ", x, perl = TRUE)        # code blocks
  x <- gsub("`[^`]*`", " ", x)                                 # inline code
  x <- gsub("https?://\\S+", " ", x)                           # links
  x <- gsub("<!--[\\s\\S]*?-->", " ", x, perl = TRUE)          # template comments
  x <- gsub("[#>*_|\\[\\]()]", " ", x)
  x
}

fit_topics <- function(threads, K = params$K_topics, max_docs = params$stm_max_docs, seed = params$seed) {
  set.seed(seed)
  txt <- clean_text(paste(threads$title, threads$body))
  corp <- quanteda::corpus(txt, docvars = data.frame(platform = threads$platform))
  toks <- quanteda::tokens(corp, remove_punct = TRUE, remove_numbers = TRUE,
                           remove_symbols = TRUE, remove_url = TRUE) |>
    quanteda::tokens_tolower() |>
    quanteda::tokens_remove(quanteda::stopwords("en")) |>
    quanteda::tokens_keep(min_nchar = 3)
  dfm <- quanteda::dfm(toks) |>
    quanteda::dfm_trim(min_docfreq = max(5, ceiling(0.0005 * nrow(threads))), docfreq_type = "count") |>
    quanteda::dfm_trim(max_docfreq = 0.5, docfreq_type = "prop")
  nonempty <- which(quanteda::ntoken(dfm) > 0)
  fit_idx <- if (length(nonempty) > max_docs) sample(nonempty, max_docs) else nonempty
  stm_in <- quanteda::convert(dfm[fit_idx, ], to = "stm")
  message("Fitting STM: ", length(fit_idx), " documents, ", length(stm_in$vocab), " terms, K = ", K)
  fit <- stm::stm(stm_in$documents, stm_in$vocab, K = K, prevalence = ~ platform,
                  data = stm_in$meta, init.type = "Spectral", max.em.its = 75, verbose = FALSE)
  # Assign every thread (incl. those not used for fitting) to its most probable topic
  logbeta <- fit$beta$logbeta[[1]]                       # K x V
  colnames(logbeta) <- fit$vocab
  dfm_all <- quanteda::dfm_match(dfm, fit$vocab)
  prior <- log(colMeans(fit$theta))
  scores <- as.matrix(dfm_all %*% Matrix::t(Matrix::Matrix(logbeta, sparse = FALSE)))
  scores <- sweep(scores, 2, prior, "+")
  topic <- max.col(scores, ties.method = "first")
  topic[quanteda::ntoken(dfm_all) == 0] <- NA
  labels <- stm::labelTopics(fit, n = 7)$frex
  list(fit = fit, topic = topic,
       labels = tibble::tibble(topic = seq_len(K), frex = apply(labels, 1, paste, collapse = ", ")))
}

# ---- Discretionary coalitions (one row per deliberation) ---------------------
coalition_table <- function(threads, events, aff, frame = sample_frame) {
  per_author <- events |>
    dplyr::group_by(platform, number, author) |>
    dplyr::summarise(turns = dplyr::n(),
                     assoc = names(sort(table(author_assoc), decreasing = TRUE))[1], .groups = "drop") |>
    dplyr::left_join(aff, by = c("platform", "author"))
  coal <- per_author |>
    dplyr::group_by(platform, number) |>
    dplyr::summarise(size = dplyr::n(),
                     parsimony = 1 - mean(turns == 1),
                     necessity = as.integer(any(assoc %in% MAINT)),
                     bypass = mean(!assoc %in% MAINT),
                     variety = blau(org),
                     known_org_share = mean(!is.na(org)),
                     .groups = "drop")
  threads |>
    dplyr::select(platform, number, is_pr, created, closed) |>
    dplyr::left_join(coal, by = c("platform", "number")) |>
    dplyr::left_join(frame |> dplyr::select(platform, window_end), by = "platform") |>
    dplyr::mutate(end = as.POSIXct(window_end + 1, tz = "UTC"),
                  resolved = as.integer(!is.na(closed) & closed <= end),
                  duration = as.numeric(difftime(dplyr::if_else(resolved == 1, closed, end), created, units = "days")),
                  duration = pmax(duration, 1 / 24),
                  variety_missing = as.integer(is.na(variety)),
                  variety0 = dplyr::coalesce(variety, 0),
                  month = lubridate::floor_date(as.Date(created), "month")) |>
    dplyr::select(-window_end, -end)
}

# ---- Landmark version for H3 ---------------------------------------------------
# Coalitions are measured from contributions in the first `days` days only, and
# only threads still open on that day are kept; time to resolution is counted
# from that day. This stops slow threads from looking "bigger" simply because
# they stayed open long enough to attract more people (reverse causality).
coalition_table_landmark <- function(threads, events, aff, days = 14, frame = sample_frame) {
  ev <- events |>
    dplyr::inner_join(threads |> dplyr::select(platform, number, t0 = created), by = c("platform", "number")) |>
    dplyr::filter(time <= t0 + days * 86400) |>
    dplyr::select(-t0)
  coalition_table(threads, ev, aff, frame) |>
    dplyr::filter(duration > days, !is.na(size)) |>
    dplyr::mutate(duration = duration - days)
}

# ---- Monthly co-deliberation network ------------------------------------------
month_network <- function(ev) {
  e <- dplyr::distinct(ev, author, number)
  act <- dplyr::count(e, author, sort = TRUE)
  if (nrow(act) > params$max_nodes_network) e <- dplyr::filter(e, author %in% act$author[1:params$max_nodes_network])
  e$number <- paste0("#", e$number)
  g2 <- igraph::graph_from_data_frame(e, directed = FALSE)
  igraph::V(g2)$type <- igraph::V(g2)$name %in% e$number
  igraph::bipartite_projection(g2, types = igraph::V(g2)$type, which = "false", multiplicity = TRUE)
}

network_measures <- function(proj, aa_threshold = params$aa_threshold) {
  if (igraph::ecount(proj) < params$min_edges_network)
    return(c(polarisation = NA_real_, rop = NA_real_, rop_bridge = NA_real_,
             n_nodes = igraph::vcount(proj), n_edges = igraph::ecount(proj)))
  set.seed(params$seed)
  cl <- igraph::cluster_louvain(proj, weights = igraph::E(proj)$weight)
  aa <- igraph::similarity(proj, method = "invlogweighted")
  adj <- as.matrix(igraph::as_adjacency_matrix(proj, sparse = TRUE)) > 0
  ut <- upper.tri(aa)
  memb <- igraph::membership(cl)
  across <- outer(memb, memb, "!=")[ut]
  is_latent <- aa[ut] >= aa_threshold & !adj[ut]
  latent <- sum(is_latent)
  realised <- sum(adj[ut])
  # rop: all latent ties; rop_bridge: latent ties that would join different
  # communities (potential links between parts of the field, as in DP3)
  c(polarisation = igraph::modularity(cl), rop = latent / (latent + realised),
    rop_bridge = sum(is_latent & across) / (latent + realised),
    n_nodes = igraph::vcount(proj), n_edges = igraph::ecount(proj))
}

# ---- Platform-month panel ------------------------------------------------------
build_panel <- function(threads, events, coal, topics, frame = sample_frame) {
  closure_rx <- paste0("\\b(", paste(gsub("'", "'?", closure_terms), collapse = "|"), ")\\b")
  th <- coal |> dplyr::left_join(threads |> dplyr::select(platform, number) |>
                                   dplyr::mutate(topic = topics$topic), by = c("platform", "number"))
  ends <- frame |> dplyr::select(platform, window_end)

  base <- th |>
    dplyr::left_join(ends, by = "platform") |>
    dplyr::group_by(platform, month) |>
    dplyr::summarise(
      n_threads = dplyr::n(),
      stalemate = if (dplyr::first(month) + 31 + params$stalemate_days <= dplyr::first(window_end))
        mean(!(resolved == 1 & duration <= params$stalemate_days)) else NA_real_,
      ttr_median = stats::median(duration[resolved == 1]),
      topic_entropy = { t <- topic[!is.na(topic)]; if (length(t) < 5) NA_real_ else { q <- prop.table(table(t)); -sum(q * log(q)) } },
      .groups = "drop")

  ev <- events |> dplyr::mutate(month = lubridate::floor_date(as.Date(time), "month"))
  net <- ev |>
    dplyr::group_by(platform, month) |>
    dplyr::group_modify(~ tibble::as_tibble(as.list(network_measures(month_network(.x))))) |>
    dplyr::ungroup()
  clo <- ev |>
    dplyr::filter(type != "opening") |>
    dplyr::mutate(b = tolower(body)) |>
    dplyr::group_by(platform, month) |>
    dplyr::summarise(closure_rate = 1000 * sum(stringr::str_count(b, closure_rx)) /
                       max(1, sum(stringr::str_count(b, "\\S+"))),
                     n_contributors = dplyr::n_distinct(author), .groups = "drop")

  base |>
    dplyr::full_join(net, by = c("platform", "month")) |>
    dplyr::full_join(clo, by = c("platform", "month")) |>
    dplyr::left_join(frame |> dplyr::select(platform, role, shock, parent), by = "platform") |>
    dplyr::arrange(platform, month) |>
    dplyr::group_by(platform) |>
    dplyr::mutate(z_stalemate = zscore(stalemate), z_polarisation = zscore(polarisation),
                  z_closure = zscore(closure_rate), z_monotheme = -zscore(topic_entropy)) |>
    dplyr::ungroup() |>
    dplyr::mutate(vortex = rowMeans(cbind(z_stalemate, z_polarisation, z_closure, z_monotheme), na.rm = TRUE),
                  vortex = ifelse(is.nan(vortex), NA, vortex))
}
