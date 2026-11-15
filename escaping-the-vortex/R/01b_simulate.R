# =============================================================================
# 01b_simulate.R  -  synthetic data with the same structure as the cleaned
# GitHub data. Used only when SIMULATE <- TRUE, to test the pipeline end to end.
# Effects are planted so you can see the models recover them:
#   after a shock, treated platforms stall, polarise and narrow their topics,
#   less so when their contributor field is more cross-connected (higher RoP).
#   No fork effect is planted, so H4 should come out null on simulated data.
# =============================================================================

simulate_data <- function(frame = sample_frame, seed = params$seed, threads_per_month = 40) {
  set.seed(seed)
  K <- 30
  vocab <- lapply(1:K, function(k) paste0("t", k, "w", 1:15))
  general <- c("issue", "error", "version", "config", "request", "support", "update",
               "build", "test", "docs", "release", "cluster", "node", "client", "api")
  orgs <- c("acme", "globex", "initech", "umbrella", "hooli", "stark", "wayne", "tyrell", "cyberdyne", "wonka")
  mk_text <- function(k, n) paste(c(sample(vocab[[k]], n, TRUE), sample(general, 3, TRUE)), collapse = " ")

  threads <- list(); events <- list(); aff <- list(); meta <- list()
  users_by_platform <- list()
  # cross-group mixing (drives RoP); treated platforms differ so H2 has variation
  mixing <- c(elasticsearch = 0.15, terraform = 0.35, redis = 0.55)

  for (i in seq_len(nrow(frame))) {
    r <- frame[i, ]
    p <- r$platform
    n_users <- 400
    owner_org <- if (r$role == "control") sample(orgs, 1) else p
    users <- tibble::tibble(
      author = paste0(p, "_u", 1:n_users),
      group = sample(1:4, n_users, TRUE, prob = c(.4, .25, .2, .15)),
      org = sample(c(owner_org, orgs, NA), n_users, TRUE, prob = c(.3, rep(.05, 10), .2)),
      activity = rexp(n_users, 1))
    users$assoc <- ifelse(users$org %in% owner_org & runif(n_users) < .5, "MEMBER", "CONTRIBUTOR")
    if (r$role == "fork") {
      parent <- users_by_platform[[r$parent]]
      # fork recruits: dissenting-camp members (groups 2-4) and some cross-connected users
      take <- parent |> dplyr::filter(group != 1) |> dplyr::slice_sample(prop = .35)
      users <- dplyr::bind_rows(take |> dplyr::mutate(assoc = "MEMBER"), users |> dplyr::slice(1:250))
      users$org[is.na(users$org)] <- sample(orgs, sum(is.na(users$org)), TRUE)
    }
    users_by_platform[[p]] <- users
    aff[[p]] <- users |> dplyr::transmute(platform = p, author, org)

    months <- seq(lubridate::floor_date(r$window_start, "month"), r$window_end, by = "month")
    shock <- if (!is.na(r$shock)) r$shock else as.Date("2100-01-01")
    mix <- if (p %in% names(mixing)) mixing[[p]] else if (r$role == "fork") .4 else runif(1, .25, .45)
    meta[[p]] <- tibble::tibble(platform = p, repo_created = r$window_start)
    num <- 0
    for (m in seq_along(months)) {
      mo <- months[m]
      post <- mo >= lubridate::floor_date(shock, "month")
      eff <- if (post) (1 - mix) * min(1, as.numeric(mo - shock) / 120 + .3) else 0
      n_t <- rpois(1, threads_per_month * (if (post) 1.3 else 1))
      topic_probs <- rep(1, K); if (post) topic_probs[1:3] <- 1 + 25 * eff
      for (j in seq_len(n_t)) {
        num <- num + 1
        k <- sample(1:K, 1, prob = topic_probs)
        g <- sample(1:4, 1, prob = c(.4, .25, .2, .15))
        p_in <- min(.97, 1 - mix + .35 * eff)
        n_part <- 1 + rpois(1, 2.5)
        pool_in <- users$author[users$group == g]; pool_out <- users$author[users$group != g]
        wts_in <- users$activity[users$group == g]; wts_out <- users$activity[users$group != g]
        parts <- unique(vapply(seq_len(n_part), function(z)
          if (runif(1) < p_in) sample(pool_in, 1, prob = wts_in) else sample(pool_out, 1, prob = wts_out), ""))
        created <- mo + sample(0:27, 1) + lubridate::seconds(sample(0:86399, 1))
        has_maint <- any(users$assoc[match(parts, users$author)] == "MEMBER")
        rate <- 1 / (12 * (1 + 4 * eff) * (if (has_maint) .7 else 1.3) / (1 + .1 * length(parts)))
        dur <- rexp(1, rate)
        closed <- if (runif(1) < .85 - .3 * eff) created + lubridate::ddays(dur) else as.POSIXct(NA)
        is_pr <- runif(1) < .4
        threads[[length(threads) + 1]] <- tibble::tibble(
          platform = p, number = num, is_pr = is_pr, author = parts[1],
          author_assoc = users$assoc[match(parts[1], users$author)],
          created = created, closed = closed, title = mk_text(k, 4), body = mk_text(k, 25))
        n_c <- rpois(1, 3) + length(parts) - 1
        if (n_c > 0) {
          who <- sample(parts, n_c, TRUE)
          ctime <- created + lubridate::dhours(sort(runif(n_c, 0, 24 * max(min(dur, 60), 0.1))))
          closure <- runif(n_c) < (.03 + .15 * eff)
          body <- vapply(seq_len(n_c), function(z) paste(mk_text(k, 10),
                    if (closure[z]) sample(closure_terms, 1) else ""), "")
          events[[length(events) + 1]] <- tibble::tibble(
            platform = p, number = num, author = who,
            author_assoc = users$assoc[match(who, users$author)],
            time = ctime, body = body,
            type = ifelse(is_pr & runif(n_c) < .5, "review_comment", "issue_comment"))
        }
      }
    }
  }
  threads <- dplyr::bind_rows(threads)
  ev <- dplyr::bind_rows(events)
  openings <- threads |> dplyr::transmute(platform, number, author, author_assoc, time = created,
                                          body = paste(title, body), type = "opening")
  list(threads = threads, events = dplyr::bind_rows(openings, ev) |> dplyr::filter(time <= max(frame$window_end) + 1),
       aff = dplyr::bind_rows(aff), meta = dplyr::bind_rows(meta))
}
