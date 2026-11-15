# =============================================================================
# 01_collect.R  -  download the deliberation records from the GitHub REST API
#
# Resumable: every page is saved to data/raw/<platform>/<endpoint>/ as it
# arrives, with the next page's URL in a state file. If R stops (rate limit,
# lost connection, closed laptop), run this script again and it carries on.
# =============================================================================

library(httr2)

API <- "https://api.github.com"

gh_request <- function(url, query = list()) {
  tok <- github_token()
  req <- request(url) |>
    req_url_query(!!!query) |>
    req_headers(Accept = "application/vnd.github+json",
                `X-GitHub-Api-Version` = "2022-11-28",
                `User-Agent` = "PlatformSTS-research") |>
    req_error(is_error = function(resp) FALSE) |>
    req_timeout(60)
  if (tok != "") req <- req_auth_bearer_token(req, tok)
  req
}

# Perform a request, waiting out primary and secondary rate limits.
gh_perform <- function(req, tries = 8) {
  for (i in seq_len(tries)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) { message("  network error, retry in 30s: ", conditionMessage(resp)); Sys.sleep(30); next }
    st <- resp_status(resp)
    if (st == 200) {
      rem <- suppressWarnings(as.integer(resp_header(resp, "x-ratelimit-remaining")))
      if (!is.na(rem) && rem < 20) wait_for_reset(resp)
      return(resp)
    }
    if (st %in% c(403, 429)) {
      ra <- resp_header(resp, "retry-after")
      if (!is.null(ra)) { message("  secondary rate limit, waiting ", ra, "s"); Sys.sleep(as.numeric(ra) + 1); next }
      if (identical(resp_header(resp, "x-ratelimit-remaining"), "0")) { wait_for_reset(resp); next }
      stop("GitHub refused the request (", st, "). Is GITHUB_PAT set and valid? ",
           resp_body_string(resp))
    }
    if (st %in% c(500, 502, 503, 504)) { message("  server error ", st, ", retry in 20s"); Sys.sleep(20); next }
    if (st %in% c(404, 410, 422)) return(NULL)   # deleted user / past pagination limit
    stop("Unexpected status ", st, " for ", req$url)
  }
  stop("Gave up after ", tries, " attempts: ", req$url)
}

wait_for_reset <- function(resp) {
  reset <- as.numeric(resp_header(resp, "x-ratelimit-reset"))
  secs <- max(5, reset - as.numeric(Sys.time()) + 5)
  message(sprintf("  rate limit reached; sleeping %.0f min (until %s)", secs / 60,
                  format(Sys.time() + secs, "%H:%M")))
  Sys.sleep(secs)
}

trim_body <- function(x, n = 5000) { x <- ifelse(is.na(x), "", x); substr(x, 1, n) }
`%||%` <- function(a, b) if (is.null(a)) b else a
chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)

# ---- Parsers: keep only the fields the analysis needs -----------------------
parse_issues <- function(items) purrr::map_dfr(items, function(x) tibble::tibble(
  number = x$number, is_pr = !is.null(x$pull_request),
  author = chr(x$user$login), author_assoc = chr(x$author_association),
  created_at = chr(x$created_at), closed_at = chr(x$closed_at),
  state = chr(x$state), n_comments = x$comments %||% NA_integer_,
  labels = paste(purrr::map_chr(x$labels, ~ chr(.x$name)), collapse = ";"),
  title = chr(x$title), body = trim_body(chr(x$body))))

parse_issue_comments <- function(items) purrr::map_dfr(items, function(x) tibble::tibble(
  id = as.numeric(x$id %||% NA), number = as.integer(sub(".*/", "", x$issue_url)), type = "issue_comment",
  author = chr(x$user$login), author_assoc = chr(x$author_association),
  created_at = chr(x$created_at), updated_at = chr(x$updated_at), body = trim_body(chr(x$body))))

parse_review_comments <- function(items) purrr::map_dfr(items, function(x) tibble::tibble(
  id = as.numeric(x$id %||% NA), number = as.integer(sub(".*/", "", x$pull_request_url)), type = "review_comment",
  author = chr(x$user$login), author_assoc = chr(x$author_association),
  created_at = chr(x$created_at), updated_at = chr(x$updated_at), body = trim_body(chr(x$body))))

parse_commits <- function(items) purrr::map_dfr(items, function(x) tibble::tibble(
  sha = x$sha, author = chr(x$author$login),
  email = chr(x$commit$author$email), date = chr(x$commit$author$date)))

# ---- Generic resumable pager -------------------------------------------------
# Pages newest -> oldest and stops once items are older than `start`.
collect_endpoint <- function(platform, endpoint, path, query, parser, start, date_col = "created_at",
                             stop_on_date = TRUE) {
  dir <- file.path(paths$raw, platform, endpoint)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  state_file <- file.path(dir, "_state.rds")
  st <- if (file.exists(state_file)) readRDS(state_file) else
    list(next_url = NULL, page = 0, done = FALSE)
  if (st$done) { message("  ", basename(dir), ": already complete (", st$page, " pages)"); return(invisible()) }

  url <- st$next_url %||% paste0(API, path)
  q <- if (is.null(st$next_url)) c(query, per_page = 100) else list()
  repeat {
    resp <- gh_perform(gh_request(url, q))
    if (is.null(resp)) { st$done <- TRUE; saveRDS(st, state_file); break }
    items <- resp_body_json(resp, simplifyVector = FALSE)
    if (length(items) == 0) { st$done <- TRUE; saveRDS(st, state_file); break }
    df <- parser(items)
    st$page <- st$page + 1
    saveRDS(df, file.path(dir, sprintf("page_%05d.rds", st$page)))
    oldest <- suppressWarnings(min(as.Date(substr(df[[date_col]], 1, 10)), na.rm = TRUE))
    nxt <- resp_link_url(resp, "next")
    if (st$page %% 10 == 0)
      message(sprintf("  %s: page %d, reached %s", basename(dir), st$page, oldest))
    if (is.null(nxt) || (stop_on_date && is.finite(oldest) && oldest < start)) {
      st$done <- TRUE; st$next_url <- NULL; saveRDS(st, state_file); break
    }
    st$next_url <- nxt; saveRDS(st, state_file)
    url <- nxt; q <- list()
  }
  message(sprintf("  %s: done, %d pages", basename(dir), st$page))
}

# ---- Threads through the search API ------------------------------------------
# Search returns at most 1,000 results per query, so the window is cut into
# date ranges small enough to stay under that (split in half when needed).
collect_search_threads <- function(platform, owner, repo, start, end) {
  dir <- file.path(paths$raw, platform, "issues")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  state_file <- file.path(dir, "_state.rds")
  st <- if (file.exists(state_file)) readRDS(state_file) else list(done = FALSE, chunks = character(), page = 0)
  if (isTRUE(st$done)) { message("  issues: already complete"); return(invisible()) }
  if (is.null(st$chunks)) st$chunks <- character()
  search_page <- function(q, page) {
    resp <- gh_perform(gh_request(paste0(API, "/search/issues"),
                                  list(q = q, per_page = 100, page = page, sort = "created", order = "asc")))
    Sys.sleep(2.2)                                   # search API: 30 requests/minute
    if (is.null(resp)) NULL else resp_body_json(resp, simplifyVector = FALSE)
  }
  do_range <- function(a, b, kind) {
    key <- paste(kind, a, b)
    if (key %in% st$chunks) return(invisible())
    q <- sprintf("repo:%s/%s is:%s created:%s..%s", owner, repo, kind, a, b)
    first <- search_page(q, 1)
    if (is.null(first)) return(invisible())
    if (first$total_count > 1000 && a < b) {
      mid <- a + floor(as.numeric(b - a) / 2)
      do_range(a, mid, kind); do_range(mid + 1, b, kind)
    } else {
      pages <- list(first$items)
      n_pages <- min(10, ceiling(first$total_count / 100))
      if (n_pages > 1) for (pg in 2:n_pages) { r <- search_page(q, pg); if (!is.null(r)) pages[[pg]] <- r$items }
      items <- unlist(pages, recursive = FALSE)
      if (length(items)) {
        st$page <<- st$page + 1
        saveRDS(parse_issues(items), file.path(dir, sprintf("page_%05d.rds", st$page)))
      }
    }
    st$chunks <<- c(st$chunks, key); saveRDS(st, state_file)
  }
  months <- seq(lubridate::floor_date(start, "month"), end, by = "month")
  for (m in seq_along(months)) {
    a <- max(months[m], start); b <- min(months[m] %m+% months(1) - 1, end)
    for (kind in c("issue", "pr")) do_range(a, b, kind)
    if (m %% 6 == 0) message(sprintf("  issues: reached %s", b))
  }
  st$done <- TRUE; saveRDS(st, state_file)
  message("  issues: done")
}

# ---- Comments, walked forward in time by last update -------------------------
# The plain comment listing stops after ~300 pages. Walking with ?since=
# sorted by update time, and restarting from the last timestamp whenever the cap
# is reached, reaches every comment. Stops once updates pass stop_date.
collect_since_walk <- function(platform, endpoint, path, parser, start, stop_date) {
  dir <- file.path(paths$raw, platform, endpoint)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  state_file <- file.path(dir, "_state.rds")
  st <- if (file.exists(state_file)) readRDS(state_file) else
    list(done = FALSE, since = paste0(start, "T00:00:00Z"), next_url = NULL, page = 0, chunk_pages = 0)
  if (isTRUE(st$done)) { message("  ", endpoint, ": already complete (", st$page, " pages)"); return(invisible()) }
  if (is.null(st$since)) st <- list(done = FALSE, since = paste0(start, "T00:00:00Z"), next_url = NULL, page = 0, chunk_pages = 0)
  repeat {
    if (is.null(st$next_url)) {
      req <- gh_request(paste0(API, path), list(sort = "updated", direction = "asc",
                                                since = st$since, per_page = 100))
      st$chunk_pages <- 0
    } else req <- gh_request(st$next_url)
    resp <- gh_perform(req)
    items <- if (is.null(resp)) list() else resp_body_json(resp, simplifyVector = FALSE)
    if (length(items)) {
      df <- parser(items)
      st$page <- st$page + 1; st$chunk_pages <- st$chunk_pages + 1
      saveRDS(df, file.path(dir, sprintf("page_%05d.rds", st$page)))
      last <- max(df$updated_at, na.rm = TRUE); st$last <- last
      if (st$page %% 20 == 0) message(sprintf("  %s: page %d, reached %s", endpoint, st$page, substr(last, 1, 10)))
      if (as.Date(substr(last, 1, 10)) > stop_date) break
    }
    nxt <- if (is.null(resp)) NULL else resp_link_url(resp, "next")
    if (!is.null(nxt)) { st$next_url <- nxt; saveRDS(st, state_file); next }
    # no next page: either the end of the data, or GitHub's page cap
    if (length(items) == 100 || is.null(resp) || st$chunk_pages >= 250) {
      new_since <- st$last
      if (is.null(new_since) || identical(new_since, st$since)) break   # no progress: stop rather than loop
      st$since <- new_since; st$next_url <- NULL; saveRDS(st, state_file); next
    }
    break
  }
  st$done <- TRUE; st$next_url <- NULL; saveRDS(st, state_file)
  message(sprintf("  %s: done, %d pages", endpoint, st$page))
}

read_endpoint <- function(platform, endpoint) {
  dir <- file.path(paths$raw, platform, endpoint)
  f <- list.files(dir, pattern = "^page_.*\\.rds$", full.names = TRUE)
  if (!length(f)) return(tibble::tibble())
  dplyr::bind_rows(lapply(f, readRDS))
}

# ---- Per-repository collection -----------------------------------------------
collect_repo <- function(row) {
  message("\n== ", row$platform, " (", row$owner, "/", row$repo, ") ",
          row$window_start, " to ", row$window_end)
  base <- sprintf("/repos/%s/%s", row$owner, row$repo)
  meta_file <- file.path(paths$raw, row$platform, "repo_meta.rds")
  dir.create(dirname(meta_file), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(meta_file)) {
    m <- resp_body_json(gh_perform(gh_request(paste0(API, base))))
    saveRDS(list(created_at = m$created_at, full_name = m$full_name,
                 stars = m$stargazers_count, forks = m$forks_count), meta_file)
  }
  stop_date <- row$window_end + 180      # comments on window threads rarely come later
  issues_state <- file.path(paths$raw, row$platform, "issues", "_state.rds")
  if (!(file.exists(issues_state) && isTRUE(readRDS(issues_state)$done)))
    collect_search_threads(row$platform, row$owner, row$repo, row$window_start, row$window_end)
  else message("  issues: already complete")
  collect_since_walk(row$platform, "issue_comments_v2", paste0(base, "/issues/comments"),
                     parse_issue_comments, row$window_start, stop_date)
  collect_since_walk(row$platform, "review_comments", paste0(base, "/pulls/comments"),
                     parse_review_comments, row$window_start, stop_date)
  collect_endpoint(row$platform, "commits", paste0(base, "/commits"),
                   list(since = paste0(row$window_start, "T00:00:00Z"),
                        until = paste0(row$window_end, "T23:59:59Z")),
                   parse_commits, row$window_start, date_col = "date", stop_on_date = FALSE)
  message("== ", row$platform, " complete")
}

# ---- Affiliations ------------------------------------------------------------
# 1) commit e-mail domain (personal webmail and GitHub no-reply -> unknown);
# 2) for the most active users still unknown, the 'company' field of their profile.
webmail <- "(^|\\.)(gmail\\.com|googlemail\\.com|hotmail\\.|outlook\\.|live\\.|yahoo\\.|icloud\\.com|me\\.com|mac\\.com|protonmail\\.|proton\\.me|qq\\.com|163\\.com|126\\.com|gmx\\.|mail\\.ru|yandex\\.)|noreply|no-reply|localhost|example\\.|\\.local$"
normalise_org <- function(x) {
  x <- tolower(trimws(x)); x <- sub("^@", "", x)
  x <- gsub("[,.]?\\s*(inc|llc|ltd|gmbh|corp|corporation|co)\\.?$", "", x)
  x <- sub("\\.(com|io|org|net|co|dev|ai|de|fr|uk|cn|jp|in)(\\..*)?$", "", x)
  x[x %in% c("", "na", "none", "n/a", "-", "self", "self-employed", "freelance", "independent")] <- NA
  x
}

build_affiliations <- function(platform) {
  cm <- read_endpoint(platform, "commits")
  aff <- if (nrow(cm)) cm |>
    dplyr::filter(!is.na(author), !is.na(email)) |>
    dplyr::mutate(domain = tolower(sub(".*@", "", email))) |>
    dplyr::filter(!grepl(webmail, domain)) |>
    dplyr::count(author, domain, sort = TRUE) |>
    dplyr::group_by(author) |> dplyr::slice(1) |> dplyr::ungroup() |>
    dplyr::transmute(author, org = normalise_org(domain)) else
    tibble::tibble(author = character(), org = character())
  aff
}

lookup_profiles <- function(platform, logins) {
  f <- file.path(paths$raw, platform, "profiles.rds")
  done <- if (file.exists(f)) readRDS(f) else tibble::tibble(author = character(), company = character())
  todo <- setdiff(logins, done$author)
  for (u in todo) {
    resp <- gh_perform(gh_request(paste0(API, "/users/", u)))
    comp <- if (is.null(resp)) NA_character_ else chr(resp_body_json(resp)$company)
    done <- dplyr::bind_rows(done, tibble::tibble(author = u, company = comp))
    if (nrow(done) %% 50 == 0) saveRDS(done, f)
  }
  saveRDS(done, f)
  done
}

# ---- Estimate download size before starting ----------------------------------
# Uses the search API (one call per repository) to count issues + pull requests.
# Comments are fetched newest-first back to window_start, so the scan covers
# window_start -> today. Assumes ~6 comments and ~1.5 review comments per thread.
estimate_collection <- function(frame = sample_frame, comments_per_thread = 6, reviews_per_thread = 1.5) {
  if (github_token() == "") stop("Set GITHUB_PAT first (see 00_setup.R).")
  est <- purrr::map_dfr(seq_len(nrow(frame)), function(i) {
    r <- frame[i, ]
    q <- function(a, b) {
      resp <- gh_perform(gh_request(paste0(API, "/search/issues"),
        list(q = sprintf("repo:%s/%s created:%s..%s", r$owner, r$repo, a, b), per_page = 1)))
      Sys.sleep(2.5)                                   # search API: 30 requests/minute
      resp_body_json(resp)$total_count
    }
    in_window <- q(r$window_start, r$window_end)
    scanned <- q(r$window_start, Sys.Date())
    tibble::tibble(platform = r$platform, threads_in_window = in_window, threads_scanned = scanned,
                   api_calls = ceiling(scanned / 100 * (1 + comments_per_thread + reviews_per_thread)))
  })
  est$hours_at_5000_per_hour <- round(est$api_calls / 4800, 1)
  print(est)
  message(sprintf("Total: about %s API calls, roughly %.1f hours (runs unattended; resumable).",
                  format(sum(est$api_calls), big.mark = ","), sum(est$api_calls) / 4800))
  invisible(est)
}

# ---- Run ---------------------------------------------------------------------
run_collection <- function(frame = sample_frame, workers = 4) {
  if (github_token() == "")
    stop("No GitHub token found. See 00_setup.R: set GITHUB_PAT in .Renviron and restart R.")
  lim <- resp_body_json(gh_perform(gh_request(paste0(API, "/rate_limit"))))
  message("API limit: ", lim$resources$core$limit, " requests/hour; ",
          lim$resources$core$remaining, " left now.")
  log <- file.path(paths$raw, "collect_log.txt")
  if (workers > 1) {
    message("Downloading ", nrow(frame), " platforms, ", workers, " at a time.\n",
            "Progress is written to ", log, " (open it from the Files pane to watch).\n",
            "The Console stays quiet until everything is downloaded.")
    cl <- parallel::makePSOCKcluster(workers, outfile = log)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    wd <- getwd(); tok <- github_token()
    parallel::clusterCall(cl, function(wd, tok) {
      setwd(wd); Sys.setenv(GITHUB_PAT = tok)
      suppressMessages({ source("R/00_setup.R"); source("R/00_config.R"); source("R/01_collect.R") })
      TRUE
    }, wd, tok)
    rows <- split(frame, seq_len(nrow(frame)))
    res <- parallel::parLapplyLB(cl, rows, function(r) {
      tryCatch({ collect_repo(r); "ok" }, error = function(e) paste("ERROR:", conditionMessage(e)))
    })
    names(res) <- frame$platform
    print(unlist(res))
    if (any(grepl("^ERROR", unlist(res))))
      stop("Some platforms stopped with an error (see above). Run run_all.R again to resume them.")
  } else {
    for (i in seq_len(nrow(frame))) collect_repo(frame[i, ])
  }
  # profile look-ups for active users without an affiliation from e-mail
  for (p in frame$platform) {
    aff <- build_affiliations(p)
    authors_of <- function(e) { x <- read_endpoint(p, e); if ("author" %in% names(x)) x$author else character() }
    act <- tibble::tibble(author = c(authors_of("issues"), authors_of("issue_comments_v2"), authors_of("review_comments"))) |>
      dplyr::filter(!is.na(author), !grepl(bot_regex, author, ignore.case = TRUE)) |>
      dplyr::count(author, sort = TRUE)
    need <- head(setdiff(act$author, aff$author[!is.na(aff$org)]), params$profile_lookups)
    if (length(need)) { message("Profile look-ups for ", p, ": ", length(need)); lookup_profiles(p, need) }
  }
  message("\nCollection complete.")
}
