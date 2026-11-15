# =============================================================================
# 02_clean.R  -  raw pages -> four clean tables
#   threads : one row per issue / pull request (the trace of a deliberation)
#   events  : one row per contribution (opening post, comment, review comment)
#   aff     : contributor -> organisation
#   meta    : repository creation dates
# Bots are removed; dates are restricted to each platform's window.
# =============================================================================

is_bot <- function(x) is.na(x) | grepl(bot_regex, x, ignore.case = TRUE)

clean_platform <- function(row) {
  p <- row$platform
  iss <- read_endpoint(p, "issues")
  if (!nrow(iss)) { warning("No issues downloaded for ", p); return(NULL) }
  threads <- iss |>
    dplyr::distinct(number, .keep_all = TRUE) |>
    dplyr::mutate(platform = p, created = lubridate::ymd_hms(created_at, quiet = TRUE),
                  closed = lubridate::ymd_hms(closed_at, quiet = TRUE)) |>
    dplyr::filter(as.Date(created) >= row$window_start, as.Date(created) <= row$window_end,
                  !is_bot(author)) |>
    dplyr::select(platform, number, is_pr, author, author_assoc, created, closed, title, body)

  com <- dplyr::bind_rows(read_endpoint(p, "issue_comments_v2"), read_endpoint(p, "review_comments"))
  if (nrow(com)) {
    if (!"id" %in% names(com)) com$id <- NA_real_
    com <- dplyr::bind_rows(com |> dplyr::filter(!is.na(id)) |> dplyr::distinct(type, id, .keep_all = TRUE),
                            com |> dplyr::filter(is.na(id)) |> dplyr::distinct())
  }
  events <- dplyr::bind_rows(
    threads |> dplyr::transmute(platform, number, author, author_assoc, time = created,
                                body = paste(title, body), type = "opening"),
    com |> dplyr::mutate(platform = p, time = lubridate::ymd_hms(created_at, quiet = TRUE)) |>
      dplyr::filter(number %in% threads$number, !is_bot(author)) |>
      dplyr::select(platform, number, author, author_assoc, time, body, type)) |>
    dplyr::filter(as.Date(time) <= row$window_end)

  aff <- build_affiliations(p)
  pf <- file.path(paths$raw, p, "profiles.rds")
  if (file.exists(pf)) {
    prof <- readRDS(pf) |> dplyr::transmute(author, org_p = normalise_org(company))
    aff <- dplyr::full_join(aff, prof, by = "author") |>
      dplyr::transmute(author, org = dplyr::coalesce(org, org_p))
  }
  meta <- readRDS(file.path(paths$raw, p, "repo_meta.rds"))
  list(threads = threads, events = events,
       aff = aff |> dplyr::mutate(platform = p, .before = 1),
       meta = tibble::tibble(platform = p, repo_created = as.Date(substr(meta$created_at, 1, 10))))
}

build_clean_data <- function(frame = sample_frame) {
  parts <- purrr::map(seq_len(nrow(frame)), ~ clean_platform(frame[.x, ]))
  parts <- purrr::compact(parts)
  list(threads = dplyr::bind_rows(purrr::map(parts, "threads")),
       events  = dplyr::bind_rows(purrr::map(parts, "events")),
       aff     = dplyr::bind_rows(purrr::map(parts, "aff")),
       meta    = dplyr::bind_rows(purrr::map(parts, "meta")))
}
