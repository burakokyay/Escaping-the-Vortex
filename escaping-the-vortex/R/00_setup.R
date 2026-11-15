# =============================================================================
# 00_setup.R  -  install packages and check the GitHub token
# =============================================================================

pkgs <- c("httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr", "lubridate",
          "readr", "tibble", "ggplot2", "igraph", "quanteda", "stm", "fixest", "did",
          "survival", "glmmTMB", "broom", "irr", "flextable", "officer")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(lubridate); library(readr); library(tibble); library(ggplot2)
})

# ---- GitHub token ------------------------------------------------------------
# A personal access token raises the API limit from 60 to 5,000 requests/hour.
# 1. Create one at https://github.com/settings/tokens  (classic token; no scopes
#    needed for public repositories - leave every box unticked), or run
#    usethis::create_github_token().
# 2. Put it in your user .Renviron as  GITHUB_PAT=ghp_xxx  (usethis::edit_r_environ()),
#    then restart R.  Never paste the token into a script.
github_token <- function() {
  tok <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
  if (tok == "" && requireNamespace("gitcreds", quietly = TRUE)) {
    tok <- tryCatch(gitcreds::gitcreds_get()$password, error = function(e) "")
  }
  tok
}
