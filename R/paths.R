# Path helpers for the project.
# Uses config.yml and/or environment variables to keep file locations flexible.

suppressPackageStartupMessages({
  if (requireNamespace("here", quietly = TRUE)) {
    library(here)
  }
  if (requireNamespace("yaml", quietly = TRUE)) {
    library(yaml)
  }
})

repo_root <- function() {
  if ("here" %in% .packages()) {
    return(here::here())
  }
  normalizePath(getwd())
}

read_config <- function() {
  cfg_path <- file.path(repo_root(), "config.yml")
  if (file.exists(cfg_path) && "yaml" %in% .packages()) {
    return(yaml::read_yaml(cfg_path))
  }
  list()
}

get_cfg <- function(key, default = NULL) {
  cfg <- read_config()
  if (!is.null(cfg[[key]])) {
    return(cfg[[key]])
  }
  default
}

# A config value that is already an absolute path (e.g. a Dropbox location)
# should be used as-is; a relative value (e.g. "data") stays relative to the
# repo root. file.path() doesn't do this reset on its own.
is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[\\\\/])", path)
}

data_root <- function() {
  env <- Sys.getenv("SCHOOL_BOARDS_DATA_ROOT")
  if (nzchar(env)) {
    return(path.expand(env))
  }
  cfg_val <- get_cfg("data_root", "data")
  if (is_absolute_path(cfg_val)) {
    return(path.expand(cfg_val))
  }
  file.path(repo_root(), cfg_val)
}

external_root <- function() {
  env <- Sys.getenv("SCHOOL_BOARDS_EXTERNAL_ROOT")
  if (nzchar(env)) {
    return(path.expand(env))
  }
  get_cfg("external_data_root", "")
}

graphs_dir <- function() {
  cfg_val <- get_cfg("graphs_dir", "graphs")
  if (is_absolute_path(cfg_val)) {
    return(path.expand(cfg_val))
  }
  file.path(data_root(), cfg_val)
}

data_path <- function(...) {
  file.path(data_root(), ...)
}

ext_path <- function(...) {
  root <- external_root()
  if (!nzchar(root)) {
    stop("External data root not set. Set SCHOOL_BOARDS_EXTERNAL_ROOT or config.yml external_data_root.")
  }
  file.path(root, ...)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(path)
}
