ensure_dirs <- function(paths) {
  for (p in paths) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
}

log_message <- function(..., log_path = run_log_path) {
  msg <- paste0(...)
  message(msg)
  cat(msg, "\n", file = log_path, append = TRUE)
}
