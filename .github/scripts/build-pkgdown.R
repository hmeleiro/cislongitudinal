precache_bootstrap_toc <- function() {
  cache_dir <- file.path(
    tools::R_user_dir("pkgdown", "cache"),
    "bootstrap-toc",
    "1.0.1"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_path <- file.path(cache_dir, "bootstrap-toc.min.js")
  expected_sha256 <- "e2f79541bbbbfff2e4e534a673b615e3c331b4ccbdf1edba71fe4cae06589f0a"
  urls <- c(
    "https://raw.githubusercontent.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js",
    "https://cdn.jsdelivr.net/gh/afeld/bootstrap-toc@v1.0.1/dist/bootstrap-toc.min.js"
  )

  for (url in urls) {
    ok <- tryCatch(
      {
        utils::download.file(url, cache_path, quiet = TRUE, mode = "wb")
        identical(
          digest::digest(file = cache_path, algo = "sha256", serialize = FALSE),
          expected_sha256
        )
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (isTRUE(ok)) {
      return(invisible(cache_path))
    }
    unlink(cache_path, force = TRUE)
  }

  stop("Could not pre-cache bootstrap-toc for pkgdown.")
}

build_site_with_retries <- function(attempts = 3) {
  for (attempt in seq_len(attempts)) {
    message("pkgdown build attempt ", attempt, " of ", attempts)
    unlink("docs", recursive = TRUE, force = TRUE)
    unlink(tools::R_user_dir("pkgdown", "cache"), recursive = TRUE, force = TRUE)
    precache_bootstrap_toc()

    result <- try(pkgdown::build_site(), silent = FALSE)
    if (!inherits(result, "try-error")) {
      return(invisible(TRUE))
    }
    if (attempt == attempts) {
      stop(attr(result, "condition"))
    }
    Sys.sleep(30 * attempt)
  }
}

build_site_with_retries()
