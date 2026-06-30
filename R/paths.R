#' Return the cislongitudinal cache directory
#'
#' The dataset is stored in a persistent application data directory, not in the
#' current R project. Tests and advanced users may override the directory with
#' `options(cislongitudinal.cache_dir = "path")`.
#'
#' @return A character scalar with the cache directory.
#' @export
cis_cache_dir <- function() {
    getOption(
        "cislongitudinal.cache_dir",
        rappdirs::user_data_dir("cislongitudinal", "spainelectoralproject")
    )
}

#' Return the local Parquet path
#'
#' This function does not download data automatically. If the file is missing it
#' reports the expected path and suggests [cis_download()].
#'
#' @return A character scalar with the local Parquet path.
#' @export
cis_path <- function() {
    path <- cis_parquet_path()
    if (!file.exists(path)) {
        cli::cli_alert_info("No local CIS Parquet file was found at {.file {path}}.")
        cli::cli_alert_info("Run {.code cis_download()} to download it.")
        return(path)
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
}

cis_parquet_path <- function() {
    fs::path(cis_cache_dir(), .cis_parquet_file)
}

cis_manifest_path <- function() {
    fs::path(cis_cache_dir(), .cis_manifest_file)
}

cis_metadata_path <- function() {
    fs::path(cis_cache_dir(), .cis_metadata_file)
}

cis_cache_files <- function() {
    c(cis_parquet_path(), cis_manifest_path(), cis_metadata_path())
}
