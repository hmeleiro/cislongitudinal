#' Show information about the local CIS dataset
#'
#' @return Invisibly, a one-row tibble with local dataset metadata.
#' @export
cis_info <- function() {
    path <- cis_parquet_path()
    manifest <- cis_manifest_local()
    metadata <- cis_metadata_local()
    available <- cis_available()

    size <- if (file.exists(path)) cis_file_size(path) else NA_real_
    n_cols <- NA_integer_
    n_rows <- NA_real_
    if (available) {
        ds <- arrow::open_dataset(path)
        n_cols <- length(names(ds))
        n_rows <- cis_count_rows(ds)
    }

    out <- tibble::tibble(
        path = path,
        available = available,
        version = manifest[["version"]] %||% NA_character_,
        downloaded_at = metadata[["downloaded_at"]] %||% NA_character_,
        size_bytes = as.numeric(size),
        manifest_updated_at = manifest[["updated_at"]] %||% NA_character_,
        first_date = manifest[["first_date"]] %||% NA_character_,
        last_date = manifest[["last_date"]] %||% NA_character_,
        rows = n_rows,
        columns = n_cols
    )

    cli::cli_h2("CIS longitudinal dataset")
    cli::cli_ul(c(
        "Path: {.file {path}}",
        "Available: {available}",
        "Version: {out$version}",
        "Downloaded at: {out$downloaded_at}",
        "Size: {cis_format_size(size)}",
        "Manifest updated at: {out$manifest_updated_at}",
        "Earliest date: {out$first_date}",
        "Latest date: {out$last_date}",
        "Rows: {cis_format_unknown(n_rows)}",
        "Columns: {cis_format_unknown(n_cols)}"
    ))
    invisible(out)
}

#' Clear the local CIS cache
#'
#' @param confirm If `TRUE`, ask for interactive confirmation. If `FALSE`, delete
#'   the local Parquet, manifest, and metadata files directly.
#'
#' @return Invisibly, `TRUE` when files were removed or no files existed.
#' @export
cis_clear <- function(confirm = TRUE) {
    cis_check_bool(confirm, "confirm")
    files <- cis_cache_files()
    existing <- files[file.exists(files)]
    if (length(existing) == 0L) {
        cli::cli_alert_info("No local CIS files were found.")
        return(invisible(TRUE))
    }
    if (confirm) {
        if (!interactive()) {
            cli::cli_abort(
                "{.arg confirm} is TRUE but the session is not interactive. Use {.code cis_clear(confirm = FALSE)}."
            )
        }
        answer <- utils::askYesNo(
            sprintf("Delete %d local CIS cache file(s)?", length(existing)),
            default = FALSE
        )
        if (!isTRUE(answer)) {
            cli::cli_alert_info("Cache deletion cancelled.")
            return(invisible(FALSE))
        }
    }
    unlink(existing, force = TRUE)
    cli::cli_alert_success("Local CIS cache cleared.")
    invisible(TRUE)
}

cis_metadata_local <- function(path = cis_metadata_path()) {
    if (!file.exists(path)) {
        return(list())
    }
    tryCatch(
        jsonlite::read_json(path, simplifyVector = TRUE),
        error = function(e) list()
    )
}

cis_count_rows <- function(ds) {
    out <- tryCatch(
        {
            rows <- dplyr::summarise(ds, n = dplyr::n()) |>
                dplyr::collect()
            rows[["n"]][[1L]]
        },
        error = function(e) NA_real_
    )
    suppressWarnings(as.numeric(out))
}

cis_format_size <- function(size) {
    if (is.na(size)) {
        return("unknown")
    }
    units <- c("B", "KB", "MB", "GB")
    value <- as.numeric(size)
    idx <- 1L
    while (value >= 1024 && idx < length(units)) {
        value <- value / 1024
        idx <- idx + 1L
    }
    sprintf("%.1f %s", value, units[[idx]])
}

cis_format_unknown <- function(x) {
    if (length(x) == 0L || is.na(x)) "unknown" else as.character(x)
}
