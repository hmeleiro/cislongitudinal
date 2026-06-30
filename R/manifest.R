#' Read the CIS manifest
#'
#' @param remote If `FALSE`, read the local manifest. If `TRUE`, fetch the
#'   remote manifest.
#'
#' @return A list with manifest fields, or an empty list when no valid manifest
#'   is available.
#' @export
cis_manifest <- function(remote = FALSE) {
    cis_check_bool(remote, "remote")
    if (remote) {
        return(cis_manifest_remote(quiet = FALSE))
    }
    cis_manifest_local()
}

cis_manifest_local <- function(path = cis_manifest_path()) {
    if (!file.exists(path)) {
        return(list())
    }
    tryCatch(
        jsonlite::read_json(path, simplifyVector = TRUE),
        error = function(e) {
            cli::cli_alert_warning("The local manifest could not be read: {conditionMessage(e)}")
            list()
        }
    )
}

cis_manifest_remote <- function(quiet = FALSE) {
    req <- httr2::request(.cis_manifest_url) |>
        httr2::req_user_agent(.cis_user_agent) |>
        httr2::req_timeout(20) |>
        httr2::req_error(is_error = function(resp) FALSE)

    resp <- tryCatch(
        httr2::req_perform(req),
        error = function(e) e
    )
    if (inherits(resp, "error")) {
        if (!quiet) {
            cli::cli_alert_warning("The remote manifest could not be reached: {conditionMessage(resp)}")
        }
        return(list())
    }
    status <- httr2::resp_status(resp)
    if (status == 404L) {
        if (!quiet) {
            cli::cli_alert_warning("The remote manifest was not found (HTTP 404).")
        }
        return(list())
    }
    if (status >= 400L) {
        if (!quiet) {
            cli::cli_alert_warning("The remote manifest returned HTTP {status}.")
        }
        return(list())
    }

    manifest <- tryCatch(
        cis_parse_manifest_response(resp),
        error = function(e) e
    )
    if (inherits(manifest, "error") || !is.list(manifest)) {
        if (!quiet) {
            cli::cli_alert_warning("The remote manifest is not valid JSON.")
        }
        return(list())
    }
    manifest
}

cis_parse_manifest_response <- function(resp) {
    text <- httr2::resp_body_string(resp)
    cis_parse_manifest_text(text)
}

cis_parse_manifest_text <- function(text) {
    jsonlite::fromJSON(text, simplifyVector = TRUE)
}

cis_manifest_parquet_url <- function(manifest) {
    manifest[["parquet_url"]] %||% .cis_parquet_url
}

cis_write_json <- function(x, path) {
    jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    invisible(path)
}
