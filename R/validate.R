cis_check_bool <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
        cli::cli_abort("{.arg {arg}} must be TRUE or FALSE.")
    }
    invisible(x)
}

cis_parse_date <- function(x, arg) {
    if (is.null(x)) {
        return(NULL)
    }
    if (length(x) != 1L || is.na(x)) {
        cli::cli_abort("{.arg {arg}} must be a single valid date.")
    }
    out <- tryCatch(
        as.Date(x),
        error = function(e) NA
    )
    if (is.na(out)) {
        cli::cli_abort(
            "{.arg {arg}} must be a valid date in a format like {.val 2023-01-01}."
        )
    }
    out
}

cis_clean_version <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x)) {
        return(NA_character_)
    }
    sub("^[vV]", "", as.character(x[[1L]]))
}

cis_compare_versions <- function(remote, local) {
    remote <- cis_clean_version(remote)
    local <- cis_clean_version(local)
    if (is.na(remote) || is.na(local)) {
        return(NA_integer_)
    }
    tryCatch(
        utils::compareVersion(remote, local),
        error = function(e) NA_integer_
    )
}

cis_version_newer <- function(remote, local) {
    if (is.null(local) || length(local) == 0L || is.na(local)) {
        return(!is.na(cis_clean_version(remote)))
    }
    cmp <- cis_compare_versions(remote, local)
    !is.na(cmp) && cmp > 0
}

cis_file_size <- function(path) {
    unname(file.info(path)[["size"]])
}

cis_validate_file <- function(path, manifest = list()) {
    if (!file.exists(path)) {
        cli::cli_abort("The downloaded file does not exist.")
    }
    size <- cis_file_size(path)
    if (is.na(size) || size <= 0) {
        cli::cli_abort("The downloaded file is empty.")
    }
    expected_size <- suppressWarnings(as.numeric(manifest[["size_bytes"]] %||% NA_real_))
    if (!is.na(expected_size) && !identical(as.numeric(size), expected_size)) {
        cli::cli_abort("The downloaded file size does not match the manifest.")
    }
    expected_sha <- manifest[["sha256"]] %||% NA_character_
    if (!is.na(expected_sha) && nzchar(expected_sha)) {
        if (!grepl("^[0-9a-fA-F]{64}$", expected_sha)) {
            cli::cli_abort("The manifest contains an invalid SHA-256 checksum.")
        }
        actual_sha <- tolower(digest::digest(file = path, algo = "sha256", serialize = FALSE))
        if (!identical(actual_sha, tolower(expected_sha))) {
            cli::cli_abort("The downloaded file SHA-256 checksum does not match the manifest.")
        }
    }
    invisible(TRUE)
}
