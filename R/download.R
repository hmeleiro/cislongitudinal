#' Download the CIS longitudinal Parquet file
#'
#' Downloads the dataset to the package cache if it is not already available.
#'
#' @param force Download again even when a valid local copy exists.
#' @param quiet Suppress informational messages.
#'
#' @return The local Parquet path, invisibly.
#' @export
cis_download <- function(force = FALSE, quiet = FALSE) {
    cis_check_bool(force, "force")
    cis_check_bool(quiet, "quiet")
    cis_download_impl(force = force, quiet = quiet)
}

#' Update the local CIS longitudinal dataset
#'
#' Checks the remote manifest and downloads the Parquet file when a newer version
#' is available.
#'
#' @param quiet Suppress informational messages.
#'
#' @return The local Parquet path, invisibly.
#' @export
cis_update <- function(quiet = FALSE) {
    cis_check_bool(quiet, "quiet")

    if (!cis_available()) {
        if (!quiet) {
            cli::cli_alert_info("No valid local copy was found; downloading the dataset.")
        }
        return(cis_download_impl(force = TRUE, quiet = quiet))
    }

    remote <- cis_manifest_remote(quiet = quiet)
    if (length(remote) == 0L) {
        if (!quiet) {
            cli::cli_alert_warning("Could not check for updates; using the local copy.")
        }
        return(invisible(cis_path()))
    }

    local <- cis_manifest_local()
    local_version <- local[["version"]] %||% NA_character_
    remote_version <- remote[["version"]] %||% NA_character_
    if (is.na(cis_clean_version(local_version)) &&
        cis_local_matches_manifest(cis_parquet_path(), remote)) {
        cis_write_json(remote, cis_manifest_path())
        if (!quiet) {
            cli::cli_alert_success("The local CIS dataset matches the remote manifest.")
        }
        return(invisible(cis_path()))
    }
    if (cis_version_newer(remote_version, local_version)) {
        if (!quiet) {
            if (is.na(cis_clean_version(local_version))) {
                cli::cli_alert_info("The local manifest is missing or incomplete; downloading the current dataset.")
            } else {
                cli::cli_alert_info("A newer CIS dataset is available ({remote_version}).")
            }
        }
        return(cis_download_impl(force = TRUE, quiet = quiet, manifest = remote))
    }

    if (!quiet) {
        cli::cli_alert_success("The local CIS dataset is up to date.")
    }
    invisible(cis_path())
}

#' Check whether a valid local CIS copy is available
#'
#' @return `TRUE` if the local Parquet file exists, has non-zero size, and can
#'   be opened by Arrow.
#' @export
cis_available <- function() {
    path <- cis_parquet_path()
    if (!file.exists(path)) {
        return(FALSE)
    }
    size <- cis_file_size(path)
    if (is.na(size) || size <= 0) {
        return(FALSE)
    }
    ok <- tryCatch(
        {
            arrow::open_dataset(path)
            TRUE
        },
        error = function(e) FALSE
    )
    isTRUE(ok)
}

cis_local_matches_manifest <- function(path, manifest) {
    tryCatch(
        {
            cis_validate_file(path, manifest)
            TRUE
        },
        error = function(e) FALSE
    )
}

cis_download_impl <- function(force = FALSE, quiet = FALSE, manifest = NULL) {
    path <- cis_parquet_path()
    if (!force && cis_available()) {
        if (!quiet) {
            cli::cli_alert_success("The CIS dataset is already available locally.")
        }
        return(invisible(normalizePath(path, winslash = "/", mustWork = TRUE)))
    }

    fs::dir_create(cis_cache_dir(), recurse = TRUE)

    manifest <- manifest %||% cis_manifest_remote(quiet = quiet)
    if (length(manifest) == 0L) {
        if (file.exists(path) && cis_file_size(path) > 0) {
            if (!quiet) {
                cli::cli_alert_warning("Could not check the manifest; using the local copy.")
            }
            return(invisible(normalizePath(path, winslash = "/", mustWork = TRUE)))
        }
        if (!quiet) {
            cli::cli_alert_warning("Could not read the manifest; trying the Parquet URL directly.")
        }
        manifest <- list(parquet_url = .cis_parquet_url)
    }

    url <- cis_manifest_parquet_url(manifest)
    tmp <- tempfile("cislongitudinal_", tmpdir = cis_cache_dir(), fileext = ".parquet")
    on.exit(unlink(tmp, force = TRUE), add = TRUE)

    size <- suppressWarnings(as.numeric(manifest[["size_bytes"]] %||% NA_real_))
    if (!quiet && !is.na(size)) {
        cli::cli_inform("Downloading CIS dataset ({sprintf('%.1f MB', size / 1024^2)})...")
    } else if (!quiet) {
        cli::cli_inform("Downloading CIS dataset...")
    }

    cis_download_file(url, tmp, quiet = quiet)
    cis_validate_file(tmp, manifest)
    cis_install_parquet(tmp, manifest)

    if (!quiet) {
        cli::cli_alert_success("CIS dataset installed at {.file {path}}.")
    }
    invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

cis_download_file <- function(url, path, quiet = FALSE) {
    req <- httr2::request(url) |>
        httr2::req_user_agent(.cis_user_agent) |>
        httr2::req_timeout(120)
    if (!quiet) {
        req <- httr2::req_progress(req)
    }
    tryCatch(
        httr2::req_perform(req, path = path),
        error = function(e) {
            cli::cli_abort("Could not download the CIS Parquet file: {conditionMessage(e)}")
        }
    )
    invisible(path)
}

cis_install_parquet <- function(candidate, manifest) {
    path <- cis_parquet_path()
    manifest_path <- cis_manifest_path()
    metadata_path <- cis_metadata_path()
    fs::dir_create(dirname(path), recurse = TRUE)

    manifest_tmp <- tempfile("manifest_", tmpdir = dirname(path), fileext = ".json")
    metadata_tmp <- tempfile("metadata_", tmpdir = dirname(path), fileext = ".json")
    on.exit(unlink(c(manifest_tmp, metadata_tmp), force = TRUE), add = TRUE)

    if (length(manifest) > 0L) {
        cis_write_json(manifest, manifest_tmp)
    }
    metadata <- list(
        downloaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        source_url = cis_manifest_parquet_url(manifest),
        package = "cislongitudinal",
        package_version = utils::packageDescription("cislongitudinal")[["Version"]] %||% "0.1.0"
    )
    cis_write_json(metadata, metadata_tmp)

    backups <- cis_backup_existing(c(path, manifest_path, metadata_path))
    installed <- FALSE
    on.exit({
        if (!installed) {
            unlink(c(path, manifest_path, metadata_path), force = TRUE)
            cis_restore_backups(backups)
        } else {
            unlink(unname(backups), force = TRUE)
        }
    }, add = TRUE)

    if (!file.rename(candidate, path)) {
        cli::cli_abort("Could not install the downloaded Parquet file.")
    }
    if (length(manifest) > 0L && !file.rename(manifest_tmp, manifest_path)) {
        cli::cli_abort("Could not install the local manifest.")
    }
    if (!file.rename(metadata_tmp, metadata_path)) {
        cli::cli_abort("Could not install the local metadata.")
    }
    installed <- TRUE
    invisible(path)
}

cis_backup_existing <- function(paths) {
    backups <- stats::setNames(rep(NA_character_, length(paths)), paths)
    for (path in paths) {
        if (!file.exists(path)) {
            next
        }
        backup <- tempfile("backup_", tmpdir = dirname(path), fileext = paste0(".", basename(path)))
        if (!file.rename(path, backup)) {
            cli::cli_abort("Could not prepare replacement of {.file {path}}.")
        }
        backups[[path]] <- backup
    }
    backups
}

cis_restore_backups <- function(backups) {
    for (path in names(backups)) {
        backup <- backups[[path]]
        if (!is.na(backup) && file.exists(backup)) {
            file.rename(backup, path)
        }
    }
    invisible(TRUE)
}
