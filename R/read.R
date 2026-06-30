#' Open the local CIS dataset lazily
#'
#' @return An Arrow dataset that can be queried with dplyr verbs.
#' @export
#'
#' @examples
#' \dontrun{
#' cis_open() |>
#'   dplyr::filter(fecha >= as.Date("2023-01-01")) |>
#'   dplyr::select(estudio, fecha, genero, edad) |>
#'   dplyr::collect()
#' }
cis_open <- function() {
    if (!cis_available()) {
        cli::cli_abort(c(
            "No valid local CIS Parquet file is available.",
            "i" = "Run {.code cis_download()} before reading the dataset."
        ))
    }
    arrow::open_dataset(cis_parquet_path())
}

#' Read the CIS longitudinal dataset
#'
#' Uses Arrow and dplyr to push filters and column selection down before data is
#' collected into memory.
#'
#' @param fecha_min Minimum survey date, included. `NULL` means no lower bound.
#' @param fecha_max Maximum survey date, included. `NULL` means no upper bound.
#' @param estudios Optional vector of study codes.
#' @param cols Optional tidyselect expression or character vector of columns to
#'   select.
#' @param keep_core_cols If `TRUE`, always include `estudio`, `fecha`, `genero`,
#'   and `edad` when `cols` is supplied.
#' @param collect If `TRUE`, return a tibble in memory. If `FALSE`, return a lazy
#'   Arrow/dplyr object.
#'
#' @return A tibble or a lazy Arrow query.
#' @export
#'
#' @examples
#' \dontrun{
#' cis_read(fecha_min = "2023-01-01")
#'
#' cis_read(
#'   fecha_min = "2020-01-01",
#'   fecha_max = "2024-12-31",
#'   cols = c("estudio", "fecha", "genero", "edad", "idv", "recuerdo")
#' )
#'
#' cis_read(cols = dplyr::starts_with("val_"))
#'
#' cis_read(fecha_min = "2020-01-01", collect = FALSE) |>
#'   dplyr::count(estudio) |>
#'   dplyr::collect()
#' }
cis_read <- function(fecha_min = NULL,
                     fecha_max = NULL,
                     estudios = NULL,
                     cols = NULL,
                     keep_core_cols = TRUE,
                     collect = TRUE) {
    cols <- rlang::enquo(cols)
    cis_check_bool(keep_core_cols, "keep_core_cols")
    cis_check_bool(collect, "collect")

    fecha_min <- cis_parse_date(fecha_min, "fecha_min")
    fecha_max <- cis_parse_date(fecha_max, "fecha_max")
    if (!is.null(fecha_min) && !is.null(fecha_max) && fecha_min > fecha_max) {
        cli::cli_abort("{.arg fecha_min} cannot be later than {.arg fecha_max}.")
    }

    ds <- cis_open()
    available_cols <- names(ds)
    selected_cols <- cis_selected_cols(cols, keep_core_cols, available_cols)

    query <- ds
    if (!is.null(fecha_min)) {
        query <- dplyr::filter(query, .data$fecha >= !!fecha_min)
    }
    if (!is.null(fecha_max)) {
        query <- dplyr::filter(query, .data$fecha <= !!fecha_max)
    }
    if (!is.null(estudios)) {
        estudios <- as.character(estudios)
        query <- dplyr::filter(query, .data$estudio %in% !!estudios)
    }
    if (!is.null(selected_cols)) {
        query <- dplyr::select(query, dplyr::all_of(selected_cols))
    }

    if (collect) {
        dplyr::collect(query)
    } else {
        query
    }
}

cis_selected_cols <- function(cols, keep_core_cols, available_cols) {
    if (rlang::quo_is_missing(cols) || identical(rlang::quo_get_expr(cols), NULL)) {
        return(NULL)
    }
    selected <- cis_eval_cols(cols, available_cols)
    if (keep_core_cols) {
        selected <- unique(c(.cis_core_cols, selected))
    }
    missing <- setdiff(selected, available_cols)
    if (length(missing) > 0L) {
        cli::cli_abort(c(
            "Some requested columns are not available in the CIS dataset.",
            "x" = "Missing: {paste(missing, collapse = ', ')}",
            "i" = "Use {.code cis_cols()} to list available columns."
        ))
    }
    selected
}

cis_eval_cols <- function(cols, available_cols) {
    expr <- rlang::quo_get_expr(cols)
    value <- NULL
    if (is.character(expr) || rlang::is_symbol(expr)) {
        value <- tryCatch(
            rlang::eval_tidy(cols),
            error = function(e) NULL
        )
    }
    if (is.character(value)) {
        if (anyNA(value)) {
            cli::cli_abort("{.arg cols} must not contain missing values.")
        }
        return(unique(value))
    }

    data <- stats::setNames(rep(list(logical()), length(available_cols)), available_cols)
    selected <- tryCatch(
        tidyselect::eval_select(cols, data = data, allow_rename = FALSE),
        error = function(e) {
            cli::cli_abort(c(
                "Could not evaluate {.arg cols} as a tidyselect expression.",
                "x" = conditionMessage(e),
                "i" = "Use column names, helpers such as {.code dplyr::starts_with()}, or {.code dplyr::all_of()}."
            ))
        }
    )
    names(selected)
}

#' List available CIS columns
#'
#' @return A character vector with column names.
#' @export
cis_cols <- function() {
    names(cis_open())
}

#' Return the CIS schema
#'
#' Uses the local manifest when it includes a schema. Otherwise it falls back to
#' the Arrow schema in the local Parquet file.
#'
#' @return A tibble with at least `name` and `type`.
#' @export
cis_schema <- function() {
    manifest <- cis_manifest_local()
    schema <- manifest[["schema"]]
    if (is.list(schema) && length(schema) > 0L) {
        rows <- lapply(schema, function(x) {
            if (!is.list(x)) {
                return(NULL)
            }
            tibble::tibble(
                name = as.character(x[["name"]] %||% NA_character_),
                type = as.character(x[["type"]] %||% NA_character_),
                description = as.character(x[["description"]] %||% NA_character_)
            )
        })
        rows <- Filter(Negate(is.null), rows)
        if (length(rows) > 0L) {
            return(dplyr::bind_rows(rows))
        }
    }

    ds <- cis_open()
    tibble::tibble(
        name = names(ds),
        type = vapply(ds$schema$fields, function(x) x$type$ToString(), character(1)),
        description = NA_character_
    )
}

#' List studies available in the local CIS dataset
#'
#' @return A tibble with `estudio`, `fecha`, `anio`, and `mes`.
#' @export
cis_studies <- function() {
    df <- cis_open() |>
        dplyr::select(dplyr::all_of(c("estudio", "fecha"))) |>
        dplyr::distinct() |>
        dplyr::arrange(.data$fecha, .data$estudio) |>
        dplyr::collect()

    df$anio <- as.integer(format(df$fecha, "%Y"))
    df$mes <- as.integer(format(df$fecha, "%m"))
    tibble::as_tibble(df)
}
