# cislongitudinal

[![R-CMD-check](https://github.com/hmeleiro/cislongitudinal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hmeleiro/cislongitudinal/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://img.shields.io/badge/CRAN-not%20on%20CRAN%20yet-lightgrey.svg)](https://CRAN.R-project.org/package=cislongitudinal)
[![Spain Electoral Project](https://img.shields.io/badge/Spain%20Electoral-Project-red?style=flat-square)](https://spainelectoralproject.com)

`cislongitudinal` lets you download, cache, update, inspect, and query the
longitudinal CIS dataset published by Spain Electoral Project as a Parquet file.
The package keeps the data outside your project directory and uses Arrow so
filters and column selection can run before data is collected into memory.

## Installation

```r
# install.packages("remotes")
remotes::install_github("hmeleiro/cislongitudinal")
```

## Download the dataset

```r
library(cislongitudinal)

cis_download()
```

The local file is stored in:

```r
cis_cache_dir()
```

By default this is the application data directory returned by:

```r
rappdirs::user_data_dir("cislongitudinal", "spainelectoralproject")
```

## Check the local copy

```r
cis_available()
cis_path()
cis_info()
cis_manifest()
```

## Update

```r
cis_update()
```

`cis_update()` reads the remote manifest and replaces the local Parquet only
after a successful download and validation.

## Read data

Filter by date:

```r
df <- cis_read(fecha_min = "2023-01-01")
```

Filter by date range:

```r
df <- cis_read(
  fecha_min = "2020-01-01",
  fecha_max = "2024-12-31"
)
```

Filter by study code:

```r
df <- cis_read(estudios = c(3420, 3421, 3422))
```

Select columns:

```r
df <- cis_read(
  fecha_min = "2023-01-01",
  cols = c("estudio", "fecha", "genero", "edad", "idv", "recuerdo")
)
```

When `keep_core_cols = TRUE`, `cis_read()` always keeps the core columns:
`estudio`, `fecha`, `genero`, and `edad`.

## Lazy queries

Use `collect = FALSE` to keep working lazily:

```r
df_lazy <- cis_read(
  fecha_min = "2020-01-01",
  collect = FALSE
)

df_lazy |>
  dplyr::count(estudio) |>
  dplyr::collect()
```

For advanced queries, open the local dataset directly:

```r
cis_open() |>
  dplyr::filter(fecha >= as.Date("2023-01-01")) |>
  dplyr::select(estudio, fecha, genero, edad) |>
  dplyr::collect()
```

## Explore columns and studies

```r
cis_cols()
cis_schema()
cis_studies()
```
