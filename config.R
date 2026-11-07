# Shared utilities. Data and generated outputs must remain outside this repository.
require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install these R packages first: ", paste(missing, collapse = ", "))
}

parse_paths <- function(args, repo) {
  if (length(args) != 4L || !setequal(args[c(1, 3)], c("--data-dir", "--output-dir"))) {
    stop("Usage: Rscript run_analysis.R --data-dir /path/to/inputs --output-dir /path/to/results")
  }
  values <- setNames(args[c(2, 4)], args[c(1, 3)])
  data <- normalizePath(values[["--data-dir"]], mustWork = TRUE)
  dir.create(values[["--output-dir"]], recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(values[["--output-dir"]], mustWork = TRUE)
  inside <- function(path, root) identical(path, root) || startsWith(path, paste0(root, "/"))
  if (inside(data, repo) || inside(output, repo)) stop("Keep data and outputs outside the code repository.")
  if (inside(output, data) || inside(data, output)) stop("Use separate input and output directories.")
  list(data = data, output = output)
}

open_database <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, "SET threads=2")
  DBI::dbExecute(con, "SET memory_limit='2GB'")
  # Inputs use UTC DATE/TIMESTAMP columns, not local TIMESTAMPTZ conversion.
  # Avoid requiring DuckDB's optional ICU extension solely to set a timezone.
  DBI::dbExecute(con, "SET preserve_insertion_order=false")
  con
}

register_inputs <- function(con, data) {
  inputs <- c(f = "fact_pool_action.parquet", p = "daily_aave_oracle_prices_v2.parquet",
    l = "fact_liquidation_call.parquet", e = "event_bridge.parquet",
    ap = "accountday_position_profiles.parquet", primary = "primary_sample.parquet",
    overlap_restricted = "overlap_restricted_sample.parquet",
    annual = "address_year_flow_debt.parquet")
  paths <- file.path(data, inputs)
  if (any(!file.exists(paths))) stop("Missing inputs: ", paste(inputs[!file.exists(paths)], collapse = ", "))
  for (i in seq_along(inputs)) DBI::dbExecute(con, paste0("CREATE VIEW ", DBI::dbQuoteIdentifier(con, names(inputs)[i]),
    " AS SELECT * FROM read_parquet(", DBI::dbQuoteString(con, paths[i]), ")"))
  data.frame(file = unname(inputs), bytes = file.info(paths)$size,
    md5 = unname(tools::md5sum(paths)), row.names = NULL)
}

query <- function(con, sql) {
  result <- DBI::dbGetQuery(con, sql)
  # Counts and years fit exactly in doubles; raw token integers are read as text.
  for (name in names(result)) if (inherits(result[[name]], "integer64")) result[[name]] <- as.numeric(result[[name]])
  result
}
execute <- function(con, sql) invisible(DBI::dbExecute(con, sql))
scalar <- function(con, sql) query(con, sql)[[1]][1]
assert_sql <- function(con, sql, message) {
  if (!isTRUE(as.logical(scalar(con, sql)))) stop(message)
}
save_result <- function(output, name, value) {
  jsonlite::write_json(value, file.path(output, paste0(name, ".json")),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, digits = 16, na = "null")
  if (is.data.frame(value)) write.csv(value, file.path(output, paste0(name, ".csv")), row.names = FALSE)
  message("Saved ", name)
  invisible(value)
}
