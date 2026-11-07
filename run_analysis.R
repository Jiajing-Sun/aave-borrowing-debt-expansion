#!/usr/bin/env Rscript
# Run from any working directory; input/output paths are explicit and external.
args_all <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args_all[grepl("^--file=", args_all)])
if (length(script) != 1L) stop("Run this file with Rscript.")
repo <- dirname(normalizePath(script, mustWork = TRUE))
source(file.path(repo, "config.R"))
require_packages(c("DBI", "duckdb", "jsonlite", "Rmpfr"))
paths <- parse_paths(commandArgs(trailingOnly = TRUE), repo)
for (file in c("transaction_analysis.R", "debt_trajectories.R", "weekly_analysis.R", "validate_results.R", "make_exhibits.R")) source(file.path(repo, file))

main <- function() {
  con <- open_database()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  save_result(paths$output, "run_status", list(status = "Started; results may be incomplete until all stages finish."))
  manifest <- register_inputs(con, paths$data)
  save_result(paths$output, "input_manifest", manifest)
  transaction_analysis(con, paths$output)
  debt_trajectories(con, paths$output)
  weekly_analysis(con, paths$output)
  validate_results(con, paths$output)
  make_exhibits(paths$output)
  writeLines(capture.output(sessionInfo()), file.path(paths$output, "session_info.txt"))
  save_result(paths$output, "run_status", list(completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    status = "All analysis stages and internal validation completed."))
  message("Completed. Outputs: ", paths$output)
}
main()
