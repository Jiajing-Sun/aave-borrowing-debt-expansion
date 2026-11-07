#!/usr/bin/env Rscript
# Small synthetic accounting cases; no research dataset is bundled or downloaded.
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
repo <- dirname(normalizePath(script, mustWork = TRUE))
source(file.path(repo, "config.R"))
source(file.path(repo, "transaction_analysis.R"))
source(file.path(repo, "weekly_analysis.R"))
require_packages(c("DBI", "duckdb", "jsonlite"))

self_test <- function() {
  con <- open_database()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  output <- tempfile("aave_synthetic_")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  # A: same transaction; B: later repayment-only transaction; C: currency switch;
  # D: repayment before borrowing; E: nearly equal borrowing then repayment.
  f <- data.frame(event_id = paste0("event", 1:11),
    transaction_hash = c("a", "a", "b1", "b2", "c", "c", "d", "d", "e", "e", "history"),
    economic_aave_account = c(rep(LETTERS[1:5], each = 2), "A"),
    reserve_address = c("X", "X", "X", "X", "X", "Y", "X", "X", "X", "X", "X"),
    date_utc = as.Date(c(rep("2023-02-27", 10), "2023-02-20")),
    log_index = c(1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1),
    action_type = c(rep(c("borrow", "repay"), 5), "borrow"),
    amount_raw_decimal = c(100, 80, 100, 80, 100, 100, 100, 100, 100, 99.95, 5),
    asset_decimals = 0L)
  DBI::dbWriteTable(con, "f", f)
  DBI::dbWriteTable(con, "p", data.frame(reserve = c("X", "Y", "X"),
    date_utc = as.Date(c("2023-02-27", "2023-02-27", "2023-02-20")), price_usd = 1))
  DBI::dbWriteTable(con, "l", data.frame(economic_aave_account = character(), transaction_hash = character()))
  execute(con, "CREATE TABLE e AS SELECT event_id borrow_event_id,economic_aave_account wallet,
    transaction_hash,date_utc,CAST(date_trunc('week',date_utc) AS DATE) week_start,
    amount_raw_decimal borrow_usd_reference,FALSE strict_debt_refinancing,FALSE original_has_flash
    FROM f WHERE action_type='borrow'")
  transaction_analysis(con, output)
  a <- query(con, "SELECT * FROM daycalc WHERE wallet='A' AND date_utc=DATE '2023-02-27'")
  b <- query(con, "SELECT * FROM daycalc WHERE wallet='B'")
  c <- query(con, "SELECT * FROM txcalc WHERE wallet='C'")
  stopifnot(a$tx_overlap == 80, a$additional_cross_tx_overlap == 0, a$positive_net_day_draw == 20,
    b$tx_overlap == 0, b$additional_cross_tx_overlap == 80, b$positive_net_day_draw == 20,
    c$same_asset_overlap == 0, c$cross_asset_overlap == 100,
    scalar(con, "SELECT prior_n FROM activity_bins WHERE wallet='A'") == 1)
  rounds <- jsonlite::fromJSON(file.path(output, "single_asset_atomic_roundtrips.json"))
  stopifnot(rounds$account_transactions == 1) # E qualifies; D's event order fails.

  # Within-cell slopes must match an explicit dummy-variable regression.
  set.seed(2511)
  g <- rep(1:12, each = 20)
  x <- matrix(rnorm(480), ncol = 2)
  y <- 0.7 * x[, 1] - 0.2 * x[, 2] + g / 4 + rnorm(240, sd = 0.3)
  slope <- qr.solve(within_cells(x, g), within_cells(y, g))
  dummy <- coef(lm(y ~ x + factor(g)))[2:3]
  stopifnot(max(abs(slope - dummy)) < 1e-10)
  message("Synthetic transaction accounting, event ordering, historical frequency, and within-cell OLS checks passed.")
}
self_test()
