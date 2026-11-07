# Independent high-precision reconstruction from raw integer amounts for 36 keys.
validate_results <- function(con, output) {
  execute(con, "CREATE TEMP TABLE audit_sample AS
    (SELECT * FROM txcalc ORDER BY offset_usd DESC,tx LIMIT 12)
    UNION (SELECT * FROM txcalc ORDER BY md5(tx||wallet) LIMIT 24)")
  raw <- query(con, "SELECT a.transaction_hash tx,a.economic_aave_account wallet,
    a.reserve_address reserve,a.action_type,CAST(a.amount_raw_decimal AS VARCHAR) amount_text,
    a.asset_decimals,CAST(p.price_usd AS VARCHAR) price_text
    FROM f a JOIN audit_sample s ON a.transaction_hash=s.tx AND a.economic_aave_account=s.wallet
    JOIN p ON a.reserve_address=p.reserve AND a.date_utc=p.date_utc
    WHERE a.action_type IN ('borrow','repay')")
  sample <- query(con, "SELECT * FROM audit_sample")
  amount <- Rmpfr::mpfr(raw$amount_text, 256) / Rmpfr::mpfr(10, 256)^raw$asset_decimals *
    Rmpfr::mpfr(raw$price_text, 256)
  maximum_error <- 0
  for (i in seq_len(nrow(sample))) {
    sel <- raw$tx == sample$tx[i] & raw$wallet == sample$wallet[i]
    b <- sum(amount[sel & raw$action_type == "borrow"])
    r <- sum(amount[sel & raw$action_type == "repay"])
    same <- Rmpfr::mpfr(0, 256)
    for (asset in unique(raw$reserve[sel])) {
      a <- sel & raw$reserve == asset
      same <- same + min(sum(amount[a & raw$action_type == "borrow"]), sum(amount[a & raw$action_type == "repay"]))
    }
    expected <- c(as.numeric(b), as.numeric(r), as.numeric(same), as.numeric(min(b, r)), as.numeric(min(b, r) - same))
    actual <- as.numeric(sample[i, c("gross_b", "r", "same_asset_overlap", "offset_usd", "cross_asset_overlap")])
    maximum_error <- max(maximum_error, abs(actual - expected))
  }
  stopifnot(maximum_error < 0.001)
  valid_keys <- scalar(con, "SELECT bool_and(length(wallet)=42 AND length(tx)=66) FROM txcalc")
  stopifnot(isTRUE(valid_keys))
  save_result(output, "independent_verification", list(sample_account_transactions = nrow(sample),
    raw_borrow_repay_events = nrow(raw), high_precision_max_error_usd = maximum_error,
    all_address_and_hash_lengths_valid = valid_keys))
}
