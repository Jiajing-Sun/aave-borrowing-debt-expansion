# Reconcile ordinary Borrow and Repay events within debt accounts.
# SQL runs through the DuckDB R interface; no Python process is required.
transaction_analysis <- function(con, output) {
  assert_sql(con, "SELECT count(*)=count(DISTINCT borrow_event_id) FROM e", "Duplicate Borrow bridge keys.")
  assert_sql(con, "SELECT count(*)=0 FROM (SELECT reserve,date_utc,count(*) n FROM p GROUP BY 1,2 HAVING n<>1)", "Duplicate oracle keys.")
  assert_sql(con, "SELECT count(*)=0 FROM f WHERE action_type IN ('borrow','repay') AND (economic_aave_account IS NULL OR transaction_hash IS NULL OR date_utc IS NULL OR amount_raw_decimal IS NULL OR asset_decimals IS NULL)", "Incomplete event keys or amounts.")
  execute(con, "CREATE TEMP TABLE actions AS SELECT f.event_id,f.transaction_hash tx,
    f.economic_aave_account wallet,f.reserve_address reserve,f.date_utc,f.log_index,f.action_type,
    CAST(f.amount_raw_decimal AS DOUBLE)/power(10.0,f.asset_decimals)*p.price_usd usd
    FROM f LEFT JOIN p ON f.reserve_address=p.reserve AND f.date_utc=p.date_utc
    WHERE f.action_type IN ('borrow','repay')")
  assert_sql(con, "SELECT count(*)=0 FROM actions WHERE usd IS NULL OR NOT isfinite(usd) OR usd<0", "Missing/invalid priced actions; do not zero-fill.")
  assert_sql(con, "SELECT count(*)=count(DISTINCT event_id) FROM actions", "Duplicate action identifiers.")
  assert_sql(con, "SELECT count(*)=0 FROM (SELECT * FROM actions WHERE action_type='borrow') a FULL JOIN e ON a.event_id=e.borrow_event_id WHERE a.event_id IS NULL OR e.borrow_event_id IS NULL OR a.wallet<>e.wallet OR a.tx<>e.transaction_hash OR a.date_utc<>e.date_utc OR e.borrow_usd_reference IS NULL OR abs(a.usd-e.borrow_usd_reference)>=0.01", "Borrow bridge does not reconcile.")
  assert_sql(con, "SELECT count(*)=0 FROM e WHERE CAST(date_trunc('week',date_utc) AS DATE)<>CAST(week_start AS DATE) OR week_start IS NULL", "Inconsistent UTC week keys.")
  execute(con, "CREATE TEMP TABLE asset_tx AS SELECT tx,wallet,reserve,
    sum(CASE WHEN action_type='borrow' THEN usd ELSE 0 END) b,
    sum(CASE WHEN action_type='repay' THEN usd ELSE 0 END) r FROM actions GROUP BY 1,2,3")
  execute(con, "CREATE TEMP TABLE txmeta AS SELECT transaction_hash tx,wallet,
    first(date_utc) date_utc,count(*) borrow_events,sum(borrow_usd_reference) gross_b,
    sum(CASE WHEN strict_debt_refinancing THEN borrow_usd_reference ELSE 0 END) certified_refi_b,
    bool_or(original_has_flash) has_flash FROM e GROUP BY 1,2")
  execute(con, "CREATE TEMP TABLE txcalc AS WITH totals AS
    (SELECT tx,wallet,sum(b) b,sum(r) r,sum(least(b,r)) same_asset_overlap FROM asset_tx GROUP BY 1,2)
    SELECT m.*,x.r,x.same_asset_overlap,least(x.b,x.r) offset_usd,
    greatest(x.b-x.r,0) positive_net_draw_usd,
    least(x.b,x.r)-x.same_asset_overlap cross_asset_overlap FROM totals x JOIN txmeta m USING(tx,wallet)")
  assert_sql(con, "SELECT max(abs(gross_b-offset_usd-positive_net_draw_usd))<0.01 AND min(cross_asset_overlap)>-0.01 FROM txcalc", "Transaction identity failed.")
  execute(con, "CREATE TEMP TABLE wallet_rank AS SELECT wallet,row_number() OVER(ORDER BY sum(gross_b) DESC,wallet) rank_b FROM txcalc GROUP BY wallet")
  transaction_summary <- function(where = "TRUE") query(con, paste0("SELECT 'ALL' AS label,
    count(*) account_transactions,count(DISTINCT wallet) wallets,sum(borrow_events) borrow_events,
    sum(gross_b) gross_borrow_usd,sum(offset_usd)/sum(gross_b) offset_fraction,
    sum(same_asset_overlap)/sum(gross_b) same_asset_offset_fraction,
    sum(cross_asset_overlap)/sum(gross_b) cross_asset_offset_fraction,
    sum(positive_net_draw_usd)/sum(gross_b) positive_net_draw_fraction,
    sum(certified_refi_b)/sum(gross_b) certified_refi_borrow_fraction FROM txcalc WHERE ", where))
  save_result(output, "same_transaction_summary", transaction_summary())
  save_result(output, "same_transaction_exclude_top10", transaction_summary("wallet IN (SELECT wallet FROM wallet_rank WHERE rank_b>10)"))
  save_result(output, "single_asset_atomic_roundtrips", query(con, "WITH a AS (
    SELECT tx,wallet,count(*) FILTER(WHERE action_type='borrow') nb,
    count(*) FILTER(WHERE action_type='repay') nr,count(DISTINCT reserve) assets,
    min(log_index) FILTER(WHERE action_type='borrow') bl,
    min(log_index) FILTER(WHERE action_type='repay') rl FROM actions GROUP BY 1,2)
    SELECT x.has_flash,count(*) account_transactions,count(DISTINCT x.wallet) wallets,
    sum(x.gross_b) gross_borrow_usd,sum(x.gross_b)/(SELECT sum(gross_b) FROM txcalc) fraction_all_gross
    FROM txcalc x JOIN a USING(tx,wallet) WHERE nb=1 AND nr=1 AND assets=1 AND bl<rl
    AND abs(x.gross_b-x.r)<=.001*x.gross_b GROUP BY 1 ORDER BY 1"))
  save_result(output, "atomic_offset_concentration", query(con, "WITH w AS
    (SELECT wallet,sum(same_asset_overlap) amount FROM txcalc GROUP BY 1), ranked AS
    (SELECT *,row_number() OVER(ORDER BY amount DESC,wallet) rn FROM w)
    SELECT sum(amount) FILTER(WHERE rn<=10)/sum(amount) top10_share FROM ranked"))

  # Repayment-only transactions enter daily totals before selecting borrowing days.
  execute(con, "CREATE TEMP TABLE alltx AS SELECT tx,wallet,first(date_utc) date_utc,
    sum(CASE WHEN action_type='borrow' THEN usd ELSE 0 END) b,
    sum(CASE WHEN action_type='repay' THEN usd ELSE 0 END) r FROM actions GROUP BY 1,2")
  execute(con, "CREATE TEMP TABLE daycalc AS SELECT wallet,date_utc,
    CAST(date_trunc('week',date_utc) AS DATE) week_start,sum(b) gross_b,sum(r) r,
    sum(least(b,r)) tx_overlap,least(sum(b),sum(r))-sum(least(b,r)) additional_cross_tx_overlap,
    greatest(sum(b)-sum(r),0) positive_net_day_draw FROM alltx GROUP BY 1,2 HAVING sum(b)>0")
  assert_sql(con, "SELECT min(additional_cross_tx_overlap)>-0.01 AND max(abs(gross_b-tx_overlap-additional_cross_tx_overlap-positive_net_day_draw))<0.01 FROM daycalc", "Daily identity failed.")
  save_result(output, "aggregate_daily_flows", query(con, "SELECT count(*) accountdays,
    sum(gross_b) gross_borrow_usd,sum(tx_overlap)/sum(gross_b) same_tx_offset_fraction,
    sum(additional_cross_tx_overlap)/sum(gross_b) extra_daily_offset_fraction,
    sum(positive_net_day_draw)/sum(gross_b) positive_net_day_draw_fraction FROM daycalc"))
  execute(con, "CREATE TEMP TABLE activity_bins AS WITH w AS (
    SELECT wallet,CAST(week_start AS DATE) week_start,count(*) n FROM e GROUP BY 1,2), a AS (
    SELECT *,coalesce(sum(n) OVER(PARTITION BY wallet ORDER BY week_start RANGE BETWEEN INTERVAL 28 DAY PRECEDING AND INTERVAL 1 DAY PRECEDING),0) prior_n FROM w)
    SELECT *,CASE WHEN prior_n=0 THEN '0' WHEN prior_n<=2 THEN '1-2'
    WHEN prior_n<=9 THEN '3-9' ELSE '10+' END frequency_group FROM a
    WHERE week_start>=DATE '2023-02-27' AND week_start<=DATE '2026-06-29'")
  save_result(output, "daily_decomposition_by_prior_frequency", query(con, "SELECT frequency_group,
    count(*) accountdays,count(DISTINCT d.wallet) wallets,sum(gross_b) gross_borrow_usd,
    sum(tx_overlap)/sum(gross_b) same_tx_offset_fraction,
    sum(additional_cross_tx_overlap)/sum(gross_b) extra_daily_offset_fraction,
    sum(positive_net_day_draw)/sum(gross_b) positive_net_day_draw_fraction
    FROM daycalc d JOIN activity_bins USING(wallet,week_start) GROUP BY 1 ORDER BY 1"))
  save_result(output, "prior_frequency_robustness", query(con, "SELECT frequency_group,count(*) accountdays,
    sum(gross_b) gross_borrow_usd,sum(positive_net_day_draw)/sum(gross_b) positive_net_day_draw_fraction,
    sum(positive_net_day_draw) FILTER(WHERE rank_b>10)/sum(gross_b) FILTER(WHERE rank_b>10) excluding_top10_fraction,
    sum(positive_net_day_draw) FILTER(WHERE gross_b>=100)/sum(gross_b) FILTER(WHERE gross_b>=100) minimum100_fraction
    FROM daycalc JOIN activity_bins USING(wallet,week_start) JOIN wallet_rank USING(wallet) GROUP BY 1 ORDER BY 1"))
  save_result(output, "prior_frequency_years", query(con, "SELECT year(date_utc) calendar_year,
    frequency_group,count(*) accountdays,sum(positive_net_day_draw)/sum(gross_b) positive_net_day_draw_fraction
    FROM daycalc JOIN activity_bins USING(wallet,week_start) GROUP BY 1,2 ORDER BY 1,2"))
  save_result(output, "event_audit", query(con, "SELECT count(*) borrow_events,count(DISTINCT wallet) wallets,
    min(date_utc) first_date,max(date_utc) last_date,sum(borrow_usd_reference) gross_borrow_usd,
    (SELECT count(*) FROM e JOIN l ON e.wallet=l.economic_aave_account AND e.transaction_hash=l.transaction_hash) liquidation_tx_account_overlap FROM e"))
}
