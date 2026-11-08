# Use preconstructed fixed-reference balance profiles; unknown debt is not zero.
debt_trajectories <- function(con, output) {
  assert_sql(con, "SELECT count(*)=count(DISTINCT(accountday_id,horizon_days)) FROM ap", "Duplicate trajectory keys.")
  execute(con, "CREATE TEMP TABLE ap_slim AS SELECT accountday_id,wallet,reference_date,
    borrow_usd_known,horizon_days,total_debt_baseline_fixed_usd baseline,
    total_debt_endpoint_fixed_usd endpoint,total_debt_delta_fixed_usd delta,
    greatest(1.0,.01*borrow_usd_known) tolerance,
    complete_observation AND total_debt_baseline_fixed_usd IS NOT NULL
    AND total_debt_endpoint_fixed_usd IS NOT NULL AND borrow_amount_unknown_events=0 AS valid
    FROM ap WHERE horizon_days IN (0,1,7,28)")
  assert_sql(con, "SELECT count(*)=0 FROM ap_slim WHERE valid AND (NOT isfinite(baseline) OR NOT isfinite(endpoint) OR delta IS NULL OR NOT isfinite(delta) OR borrow_usd_known IS NULL OR NOT isfinite(borrow_usd_known) OR abs(endpoint-baseline-delta)>0.01)", "Invalid fixed-reference balances.")
  save_result(output, "trajectory_coverage", query(con, "SELECT horizon_days,count(*) candidate_accountdays,
    count(*) FILTER(WHERE valid) valid_accountdays,sum(borrow_usd_known) candidate_borrow_usd,
    sum(borrow_usd_known) FILTER(WHERE valid) valid_borrow_usd FROM ap_slim GROUP BY 1 ORDER BY 1"))
  execute(con, "CREATE TEMP TABLE balanced AS SELECT accountday_id FROM ap_slim GROUP BY 1
    HAVING count(*)=4 AND bool_and(coalesce(valid,FALSE))")
  execute(con, "CREATE TEMP TABLE traj AS SELECT a.*,delta>tolerance AS grows FROM ap_slim a JOIN balanced USING(accountday_id)")
  save_result(output, "balanced_trajectory", query(con, "SELECT count(*) accountdays,
    count(DISTINCT wallet) wallets,min(reference_date) first_date,max(reference_date) last_date,
    sum(borrow_usd_known)/(SELECT sum(borrow_usd_reference) FROM e) borrow_coverage_fraction FROM traj WHERE horizon_days=0"))
  save_result(output, "trajectory_by_prior_frequency", query(con, "SELECT a.frequency_group AS label,
    t.horizon_days,count(*) accountdays,count(DISTINCT t.wallet) wallets,
    sum(borrow_usd_known) borrowing_on_days_usd,avg(grows::INTEGER) net_expansion_day_fraction,
    avg((delta< -tolerance)::INTEGER) net_reduction_day_fraction,
    avg((abs(delta)<=tolerance)::INTEGER) near_baseline_day_fraction
    FROM traj t JOIN activity_bins a ON t.wallet=a.wallet
    AND CAST(date_trunc('week',t.reference_date) AS DATE)=a.week_start GROUP BY 1,2 ORDER BY 1,2"))
  save_result(output, "trajectory_frequency_sample_flow", query(con, "SELECT
    count(*) balanced_accountdays,count(*) FILTER(WHERE a.wallet IS NOT NULL) frequency_eligible_accountdays,
    count(*) FILTER(WHERE a.wallet IS NULL) outside_frequency_window,
    min(t.reference_date) FILTER(WHERE a.wallet IS NULL) excluded_first_date,
    max(t.reference_date) FILTER(WHERE a.wallet IS NULL) excluded_last_date
    FROM traj t LEFT JOIN activity_bins a ON t.wallet=a.wallet
    AND CAST(date_trunc('week',t.reference_date) AS DATE)=a.week_start WHERE horizon_days=0"))
}
