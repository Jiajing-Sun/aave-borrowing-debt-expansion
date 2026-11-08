# Borrowing Volume and Debt Expansion: Evidence from Aave V3

R analysis code for the research manuscript by Junda Wu and Jiajing Sun. The code asks how recorded borrowing relates to transaction-level offsets, positive daily net borrowing, and subsequent debt balances in Ethereum's Aave V3 Core market.

**This repository contains code only.** No raw or derived research data, wallet-level exports, manuscript files, or generated results are distributed here. All files are in the repository root. Comments and documentation are in English.

## Scope

This is an analysis-stage replication package. It starts from the project's archived Parquet event tables and prepared balance/weekly panels. It does **not** download Ethereum logs or reconstruct historical debt indices, legacy GHO balances, or the weekly estimation samples from scratch. The required inputs are listed below; they must be obtained separately. The repository alone is therefore insufficient for a complete empirical rerun.

The scripts implement the analysis in R, using SQL through the DuckDB R interface for large tables and base R for estimation and plotting. They do not invoke Python or read precomputed coefficients as results.

## Files

| File | Purpose |
| --- | --- |
| `run_analysis.R` | Run the analysis, validation, and exhibit generation. |
| `config.R` | Validate paths, connect to DuckDB, and register external inputs. |
| `transaction_analysis.R` | Rebuild transaction and daily borrowing/repayment components; construct prior-frequency groups and concentration/year checks. |
| `debt_trajectories.R` | Select a balanced four-horizon sample and summarize fixed-reference debt expansion. |
| `weekly_analysis.R` | Re-estimate weekly regressions and two-way clustered inference; reproduce flow/debt rankings. |
| `validate_results.R` | Recalculate deterministic transaction samples using 256-bit arithmetic. |
| `make_exhibits.R` | Generate Figure 1 and editable rows for the new accounting and trajectory tables. |
| `self_test.R` | Test synthetic timing, currency-switching, history, and fixed-effect cases without research data. |

## Run

Use R 4.5 or newer. Install the required packages from CRAN:

```r
install.packages(c("DBI", "duckdb", "jsonlite", "Rmpfr"))
```

`Rmpfr` requires MPFR/GMP libraries when installed from source; use CRAN binaries where available. No package installation or data download occurs automatically when running the scripts.

From a terminal:

```sh
Rscript self_test.R
Rscript run_analysis.R --data-dir /path/to/aave-inputs --output-dir /path/to/aave-results
```

The command works from any current directory when given the full path to the script. Keep inputs and outputs in separate directories outside this repository. Existing output files with the same names are replaced. DuckDB uses two threads and a 2 GB database memory limit; R's regression allocations are additional. Allow several GB of available memory and temporary disk space.

## Required external inputs

Place these eight files directly in the input directory. Column names are case-sensitive. The first four are archived event/price tables; the remaining four are derived inputs.

| File | Required columns |
| --- | --- |
| `fact_pool_action.parquet` | `event_id`, `action_type`, `transaction_hash`, `economic_aave_account`, `reserve_address`, `date_utc`, `log_index`, `amount_raw_decimal`, `asset_decimals` |
| `daily_aave_oracle_prices_v2.parquet` | `reserve`, `date_utc`, `price_usd` |
| `fact_liquidation_call.parquet` | `economic_aave_account`, `transaction_hash` |
| `event_bridge.parquet` | `borrow_event_id`, `wallet`, `transaction_hash`, `date_utc`, `week_start`, `borrow_usd_reference`, `strict_debt_refinancing`, `original_has_flash` |
| `accountday_position_profiles.parquet` | `accountday_id`, `wallet`, `reference_date`, `borrow_usd_known`, `borrow_amount_unknown_events`, `horizon_days`, `complete_observation`, `total_debt_baseline_fixed_usd`, `total_debt_endpoint_fixed_usd`, `total_debt_delta_fixed_usd` |
| `primary_sample.parquet` | `wallet`, `reserve`, `week_start`, `h`, `size_decade`, `D0_fixed`, `B_fixed`, `E_right_fixed`, `prior_B`, `past_events`, `prior_W` |
| `overlap_restricted_sample.parquet` | Same columns as `primary_sample.parquet`. |
| `address_year_flow_debt.parquet` | `year`, `wallet`, `B`, `debt_dollar_days` |

### Input conventions

- Event actions are lowercase `borrow` and `repay`. The economic account is the debt owner, not necessarily the sender. Amounts are raw integer token units; use a decimal/text-compatible representation that preserves the integer. Reserve/account keys must be consistently normalized. Dates are UTC, and week starts are Mondays.
- Oracle prices have one row per reserve/date. The bridge has one row per ordinary Borrow event. Its flash-loan and strict-refinancing flags are supplied metadata, not inferred loan-purpose labels. This code validates its event keys and dollar amounts against the action table.
- Balance profiles contain horizons 0, 1, 7, and 28. Baseline debt is at the previous day's close. Both endpoints use the borrowing date's end-of-day prices and debt indices. The upstream `complete_observation` flag must reject incomplete windows and incompatible debt states, including incompatible GHO/stable-rate states. Missing totals must remain missing. Profile construction is an upstream requirement, not performed here.
- Weekly samples retain recent borrowers with opening debt at least USD 100 and at least five observations of each `h` state in a week/asset/opening-debt-decade cell. `h` marks a prior four-week repayment-offset ratio of at least 0.8. Prior borrowing/count histories cover all assets; opening debt and outcomes cover DAI, USDC, USDT, WETH, and wstETH at fixed 5 January 2025 prices. `E_right_fixed` is the sum of seven daily balances, not their average. The code recomputes all logarithms and active-week indicators. It retains the supplied `h` classification, including any upstream numerical-boundary handling.
- The overlap sample is prepared upstream using the intersection of both states' observed prior borrowing-dollar/count ranges and common active-week values within each cell, followed by the same minimum-five support rule. This package estimates from those prepared rows; it does not recreate the restrictions.
- Annual ranking inputs use the original filtered complete-week panel and weekly opening prices. Debt-dollar-days are sums of daily balances over the common observation window. Their sample and price convention differ from the full event sample.

## Definitions and interpretation

Within a transaction and debt account, gross borrowing `B` equals `min(B, R) + max(B - R, 0)`. Same-asset offsets are the sum of asset-specific minima; the remainder of the total offset is cross-asset. Within a day, the decomposition separates transaction offsets, additional offsets across transactions, and positive daily net borrowing. Repayment-only transactions enter daily totals.

Frequency groups use Borrow event counts in the preceding four complete UTC weeks, without using repayment. Eligible outcome week starts run from 27 February 2023 to 29 June 2026. Several Borrow events in one transaction count separately. Top-ten exclusions use full-sample gross borrowing with deterministic address tie breaking.

Debt expansion exceeds `max(USD 1, 0.01 * reference-day borrowing)`. All four horizons must be observed for an account-day to enter the balanced sample. Flow shares are dollar-weighted ratios of sums; trajectory rates give account-days equal weight. Later borrowing affects endpoints, so these are not individual-loan survival rates.

The weekly regressions absorb week/asset/opening-debt-bin cells. M1 includes `h` and log opening debt; M2 adds log one plus prior borrowing dollars/counts and indicators for two, three, and four active weeks. Two-way CR1 inference adds account and week covariance components and subtracts their intersection. Each component uses `G/(G-1) * (N-1)/(N-K)`, where `K` includes cell effects and slope rank. Confidence intervals use `min(account clusters, week clusters) - 1` degrees of freedom. Negative variance estimates are not silently repaired.

Accounting overlap does not establish that a draw funded a repayment or identify refinancing, welfare, or loan purpose. Borrowing repaid within the day can provide useful liquidity. Summing positive account-day flows does not measure market-wide net credit creation. The comparisons are descriptive; addresses are not identified individuals, and exposure outside Aave V3 Core is not observed.

## Outputs and validation

Results are written only to the external output directory as CSV/JSON summaries, a vector PDF, and editable LaTeX table rows. `aggregate_daily_flows` reports the full-event-sample daily decomposition, `frequency_group_weights` reports each frequency group's borrowing-dollar and account-day shares, and `trajectory_frequency_sample_flow` reconciles the balanced and frequency-eligible trajectory samples. Figure 1 and the main trajectory table include all four groups, including zero prior draws. The regression CSV contains all estimated coefficients and inference; the script does not format the original regression table. The input manifest records file names, sizes, and MD5 checksums for local reproducibility. Session information records package versions.

Checks include unique event/price keys, complete pricing, event-value reconciliation, transaction/daily identities, balanced trajectory keys, weekly support, and regression normal equations. A deterministic sample combines the twelve largest offsets with twenty-four transaction/account keys ordered by an MD5 hash. Raw token integers are read as text and reconstructed with 256-bit arithmetic; differences must be below USD 0.001. These checks validate computations on the supplied archive, not the correctness of every upstream chain extraction.

The `.gitignore` uses an explicit source-file allowlist. This is a guard against accidentally committing inputs and outputs; it is not a substitute for reviewing staged files before publication.
