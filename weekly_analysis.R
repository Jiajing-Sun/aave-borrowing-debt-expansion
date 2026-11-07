# Re-estimate the complementary regressions from prepared, validated weekly samples.
# The event-flow sample and weekly samples have different denominators and prices.
group_codes <- function(frame, fields) {
  key <- do.call(paste, c(lapply(frame[fields], as.character), sep = "|"))
  match(key, unique(key))
}
within_cells <- function(z, groups) {
  z <- as.matrix(z)
  totals <- rowsum(z, groups, reorder = TRUE)
  means <- totals / tabulate(groups)
  z - means[groups, , drop = FALSE]
}
cluster_covariance <- function(scores, groups, bread, n, k) {
  summed <- rowsum(scores, groups, reorder = TRUE)
  g <- nrow(summed)
  if (g <= 1 || n <= k) stop("Insufficient clusters or residual degrees of freedom.")
  (g / (g - 1)) * ((n - 1) / (n - k)) * bread %*% crossprod(summed) %*% bread
}

weekly_analysis <- function(con, output) {
  results <- list()
  diagnostics <- list()
  descriptives <- list()
  for (sample in c("primary", "overlap_restricted")) {
    d <- query(con, paste0("SELECT wallet,reserve,week_start,h,size_decade,D0_fixed,
      B_fixed,E_right_fixed,prior_B,past_events,prior_W FROM ", DBI::dbQuoteIdentifier(con, sample)))
    if (anyDuplicated(d[c("wallet", "reserve", "week_start")])) stop("Duplicate weekly sample keys.")
    numeric_fields <- c("h", "size_decade", "D0_fixed", "B_fixed", "E_right_fixed", "prior_B", "past_events", "prior_W")
    stopifnot(all(is.finite(as.matrix(d[numeric_fields]))), all(d$h %in% 0:1), all(d$D0_fixed >= 100),
      all(d$B_fixed >= 0), all(d$E_right_fixed >= 0), all(d$prior_B > 0),
      all(d$past_events >= 1), all(d$prior_W %in% 1:4),
      all(d$size_decade == pmin(8, floor(log10(d$D0_fixed)))))
    # Recompute transformations rather than rely on saved log columns.
    d$logD <- log(d$D0_fixed)
    d$logPB <- log1p(d$prior_B)
    d$logPN <- log1p(d$past_events)
    d$logBorrow <- log1p(d$B_fixed)
    d$logDebt <- log1p(d$E_right_fixed / 7)
    for (j in 2:4) d[[paste0("PW", j)]] <- as.integer(d$prior_W == j)
    fe <- group_codes(d, c("week_start", "reserve", "size_decade"))
    address <- group_codes(d, "wallet")
    week <- group_codes(d, "week_start")
    intersection <- group_codes(d, c("wallet", "week_start"))
    support <- table(fe, factor(d$h, levels = 0:1))
    stopifnot(all(support >= 5))
    n <- nrow(d)
    for (h in 0:1) descriptives[[length(descriptives) + 1L]] <- data.frame(sample = sample, h = h,
      observations = sum(d$h == h), mean_opening_debt = mean(d$D0_fixed[d$h == h]),
      mean_average_daily_debt = mean(d$E_right_fixed[d$h == h]) / 7)
    for (model in c("M1", "M2")) {
      terms <- c("h", "logD", if (model == "M2") c("logPB", "logPN", "PW2", "PW3", "PW4"))
      x <- within_cells(d[terms], fe)
      qr_x <- qr(x)
      stopifnot(qr_x$rank == length(terms))
      k <- max(fe) + qr_x$rank
      bread <- solve(crossprod(x))
      for (outcome in c("logBorrow", "logDebt")) {
        y <- drop(within_cells(d[outcome], fe))
        beta <- drop(qr.coef(qr_x, y))
        residual <- drop(y - x %*% beta)
        scores <- x * residual
        covariance <- cluster_covariance(scores, address, bread, n, k) +
          cluster_covariance(scores, week, bread, n, k) -
          cluster_covariance(scores, intersection, bread, n, k)
        covariance <- (covariance + t(covariance)) / 2
        stopifnot(all(diag(covariance) > 0))
        se <- sqrt(diag(covariance))
        df <- min(max(address), max(week)) - 1
        critical <- qt(0.975, df)
        normal_error <- max(abs(crossprod(x, residual))) / n
        stopifnot(normal_error < 1e-7)
        results[[length(results) + 1L]] <- data.frame(sample = sample, model = model,
          outcome = outcome, term = terms, coefficient = beta, se = se,
          ci_low = beta - critical * se, ci_high = beta + critical * se,
          p_value = 2 * pt(-abs(beta / se), df), N = n, addresses = max(address),
          weeks = max(week), FE_cells = max(fe), K = k, t_df = df,
          R2 = 1 - sum(residual^2) / sum((d[[outcome]] - mean(d[[outcome]]))^2),
          within_R2 = 1 - sum(residual^2) / sum(y^2), row.names = NULL)
        diagnostics[[length(diagnostics) + 1L]] <- data.frame(sample = sample, model = model,
          outcome = outcome, normal_equation_error_per_row = normal_error,
          minimum_covariance_eigenvalue = min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values))
        message(sample, " ", model, " ", outcome, ": H = ", round(beta[1], 6), ", SE = ", round(se[1], 6))
      }
    }
  }
  save_result(output, "weekly_regressions", do.call(rbind, results))
  save_result(output, "weekly_estimation_checks", do.call(rbind, diagnostics))
  save_result(output, "weekly_descriptives", do.call(rbind, descriptives))

  # Rank by summed borrowing and debt-dollar-days with deterministic tie breaking.
  annual <- query(con, "SELECT * FROM annual")
  stopifnot(!anyDuplicated(annual[c("wallet", "year")]), all(is.finite(annual$B)),
    all(is.finite(annual$debt_dollar_days)), all(annual$B >= 0), all(annual$debt_dollar_days >= 0))
  ranking <- function(d, period) {
    a <- aggregate(d[c("B", "debt_dollar_days")], list(wallet = d$wallet), sum)
    a <- a[a$B > 0 | a$debt_dollar_days > 0, ]
    top_b <- head(a$wallet[order(-a$B, a$wallet)], 100)
    top_d <- head(a$wallet[order(-a$debt_dollar_days, a$wallet)], 100)
    only <- a$wallet %in% setdiff(top_b, top_d)
    data.frame(period = as.character(period), overlap = length(intersect(top_b, top_d)),
      borrowing_only_accounts = sum(only), positive_accounts = nrow(a),
      total_borrow_usd = sum(a$B), borrowing_only_borrow_share = sum(a$B[only]) / sum(a$B),
      borrowing_only_debt_share = sum(a$debt_dollar_days[only]) / sum(a$debt_dollar_days))
  }
  rankings <- list(ranking(annual, "full_history"))
  for (year in sort(unique(annual$year))) rankings[[length(rankings) + 1L]] <- ranking(annual[annual$year == year, ], year)
  save_result(output, "flow_debt_rankings", do.call(rbind, rankings))
}
