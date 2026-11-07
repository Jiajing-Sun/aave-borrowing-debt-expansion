# Generate vector artwork and editable LaTeX table rows from computed results.
make_exhibits <- function(output) {
  read_result <- function(name) jsonlite::fromJSON(file.path(output, paste0(name, ".json")))
  percent <- function(x) sprintf("%.2f", 100 * x)
  write_rows <- function(name, rows) {
    writeLines(vapply(rows, function(row) paste0(paste(row, collapse = " & "), " \\\\"), character(1)),
      file.path(output, paste0(name, ".tex")))
  }
  groups <- c("0", "1-2", "3-9", "10+")
  names <- c("Zero", "One--two", "Three--nine", "Ten-plus")
  freq <- read_result("daily_decomposition_by_prior_frequency")
  trajectory <- read_result("trajectory_by_prior_frequency")
  rows <- lapply(seq_along(groups), function(i) {
    d <- trajectory[trajectory$label == groups[i], ]
    d <- d[match(c(0, 1, 7, 28), d$horizon_days), ]
    stopifnot(nrow(d) == 4, length(unique(d$accountdays)) == 1)
    c(names[i], format(d$accountdays[1], big.mark = ",", scientific = FALSE, trim = TRUE), percent(d$net_expansion_day_fraction))
  })
  write_rows("all_trajectory_rows", rows)
  write_rows("debt_trajectory_rows", rows[-1])
  a <- read_result("same_transaction_summary")
  e <- read_result("same_transaction_exclude_top10")
  fields <- c("offset_fraction", "same_asset_offset_fraction", "cross_asset_offset_fraction", "positive_net_draw_fraction")
  labels <- c("Within-transaction offset", "Same-asset component", "Additional cross-asset component", "Positive transaction net borrowing")
  rows <- list(c("Gross borrowing (USD billion)", sprintf("%.2f", a$gross_borrow_usd / 1e9), sprintf("%.2f", e$gross_borrow_usd / 1e9)))
  for (i in seq_along(fields)) rows[[i + 1L]] <- c(labels[i], percent(a[[fields[i]]]), percent(e[[fields[i]]]))
  write_rows("atomic_rows", rows)
  rob <- read_result("prior_frequency_robustness")
  rob <- rob[match(groups, rob$frequency_group), ]
  fields <- c("positive_net_day_draw_fraction", "excluding_top10_fraction", "minimum100_fraction")
  labels <- c("All eligible account-days", "Excluding top ten borrowers", "Daily borrowing at least USD 100")
  rows <- lapply(seq_along(fields), function(i) c(labels[i], percent(rob[[fields[i]]])))
  years <- read_result("prior_frequency_years")
  for (year in sort(unique(years$calendar_year))) {
    d <- years[years$calendar_year == year, ]
    rows[[length(rows) + 1L]] <- c(as.character(year), percent(d$positive_net_day_draw_fraction[match(groups, d$frequency_group)]))
  }
  write_rows("frequency_robustness_rows", rows)

  d <- freq[match(groups[-1], freq$frequency_group), ]
  values <- as.matrix(d[c("same_tx_offset_fraction", "extra_daily_offset_fraction", "positive_net_day_draw_fraction")]) * 100
  colors <- c("#D5A65F", "#B9CDD7", "#216775")
  # Base R's PDF device uses vector shapes and standard Helvetica fonts.
  grDevices::pdf(file.path(output, "Figure_1.pdf"), width = 7, height = 3.15,
    family = "Helvetica", useDingbats = FALSE, title = "Decomposition of gross borrowing")
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(3.2, 5, 2.6, 0.7), mgp = c(1.8, 0.5, 0), tcl = -0.2, las = 1, cex = 0.9)
  plot(NA, xlim = c(0, 100), ylim = c(0.5, 3.5), xaxs = "i", yaxs = "i",
    axes = FALSE, xlab = "Share of gross borrowing (%)", ylab = "")
  for (i in 1:3) {
    y <- 4 - i
    edges <- c(0, cumsum(values[i, ]))
    for (j in 1:3) {
      rect(edges[j], y - 0.3, edges[j + 1], y + 0.3, col = colors[j], border = NA)
      text(mean(edges[c(j, j + 1)]), y, sprintf("%.1f", values[i, j]),
        col = if (j == 3) "white" else "black", cex = 0.82)
    }
  }
  axis(1, at = seq(0, 100, 25))
  axis(2, at = 3:1, labels = c("1-2 draws", "3-9 draws", "10+ draws"), tick = FALSE)
  legend("top", inset = c(0, -0.24), xpd = NA, bty = "n", horiz = TRUE,
    legend = c("Within transaction", "Additional within day", "Positive daily net borrowing"),
    fill = colors, border = NA, cex = 0.77)
}
