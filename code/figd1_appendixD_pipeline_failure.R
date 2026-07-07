# Libraries ---------------------------------------------------------------
libraries <- c("ggplot2", "patchwork")

lapply(libraries, require, character.only = TRUE)

# Data --------------------------------------------------------------------
# Santa Izabel data
years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
           2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
yields <- c(6500, 7000, 5250, 1750, 8000, 6000, 2983, 8000, 8000, 
            4000, 8700, 10000, 10000, 8000, 10000, 9900, 10500, 11200, 6000)

time_all <- 0:(length(years) - 1)

# Moving average trend (5-year centered)
moving_average_trend <- function(yields_data, window = 5) {
  half_window <- window %/% 2
  n_pts <- length(yields_data)
  trend_values <- numeric(n_pts)
  for (i in seq_len(n_pts)) {
    start_idx <- max(1, i - half_window)
    end_idx <- min(n_pts, i + half_window)
    trend_values[i] <- mean(yields_data[start_idx:end_idx])
  }
  trend_values
}

# Two cases: normal year vs failure year
cases <- list(
  list(test_year = 2013, label = "Normal year (2013: 8,700 kg/ha)"),
  list(test_year = 2006, label = "Failure year (2006: 1,750 kg/ha)")
)

results <- list()

for (case in cases) {
  test_mask <- years == case$test_year
  train_mask <- !test_mask
  
  years_train <- years[train_mask]
  years_test <- years[test_mask]
  yields_train <- yields[train_mask]
  yields_test <- yields[test_mask]
  
  time_train <- time_all[train_mask]
  time_test <- time_all[test_mask]
  
  # LINEAR DETRENDING
  fit_linear_trainonly <- lm(yields_train ~ time_train)
  trend_linear_trainonly <- local({
    f <- fit_linear_trainonly
    function(t) predict(f, newdata = data.frame(time_train = t))
  })
  yields_train_detrend_linear_trainonly <- yields_train - trend_linear_trainonly(time_train)
  yields_test_detrend_linear_trainonly <- yields_test - trend_linear_trainonly(time_test)
  
  fit_linear_full <- lm(yields ~ time_all)
  trend_linear_full <- local({
    f <- fit_linear_full
    function(t) predict(f, newdata = data.frame(time_all = t))
  })
  yields_train_detrend_linear_full <- yields_train - trend_linear_full(time_train)
  yields_test_detrend_linear_full <- yields_test - trend_linear_full(time_test)
  
  # MOVING AVERAGE DETRENDING
  trend_ma_trainonly <- moving_average_trend(yields_train, window = 5)
  yields_train_detrend_ma_trainonly <- yields_train - trend_ma_trainonly
  test_idx_in_all <- which(years == case$test_year)
  train_indices_in_all <- which(train_mask)
  nearest_train_idx <- train_indices_in_all[which.min(abs(train_indices_in_all - test_idx_in_all))]
  nearest_train_idx_in_subset <- which(train_indices_in_all == nearest_train_idx)
  trend_test_ma_trainonly <- trend_ma_trainonly[nearest_train_idx_in_subset]
  yields_test_detrend_ma_trainonly <- yields_test - trend_test_ma_trainonly
  
  trend_ma_full <- moving_average_trend(yields, window = 5)
  yields_train_detrend_ma_full <- yields_train - trend_ma_full[train_mask]
  yields_test_detrend_ma_full <- yields_test - trend_ma_full[test_mask]
  
  results[[length(results) + 1]] <- list(
    case = case,
    years_train = years_train, yields_train = yields_train,
    years_test = years_test, yields_test = yields_test,
    time_train = time_train, time_test = time_test,
    trend_linear_trainonly = trend_linear_trainonly,
    trend_linear_full = trend_linear_full,
    yields_train_detrend_linear_trainonly = yields_train_detrend_linear_trainonly,
    yields_test_detrend_linear_trainonly = yields_test_detrend_linear_trainonly,
    yields_train_detrend_linear_full = yields_train_detrend_linear_full,
    yields_test_detrend_linear_full = yields_test_detrend_linear_full,
    trend_ma_trainonly = trend_ma_trainonly,
    trend_ma_full = trend_ma_full,
    yields_train_detrend_ma_trainonly = yields_train_detrend_ma_trainonly,
    yields_test_detrend_ma_trainonly = yields_test_detrend_ma_trainonly,
    yields_train_detrend_ma_full = yields_train_detrend_ma_full,
    yields_test_detrend_ma_full = yields_test_detrend_ma_full
  )
}

# Shared styling helpers -----

x_breaks <- years[seq(1, length(years), by = 3)]
xlim_years <- c(2003, 2021)
df_all <- data.frame(year = years, yield = yields)

# In-panel legend: faint white box, centred titles, tightened key rows,
# wide keys so the dashed trend shows more than one dash
inplot_legend <- theme(
  legend.position = c(0.02, 0.98),
  legend.justification = c(0, 1),
  legend.background = element_rect(fill = alpha("white",0.7), colour = "grey70", linewidth = 0.3),
  legend.key = element_blank(),
  legend.margin = margin(2, 4, 2, 4),
  plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
  legend.key.spacing.y = unit(0, "cm"),
  legend.key.width = unit(1.1, "cm"),
  legend.key.height = unit(0.9, "lines")
)

# Distribution-shift legend: same look, now INSIDE the panel (top-right so it
# clears the far-left test lines in the failure-year cases)
hist_legend <- theme(
  legend.position = c(0.98, 0.98),
  legend.justification = c(1, 1),
  legend.box = "vertical",
  legend.box.just = "left",
  legend.box.background = element_rect(fill = alpha("white",0.7), colour = "grey70", linewidth = 0.3, alpha = 0.7),
  legend.box.margin = margin(2, 2, 2, 2),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(size = 8),
  legend.spacing.y = unit(0, "pt"),
  legend.key.spacing.y = unit(0, "cm"),
  legend.key.width = unit(1.0, "cm"),
  legend.key.height = unit(0.9, "lines"),
  plot.title = element_text(face = "bold", size = 11, hjust = 0.5)
)

pad_range <- function(v, f = 0.05) {
  r <- range(v, na.rm = TRUE)
  d <- diff(r); if (d == 0) d <- abs(r[1])
  c(r[1] - d * f, r[2] + d * f)
}

# --- Global shared y-ranges across BOTH cases, by panel type ---
orig_vals <- c(); detr_vals <- c(); hist_ymax <- 0
for (r in results) {
  orig_vals <- c(orig_vals, yields,
                 r$trend_linear_trainonly(time_all), r$trend_linear_full(time_all),
                 r$trend_ma_trainonly, r$trend_ma_full)
  detr_vals <- c(detr_vals,
                 r$yields_train_detrend_linear_trainonly, r$yields_test_detrend_linear_trainonly,
                 r$yields_train_detrend_linear_full, r$yields_test_detrend_linear_full,
                 r$yields_train_detrend_ma_trainonly, r$yields_test_detrend_ma_trainonly,
                 r$yields_train_detrend_ma_full, r$yields_test_detrend_ma_full)
  for (meth in c("lin", "ma")) {
    if (meth == "lin") {
      allv <- c(r$yields_train_detrend_linear_trainonly, r$yields_test_detrend_linear_trainonly,
                r$yields_train_detrend_linear_full, r$yields_test_detrend_linear_full)
      dat <- r$yields_train_detrend_linear_trainonly
    } else {
      allv <- c(r$yields_train_detrend_ma_trainonly, r$yields_test_detrend_ma_trainonly,
                r$yields_train_detrend_ma_full, r$yields_test_detrend_ma_full)
      dat <- r$yields_train_detrend_ma_trainonly
    }
    br <- seq(min(allv), max(allv), length.out = 15)
    cnt <- hist(dat, breaks = br, plot = FALSE, include.lowest = TRUE)$counts
    hist_ymax <- max(hist_ymax, max(cnt))
  }
}
ylim_orig <- pad_range(orig_vals)
ylim_detr <- pad_range(detr_vals)

# --- Distribution-shift panels: one fixed, shared x-axis + aligned bins ---
# All four histograms use the same detrended-yield domain (the same range as
# the detrended-scatter panels above), so bins line up across panels and every
# test line stays in view. Bins span the raw data range; the axis is padded.
hist_min <- min(detr_vals); hist_max <- max(detr_vals)
hist_binwidth <- (hist_max - hist_min) / 14
hist_boundary <- hist_min
hist_breaks_count <- seq(hist_min, hist_max, length.out = 15)
xlim_hist <- ylim_detr
hist_x_breaks <- pretty(xlim_hist)

# Shared count axis, computed with the common bins
hist_ymax <- 0
for (r in results) {
  for (dat in list(r$yields_train_detrend_linear_trainonly, r$yields_train_detrend_ma_trainonly)) {
    cnt <- hist(dat, breaks = hist_breaks_count, plot = FALSE, include.lowest = TRUE)$counts
    hist_ymax <- max(hist_ymax, max(cnt))
  }
}
ylim_hist <- c(0, hist_ymax)

# Colour scales + legend overrides (glyph sizes matched to Colombo)
scale_top <- scale_color_manual(
  name = NULL,
  breaks = c("Training", "Test", "Trend (excl. test)", "Trend (incl. test)"),
  values = c("Training" = "blue", "Test" = "red",
             "Trend (excl. test)" = "blue", "Trend (incl. test)" = "purple")
)
guide_top <- guides(color = guide_legend(override.aes = list(
  shape     = c(16, 16, NA, NA),
  linetype  = c(0, 0, 2, 1),
  linewidth = c(0, 0, 1, 1),
  size      = c(3, 4.5, 1, 0)
)))

scale_detr <- scale_color_manual(
  name = NULL,
  breaks = c("Training (excl. test)", "Test (excl. test)", "Train (incl. test)", "Test (incl. test)"),
  values = c("Training (excl. test)" = "blue", "Test (excl. test)" = "red",
             "Train (incl. test)" = "cornflowerblue", "Test (incl. test)" = "lightcoral")
)
guide_detr <- guides(color = guide_legend(override.aes = list(
  shape  = c(15, 15, 3, 3),
  size   = c(3, 3.5, 0.6, 0.8),
  stroke = c(0.5, 0.5, 1.8, 1.8)
)))

# Plots -------------------------------------------------------------------
plot_list <- list()

for (case_idx in seq_along(results)) {
  r <- results[[case_idx]]
  case <- r$case
  
  df_train <- data.frame(year = r$years_train, yield = r$yields_train)
  df_test <- data.frame(year = r$years_test, yield = r$yields_test)
  
  # --- Panel 1: Linear detrending - original data with trends ---
  df_trend_lin_to <- data.frame(year = years, yield = r$trend_linear_trainonly(time_all))
  df_trend_lin_full <- data.frame(year = years, yield = r$trend_linear_full(time_all))
  title1 <- if (case_idx == 1) paste0("Linear detrending: original data with trends\n", case$label) else case$label
  
  p1 <- ggplot() +
    geom_line(data = df_all, aes(x = year, y = yield), color = "gray", alpha = 0.3) +
    geom_point(data = df_train, aes(x = year, y = yield, color = "Training"), size = 2, alpha = 0.7) +
    geom_point(data = df_test, aes(x = year, y = yield, color = "Test"), size = 3.5, shape = 16) +
    geom_line(data = df_trend_lin_to, aes(x = year, y = yield, color = "Trend (excl. test)"), linetype = "dashed", linewidth = 1) +
    geom_line(data = df_trend_lin_full, aes(x = year, y = yield, color = "Trend (incl. test)"), linewidth = 1) +
    scale_top + guide_top +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(xlim = xlim_years, ylim = ylim_orig) +
    labs(title = title1, x = "Year", y = "Yield (kg/ha)") +
    theme_bw(base_size = 10) + inplot_legend + theme(legend.position.inside = c(0.56,0.5))
  
  # --- Panel 2: Moving average - original data with trends ---
  df_trend_ma_to <- data.frame(year = r$years_train, yield = r$trend_ma_trainonly)
  df_trend_ma_full <- data.frame(year = years, yield = r$trend_ma_full)
  title2 <- if (case_idx == 1) paste0("5-year moving average: original data with trends\n", case$label) else case$label
  
  p2 <- ggplot() +
    geom_line(data = df_all, aes(x = year, y = yield), color = "gray", alpha = 0.3) +
    geom_point(data = df_train, aes(x = year, y = yield, color = "Training"), size = 2, alpha = 0.7) +
    geom_point(data = df_test, aes(x = year, y = yield, color = "Test"), size = 3.5, shape = 16) +
    geom_line(data = df_trend_ma_to, aes(x = year, y = yield, color = "Trend (excl. test)"), linetype = "dashed", linewidth = 1) +
    geom_line(data = df_trend_ma_full, aes(x = year, y = yield, color = "Trend (incl. test)"), linewidth = 1) +
    scale_top + guide_top +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(xlim = xlim_years, ylim = ylim_orig) +
    labs(title = title2, x = "Year", y = "Yield (kg/ha)") +
    theme_bw(base_size = 10) + inplot_legend + theme(legend.position.inside = c(0.56,0.5))
  
  # --- Panel 3: Detrended targets (linear) ---
  df3_train_to <- data.frame(year = r$years_train, yield = r$yields_train_detrend_linear_trainonly)
  df3_test_to <- data.frame(year = r$years_test, yield = r$yields_test_detrend_linear_trainonly)
  df3_train_full <- data.frame(year = r$years_train, yield = r$yields_train_detrend_linear_full)
  df3_test_full <- data.frame(year = r$years_test, yield = r$yields_test_detrend_linear_full)
  
  p3 <- ggplot() +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_point(data = df3_train_full, aes(x = year, y = yield, color = "Train (incl. test)"), size = 0.6, shape = 3, stroke = 1.8) +
    geom_point(data = df3_train_to, aes(x = year, y = yield, color = "Training (excl. test)"), size = 3, alpha = 0.7, shape = 15) +
    geom_point(data = df3_test_to, aes(x = year, y = yield, color = "Test (excl. test)"), size = 3.5, shape = 15) +
    geom_point(data = df3_test_full, aes(x = year, y = yield, color = "Test (incl. test)"), size = 0.8, shape = 3, stroke = 1.8) +
    scale_detr + guide_detr +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(xlim = xlim_years, ylim = ylim_detr) +
    labs(title = "Detrended targets", x = "Year", y = "Detrended Yield (kg/ha)") +
    theme_bw(base_size = 10) + inplot_legend + theme(legend.text = element_text(size = 7), legend.position.inside = c(0.56,0.5))
  
  # --- Panel 4: Detrended targets (moving average) ---
  df4_train_to <- data.frame(year = r$years_train, yield = r$yields_train_detrend_ma_trainonly)
  df4_test_to <- data.frame(year = r$years_test, yield = r$yields_test_detrend_ma_trainonly)
  df4_train_full <- data.frame(year = r$years_train, yield = r$yields_train_detrend_ma_full)
  df4_test_full <- data.frame(year = r$years_test, yield = r$yields_test_detrend_ma_full)
  
  p4 <- ggplot() +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_point(data = df4_train_full, aes(x = year, y = yield, color = "Train (incl. test)"), size = 0.6, shape = 3, stroke = 1.8) +
    geom_point(data = df4_train_to, aes(x = year, y = yield, color = "Training (excl. test)"), size = 3, alpha = 0.7, shape = 15) +
    geom_point(data = df4_test_to, aes(x = year, y = yield, color = "Test (excl. test)"), size = 3.5, shape = 15) +
    geom_point(data = df4_test_full, aes(x = year, y = yield, color = "Test (incl. test)"), size = 0.8, shape = 3, stroke = 1.8) +
    scale_detr + guide_detr +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(xlim = xlim_years, ylim = ylim_detr) +
    labs(title = "Detrended targets", x = "Year", y = "Detrended Yield (kg/ha)") +
    theme_bw(base_size = 10) + inplot_legend + theme(legend.text = element_text(size = 7), legend.position.inside = c(0.56,0.5))
  
  # --- Panel 5: Distribution shifts (linear) ---
  df_hist <- data.frame(yield = r$yields_train_detrend_linear_trainonly)
  
  p5 <- ggplot(df_hist, aes(x = yield)) +
    geom_histogram(aes(fill = "Training (excl. test)"), binwidth = hist_binwidth, color = "black", boundary = hist_boundary) +
    geom_vline(data = data.frame(
      x   = c(r$yields_test_detrend_linear_trainonly[1], r$yields_test_detrend_linear_full[1]),
      grp = c("Test (excl. test)", "Test (incl. test)")),
      aes(xintercept = x, color = grp), linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = 0, color = "black", alpha = 0.5) +
    scale_fill_manual(name = NULL, values = c("Training (excl. test)" = "cornflowerblue")) +
    scale_color_manual(name = NULL, breaks = c("Test (excl. test)", "Test (incl. test)"),
                       values = c("Test (excl. test)" = "red", "Test (incl. test)" = "red4")) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
    scale_x_continuous(breaks = hist_x_breaks) +
    coord_cartesian(xlim = xlim_hist, ylim = ylim_hist) +
    labs(title = "Distribution shifts", x = "Detrended Yield (kg/ha)", y = "Frequency") +
    theme_bw(base_size = 10) + hist_legend + theme(legend.position.inside = c(0.56,0.95))
  
  # --- Panel 6: Distribution shifts (moving average) ---
  df_hist_ma <- data.frame(yield = r$yields_train_detrend_ma_trainonly)
  
  p6 <- ggplot(df_hist_ma, aes(x = yield)) +
    geom_histogram(aes(fill = "Training (excl. test)"), binwidth = hist_binwidth, color = "black", boundary = hist_boundary) +
    geom_vline(data = data.frame(
      x   = c(r$yields_test_detrend_ma_trainonly[1], r$yields_test_detrend_ma_full[1]),
      grp = c("Test (excl. test)", "Test (incl. test)")),
      aes(xintercept = x, color = grp), linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = 0, color = "black", alpha = 0.5) +
    scale_fill_manual(name = NULL, values = c("Training (excl. test)" = "cornflowerblue")) +
    scale_color_manual(name = NULL, breaks = c("Test (excl. test)", "Test (incl. test)"),
                       values = c("Test (excl. test)" = "red", "Test (incl. test)" = "red4")) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
    scale_x_continuous(breaks = hist_x_breaks) +
    coord_cartesian(xlim = xlim_hist, ylim = ylim_hist) +
    labs(title = "Distribution shifts", x = "Detrended Yield (kg/ha)", y = "Frequency") +
    theme_bw(base_size = 10) + hist_legend + theme(legend.position.inside = c(0.565,0.95))
  
  plot_list <- c(plot_list, list(p1, p2, p3, p4, p5, p6))
}


# Combine into 6x2 layout (6 rows, 2 columns)
combined <- wrap_plots(plot_list, ncol = 2, nrow = 6)
ggsave("./figures/figd1_pipeline_eval.png", combined, width = 10, height = 14, dpi = 300)