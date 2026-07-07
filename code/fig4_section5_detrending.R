# Libraries ---------------------------------------------------------------
libraries <- c("ggplot2", "patchwork")

lapply(libraries, require, character.only = TRUE)

# Data --------------------------------------------------------------------
# Colombo data; Maize 1st season
years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
           2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
yields <- c(3300, 3550, 3550, 3900, 4760, 5083, 5100, 
            5211, 5320, 5703, 5703, 5355, 6075, 6400, 8300, 7440, 7500, 8095, 8391)

# Synthetic predictor with better distribution
set.seed(42)
n <- length(years)
time_all <- 0:(n - 1)

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

# Split 2: Train 2006-2016, Test 2017
train_mask <- years >= 2006 & years <= 2016
test_mask <- years == 2017

years_train <- years[train_mask]
years_test <- years[test_mask]
yields_train <- yields[train_mask]
yields_test <- yields[test_mask]

time_train <- time_all[train_mask]
time_test <- time_all[test_mask]

# === LINEAR DETRENDING ===
fit_linear_trainonly <- lm(yields_train ~ time_train)
trend_linear_trainonly <- function(t) predict(fit_linear_trainonly, 
                                              newdata = data.frame(time_train = t))
yields_train_detrend_linear_trainonly <- yields_train - trend_linear_trainonly(time_train)
yields_test_detrend_linear_trainonly <- yields_test - trend_linear_trainonly(time_test)

time_combined <- c(time_train, time_test)
yields_combined <- c(yields_train, yields_test)
fit_linear_full <- lm(yields_combined ~ time_combined)
trend_linear_full <- function(t) predict(fit_linear_full, 
                                         newdata = data.frame(time_combined = t))
yields_train_detrend_linear_full <- yields_train - trend_linear_full(time_train)
yields_test_detrend_linear_full <- yields_test - trend_linear_full(time_test)

# === MOVING AVERAGE DETRENDING ===
trend_ma_trainonly <- moving_average_trend(yields_train, window = 5)
yields_train_detrend_ma_trainonly <- yields_train - trend_ma_trainonly
trend_test_ma_trainonly <- trend_ma_trainonly[length(trend_ma_trainonly)]
yields_test_detrend_ma_trainonly <- yields_test - trend_test_ma_trainonly

years_combined <- c(years_train, years_test)
trend_ma_full <- moving_average_trend(yields_combined, window = 5)
yields_train_detrend_ma_full <- yields_train - trend_ma_full[1:length(yields_train)]
yields_test_detrend_ma_full <- yields_test - trend_ma_full[length(trend_ma_full)]

train_years_extended <- c(years_train, years_test)
time_extended <- c(time_train, time_test)

# Shared styling helpers ----

x_breaks <- seq(2003, 2021, by = 3)
xlim_years <- c(2003, 2021)

# Faint white legend box that occludes gridlines
inplot_legend <- theme(
  legend.position = c(0.02, 0.98),
  legend.justification = c(0, 1),
  legend.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.3),
  legend.key = element_blank(),
  legend.margin = margin(2, 4, 2, 4),
  plot.title = element_text(face = "bold", hjust=0.5),
  legend.key.spacing.y = unit(0, 'cm'),
  legend.key.width = unit(1.1, "cm"),
  legend.key.height    = unit(0.9, "lines")
) 

pad_range <- function(v, f = 0.05) {
  r <- range(v, na.rm = TRUE)
  d <- diff(r); if (d == 0) d <- abs(r[1])
  c(r[1] - d * f, r[2] + d * f)
}

# Shared y-ranges per row
ylim_orig <- pad_range(c(yields,
                         trend_linear_trainonly(time_extended),
                         trend_linear_full(time_extended),
                         trend_ma_trainonly, trend_ma_full))
ylim_detr <- pad_range(c(yields_train_detrend_linear_trainonly, yields_test_detrend_linear_trainonly,
                         yields_train_detrend_linear_full, yields_test_detrend_linear_full,
                         yields_train_detrend_ma_trainonly, yields_test_detrend_ma_trainonly,
                         yields_train_detrend_ma_full, yields_test_detrend_ma_full))

# Colour scale + legend override: top (original-data) panels
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

# Colour scale + legend override: detrended-target panels
scale_detr <- scale_color_manual(
  name = NULL,
  breaks = c("Training (excl. test)", "Test (excl. test)", "Training (incl. test)", "Test (incl. test)"),
  values = c("Training (excl. test)" = "blue", "Test (excl. test)" = "red",
             "Training (incl. test)" = "cornflowerblue", "Test (incl. test)" = "lightcoral")
)
guide_detr <- guides(color = guide_legend(override.aes = list(
  shape  = c(15, 15, 3, 3),
  size   = c(3, 3.5, 0.6, 0.8),
  stroke = c(0.5, 0.5, 1.8, 1.8)
)))

df_all <- data.frame(year = years, yield = yields)
df_train <- data.frame(year = years_train, yield = yields_train)
df_test <- data.frame(year = years_test, yield = yields_test)

# Row 1: Original data with trends -----

df_trend_linear_trainonly <- data.frame(year = train_years_extended, 
                                        yield = trend_linear_trainonly(time_extended))
df_trend_linear_full <- data.frame(year = train_years_extended, 
                                   yield = trend_linear_full(time_extended))

p1 <- ggplot() +
  geom_line(data = df_all, 
            aes(x = year, y = yield), color = "gray", alpha = 1) +
  geom_point(data = df_train, 
             aes(x = year, y = yield, color = "Training"), size = 2, alpha = 0.7) +
  geom_point(data = df_test, 
             aes(x = year, y = yield, color = "Test"), size = 3.5, shape = 16) +
  geom_line(data = df_trend_linear_trainonly, 
            aes(x = year, y = yield, color = "Trend (excl. test)"), linetype = "dashed", linewidth = 1) +
  geom_line(data = df_trend_linear_full,
            aes(x = year, y = yield, color = "Trend (incl. test)"), linewidth = 1) +
  scale_top + guide_top +
  scale_x_continuous(breaks = x_breaks) +
  coord_cartesian(xlim = xlim_years, ylim = ylim_orig) +
  labs(title = "Linear detrending: original data with trends", x = "Year", y = "Yield (kg/ha)") +
  theme_bw(base_size = 11) + inplot_legend

df_trend_ma_trainonly <- data.frame(year = years_train, yield = trend_ma_trainonly)
df_trend_ma_full <- data.frame(year = train_years_extended, yield = trend_ma_full)

p2 <- ggplot() +
  geom_line(data = df_all, 
            aes(x = year, y = yield), color = "gray", alpha = 1) +
  geom_point(data = df_train, 
             aes(x = year, y = yield, color = "Training"), size = 2, alpha = 0.7) +
  geom_point(data = df_test, 
             aes(x = year, y = yield, color = "Test"), size = 3.5, shape = 16) +
  geom_line(data = df_trend_ma_trainonly, 
            aes(x = year, y = yield, color = "Trend (excl. test)"), linetype = "dashed", linewidth = 1) +
  geom_line(data = df_trend_ma_full, 
            aes(x = year, y = yield, color = "Trend (incl. test)"), linewidth = 1) +
  scale_top + guide_top +
  scale_x_continuous(breaks = x_breaks) +
  coord_cartesian(xlim = xlim_years, ylim = ylim_orig) +
  labs(title = "5-year moving average: original data with trends", x = "Year", y = "Yield (kg/ha)") +
  theme_bw(base_size = 11) + inplot_legend
  
# Row 2: Detrended targets -----

df3_train_to <- data.frame(year = years_train, yield = yields_train_detrend_linear_trainonly)
df3_test_to <- data.frame(year = years_test, yield = yields_test_detrend_linear_trainonly)
df3_train_full <- data.frame(year = years_train, yield = yields_train_detrend_linear_full)
df3_test_full <- data.frame(year = years_test, yield = yields_test_detrend_linear_full)

p3 <- ggplot() +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_point(data = df3_train_full, 
             aes(x = year, y = yield, color = "Training (incl. test)"), size = 0.6, shape = 3, stroke = 1.8) +
  geom_point(data = df3_train_to, 
             aes(x = year, y = yield, color = "Training (excl. test)"), size = 3, alpha = 0.7, shape = 15) +
  geom_point(data = df3_test_to, 
             aes(x = year, y = yield, color = "Test (excl. test)"), size = 3.5, shape = 15) +
  geom_point(data = df3_test_full, 
             aes(x = year, y = yield, color = "Test (incl. test)"), size = 0.8, shape = 3, stroke = 1.8) +
  scale_detr + guide_detr +
  scale_x_continuous(breaks = x_breaks) +
  coord_cartesian(xlim = xlim_years, ylim = ylim_detr) +
  labs(title = "Detrended targets", x = "Year", y = "Detrended Yield (kg/ha)") +
  theme_bw(base_size = 11) + inplot_legend + theme(legend.text = element_text(size = 8))

df4_train_to <- data.frame(year = years_train, yield = yields_train_detrend_ma_trainonly)
df4_test_to <- data.frame(year = years_test, yield = yields_test_detrend_ma_trainonly)
df4_train_full <- data.frame(year = years_train, yield = yields_train_detrend_ma_full)
df4_test_full <- data.frame(year = years_test, yield = yields_test_detrend_ma_full)

p4 <- ggplot() +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_point(data = df4_train_full, 
             aes(x = year, y = yield, color = "Training (incl. test)"), size = 0.6, shape = 3, stroke = 1.8) +
  geom_point(data = df4_train_to, 
             aes(x = year, y = yield, color = "Training (excl. test)"), size = 3, alpha = 0.7, shape = 15) +
  geom_point(data = df4_test_to, 
             aes(x = year, y = yield, color = "Test (excl. test)"), size = 3.5, shape = 15) +
  geom_point(data = df4_test_full, 
             aes(x = year, y = yield, color = "Test (incl. test)"), size = 0.8, shape = 3, stroke = 1.8) +
  scale_detr + guide_detr +
  scale_x_continuous(breaks = x_breaks) +
  coord_cartesian(xlim = xlim_years, ylim = ylim_detr) +
  labs(title = "Detrended targets", x = "Year", y = "Detrended Yield (kg/ha)") +
  theme_bw(base_size = 12) + inplot_legend + theme(legend.text = element_text(size = 9))

# Combine into 2x2 layout
combined <- (p1 | p2) / (p3 | p4)
ggsave("./figures/fig4_detrending.png", 
       combined, width = 10, height = 6, dpi = 300)
