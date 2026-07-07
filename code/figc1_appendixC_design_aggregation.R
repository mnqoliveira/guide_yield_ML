# Libraries ---------------------------------------------------------------
libraries <- c("ggplot2", "patchwork", "dplyr")

lapply(libraries, require, character.only = TRUE)

# Data --------------------------------------------------------------------
df <- read.csv("./data/weather_dou.csv")

# Wofost, Generic C4 ------------------------------------------------------
base_temp     <- 10
tupper        <- 35
season_dd     <- 1680
flowering_dd  <- 820

planting_doy  <- 10

selected_years <- c(2008, #2011, 
                    2020, 2021)
year_colors    <- c("2008" = "#1f77b4", 
                    #"2011" = "#2ca02c",
                    "2020" = "#ff7f0e", "2021" = "#d62728")

# Auxiliary functions -----------------------------------------------------
# Month/date conversion using the ACTUAL calendar of a given year
# (fixes the leap-year offset for non-leap years)
doy_to_date_year <- function(d, year) {
    as.Date(d - 1, origin = paste0(year, "-01-01"))
}

# Generic axis-tick labels: fixed non-leap reference year, cosmetic only
REF_YEAR <- 2001
doy_label <- function(d) {
    format(as.Date(d - 1, origin = paste0(REF_YEAR, "-01-01")), "%b %d")
}

# ---- compute season data ----
all_seasons  <- list()
all_monthly  <- list()
all_periods  <- list()
milestones   <- list()

for (yr in selected_years) {
    s <- df %>%
        filter(year == yr, doy >= planting_doy) %>%
        arrange(doy) %>%
        mutate(gdd     = if_else(tmean > tupper, tupper - base_temp,
                                 pmax(tmean - base_temp, 0)),
               cumgdd  = cumsum(gdd),
               # month assigned using THIS year's actual calendar
               month   = as.integer(format(doy_to_date_year(doy, yr), "%m")))
    
    end_doy        <- s$doy[which(s$cumgdd >= season_dd)[1]]
    flow_doy       <- s$doy[which(s$cumgdd >= flowering_dd)[1]]
    flow_minus_doy <- s$doy[which(s$cumgdd >= flowering_dd - 100)[1]]
    flow_plus_doy  <- s$doy[which(s$cumgdd >= flowering_dd + 100)[1]]
    
    s <- s %>% filter(doy <= end_doy)
    
    monthly <- s %>%
        group_by(month) %>%
        summarise(tmean_avg = mean(tmean),
                  doy_start = min(doy),
                  doy_end   = max(doy),
                  .groups = "drop") %>%
        mutate(year = yr)
    
    p1 <- mean(s$tmean[s$doy <= flow_doy])
    p2 <- mean(s$tmean[s$doy >= flow_minus_doy & s$doy <= flow_plus_doy])
    p3 <- mean(s$tmean[s$doy >= flow_doy])
    
    periods <- data.frame(
        year    = yr,
        period  = c("Planting → Flowering", "Fl. ±100 DD", "Flowering → End"),
        avg     = c(p1, p2, p3),
        x_start = c(planting_doy,  flow_minus_doy, flow_doy),
        x_end   = c(flow_doy,      flow_plus_doy,  end_doy)
    )
    
    milestones[[as.character(yr)]] <- list(
        end_doy        = end_doy,
        flow_doy       = flow_doy,
        flow_minus_doy = flow_minus_doy,
        flow_plus_doy  = flow_plus_doy,
        season_mean    = mean(s$tmean),
        agg_min_m      = min(monthly$tmean_avg),
        agg_max_m      = max(monthly$tmean_avg),
        agg_min_p      = min(periods$avg),
        agg_max_p      = max(periods$avg)
    )
    
    all_seasons[[as.character(yr)]] <- s %>% mutate(year = yr)
    all_monthly[[as.character(yr)]] <- monthly
    all_periods[[as.character(yr)]] <- periods
}

seasons_df <- bind_rows(all_seasons) %>% mutate(year = factor(year))
monthly_df <- bind_rows(all_monthly) %>% mutate(year = factor(year))
periods_df <- bind_rows(all_periods) %>% mutate(year = factor(year))

xmax   <- max(sapply(milestones, `[[`, "end_doy")) + 8
xticks <- seq(planting_doy, xmax, by = 10)

# ---- lane layout helpers ----
n_yr        <- length(selected_years)
lane_height <- 2.5
lane_pad    <- 0.5
total_h     <- n_yr * lane_height + (n_yr - 1) * lane_pad

lane_center <- function(i) {   # i = 1-based index from top
    total_h - (i - 1) * (lane_height + lane_pad) - lane_height / 2
}
lane_centers <- setNames(sapply(seq_along(selected_years), lane_center),
                         as.character(selected_years))
half_band <- lane_height * 0.42

t_to_y <- function(t, yc, t_min, t_max) {
    t_mid <- (t_min + t_max) / 2
    scale <- t_max - t_min + 1e-6
    yc + (t - t_mid) / scale * 2 * half_band
}

ylim_lanes <- c(-0.8, total_h + 1.0)

# ---- Row 1: daily lines ----
end_pts <- do.call(rbind, lapply(as.character(selected_years), function(yr) {
    s   <- all_seasons[[yr]]
    ms  <- milestones[[yr]]
    row <- s[s$doy == ms$end_doy, ][1, ]
    data.frame(year = factor(yr), doy = ms$end_doy, tmean = row$tmean)
}))
flow_lines <- data.frame(
    year = factor(as.character(selected_years)),
    flow_doy = sapply(as.character(selected_years), function(yr) milestones[[yr]]$flow_doy)
)

all_temps  <- bind_rows(all_seasons)$tmean
ylim1      <- c(min(all_temps) - 1.5, max(all_temps) + 3.0)

# ---- Row 1: cumulative GDD on twin axis ----
cumgdd_df <- seasons_df %>% select(year, doy, cumgdd)
# scale cumgdd to the temperature axis range, then draw a secondary axis
gdd_max   <- season_dd * 1.15
scale_fac <- diff(ylim1) / gdd_max
cumgdd_df <- cumgdd_df %>% mutate(tmean_scaled = cumgdd * scale_fac + ylim1[1])

ref_lines <- data.frame(
    cumgdd = c(flowering_dd, season_dd),
    label  = c("Flowering", "End of season")
) %>% mutate(y = cumgdd * scale_fac + ylim1[1])

p1_plot <- ggplot() +
    # background cumulative GDD curves (faint)
    geom_line(data = cumgdd_df, aes(x = doy, y = tmean_scaled, colour = year),
              linewidth = 1.0, alpha = 0.25) +
    geom_hline(data = ref_lines, aes(yintercept = y),
               colour = "grey50", linetype = "dotted", linewidth = 0.6) +
    geom_text(data = ref_lines, aes(x = xmax - 0.5, y = y + diff(ylim1) * 0.01, label = label),
              hjust = 1, vjust = 0, size = 2.5, colour = "grey50") +
    # daily temperature lines
    geom_line(data = seasons_df, aes(x = doy, y = tmean, colour = year),
              linewidth = 0.9, alpha = 0.85) +
    geom_vline(data = flow_lines, aes(xintercept = flow_doy, colour = year),
               linetype = "dashed", linewidth = 0.8, alpha = 0.6) +
    geom_vline(xintercept = planting_doy, linetype = "dotted",
               colour = "black", linewidth = 0.9) +
    geom_text(data = end_pts, aes(x = doy + 0.6, y = tmean, label = year, colour = year),
              hjust = 0, vjust = 0.5, fontface = "bold", size = 3.2, show.legend = FALSE) +
    annotate("text", x = planting_doy + 0.5, y = ylim1[2] - 0.3,
             label = "Planting", hjust = 0, vjust = 1, size = 2.8) +
    scale_colour_manual(
        values = year_colors,
        labels = sapply(as.character(selected_years), function(yr)
            sprintf("%s (mean: %.1f°C)", yr, milestones[[yr]]$season_mean))
    ) +
    scale_x_continuous(breaks = xticks, labels = doy_label(xticks),
                       limits = c(planting_doy - 2, xmax)) +
    scale_y_continuous(
        limits = ylim1,
        sec.axis = sec_axis(~ (. - ylim1[1]) / scale_fac,
                            name = "Cumulative degree days (°C·d)")
    ) +
    labs(x = "Date", y = "Mean temperature (°C)",
         title = "(a) Daily mean temperature", colour = NULL) +
    theme_bw(base_size = 10) +
    theme(legend.position   = c(0.02, 0.05),
          legend.justification = c(0, 0),
          legend.background = element_rect(fill = "white", colour = "gray80"),
          legend.key.size   = unit(0.4, "cm"),
          legend.text       = element_text(size = 8),
          axis.text.x       = element_text(angle = 30, hjust = 1, size = 7.5),
          axis.title.y.right = element_text(colour = "grey40", size = 8),
          axis.text.y.right  = element_text(colour = "grey40", size = 7.5),
          plot.title        = element_text(face = "bold", size = 10))

# ---- Lane panel builder ----
make_lane_panel <- function(agg_df, mode, title_str) {
    bar_rows   <- list()
    axis_ticks <- list()
    axis_spine <- list()
    flow_segs  <- list()
    year_labs  <- list()
    
    year_label_x <- planting_doy - 13
    tick_label_x <- planting_doy - 8.5
    spine_x      <- planting_doy - 5.5
    
    for (i in seq_along(selected_years)) {
        yr  <- as.character(selected_years[i])
        yc  <- lane_centers[yr]
        ms  <- milestones[[yr]]
        
        t_min <- (if (mode == "monthly") ms$agg_min_m else ms$agg_min_p) - 0.5
        t_max <- (if (mode == "monthly") ms$agg_max_m else ms$agg_max_p) + 0.5
        
        rows <- agg_df %>% filter(year == yr)
        rows$bar_y <- t_to_y(rows$avg, yc, t_min, t_max)
        
        bar_rows[[yr]] <- rows
        
        tv <- unique(as.integer(seq(ceiling(t_min + 0.5), floor(t_max - 0.5), length.out = 3)))
        axis_ticks[[yr]] <- data.frame(
            year   = yr,
            tv     = tv,
            ty     = t_to_y(tv, yc, t_min, t_max),
            x0     = spine_x,
            x1     = spine_x + 0.4,
            label_x = tick_label_x
        )
        
        y_bot <- t_to_y(t_min, yc, t_min, t_max)
        y_top <- t_to_y(t_max, yc, t_min, t_max)
        axis_spine[[yr]] <- data.frame(year = yr, x = spine_x, ybot = y_bot, ytop = y_top)
        
        flow_segs[[yr]] <- data.frame(
            year = yr, x = ms$flow_doy,
            ybot = y_bot, ytop = y_top
        )
        
        year_labs[[yr]] <- data.frame(year = yr, x = year_label_x, y = yc)
    }
    
    bar_df   <- bind_rows(bar_rows)   %>% mutate(year = factor(year))
    tick_df  <- bind_rows(axis_ticks) %>% mutate(year = factor(year))
    spine_df <- bind_rows(axis_spine) %>% mutate(year = factor(year))
    flow_df  <- bind_rows(flow_segs)  %>% mutate(year = factor(year))
    lab_df   <- bind_rows(year_labs)  %>% mutate(year = factor(year))
    
    sep_ys <- c(sapply(lane_centers, function(yc) yc + lane_height / 2),
                min(lane_centers) - lane_height / 2)
    
    if (mode == "monthly") {
        month_rng <- monthly_df %>%
            group_by(month) %>%
            summarise(ds = min(doy_start), de = max(doy_end), .groups = "drop") %>%
            mutate(mid = (ds + de) / 2,
                   label = month.abb[month])
        header_df  <- month_rng
        vline_xs   <- sort(unique(c(header_df$ds, header_df$de)))
    } else {
        period_mids <- periods_df %>%
            group_by(period) %>%
            summarise(mid = mean((x_start + x_end) / 2), .groups = "drop")
        header_df  <- period_mids %>% rename(label = period)
        vline_xs   <- numeric(0)
    }
    
    g <- ggplot() +
        geom_hline(yintercept = sep_ys, colour = "grey85", linewidth = 0.5) +
        geom_segment(data = spine_df,
                     aes(x = x, xend = x, y = ybot, yend = ytop, colour = year),
                     linewidth = 0.8, alpha = 0.7) +
        geom_segment(data = tick_df,
                     aes(x = x0, xend = x1, y = ty, yend = ty, colour = year),
                     linewidth = 0.7, alpha = 0.7) +
        geom_text(data = tick_df,
                  aes(x = label_x, y = ty, label = tv, colour = year),
                  hjust = 1, vjust = 0.5, size = 2.3, alpha = 0.9) +
        geom_text(data = lab_df,
                  aes(x = x, y = y, label = year, colour = year),
                  hjust = 0.5, vjust = 0.5, fontface = "bold", size = 3.2) +
        geom_segment(data = flow_df,
                     aes(x = x, xend = x, y = ybot, yend = ytop, colour = year),
                     linetype = "dashed", linewidth = 0.8, alpha = 0.5) +
        geom_segment(data = bar_df,
                     aes(x = x_start, xend = x_end, y = bar_y, yend = bar_y, colour = year),
                     linewidth = 4.5, alpha = 0.88, lineend = "butt") +
        geom_label(data = bar_df,
                   aes(x = (x_start + x_end) / 2, y = bar_y + 0.13,
                       label = sprintf("%.1f", avg), colour = year),
                   vjust = 0, size = 2.8, fontface = "bold",
                   fill = "white", label.size = 0.3, label.padding = unit(0.12, "lines")) +
        scale_colour_manual(values = year_colors) +
        scale_x_continuous(breaks = xticks, labels = doy_label(xticks),
                           limits = c(year_label_x - 1, xmax)) +
        scale_y_continuous(limits = ylim_lanes) +
        labs(x = "Date", y = NULL, title = title_str) +
        theme_void(base_size = 10) +
        theme(legend.position  = "none",
              plot.title       = element_text(face = "bold", size = 10, hjust = 0,
                                              margin = margin(b = 4)),
              axis.text.x      = element_text(angle = 30, hjust = 1, size = 7.5,
                                              colour = "grey40"),
              axis.ticks.x     = element_line(colour = "grey60"),
              axis.line.x      = element_line(colour = "grey60"),
              plot.margin      = margin(5, 5, 5, 5))
    
    if (mode == "monthly") {
        g <- g +
            geom_vline(xintercept = vline_xs, colour = "grey70",
                       linetype = "dotted", linewidth = 0.4, alpha = 0.5) +
            geom_text(data = header_df,
                      aes(x = mid, y = ylim_lanes[2] - 0.05, label = label),
                      inherit.aes = FALSE,
                      hjust = 0.5, vjust = 1, fontface = "bold", size = 3.2, colour = "black")
    } else {
        g <- g +
            geom_text(data = header_df,
                      aes(x = mid, y = ylim_lanes[2] - 0.05, label = label),
                      inherit.aes = FALSE,
                      hjust = 0.5, vjust = 1, fontface = "bold", size = 3.0, colour = "black")
    }
    g
}

monthly_agg <- monthly_df %>%
    rename(x_start = doy_start, x_end = doy_end, avg = tmean_avg) %>%
    select(year, x_start, x_end, avg)

periods_agg <- periods_df %>%
    select(year, x_start, x_end, avg, period)

p2_plot <- make_lane_panel(monthly_agg, "monthly", "(b) Monthly aggregation")
p3_plot <- make_lane_panel(periods_agg, "periods", "(c) Phenological period aggregation")

# ---- combine ----
final <- p1_plot / p2_plot / p3_plot +
    plot_layout(heights = c(1.1, 1, 1))

ggsave("./figures/fig_c1_aggregation_comparison.png",
       final, width = 12, height = 14, dpi = 300, bg = "white")
