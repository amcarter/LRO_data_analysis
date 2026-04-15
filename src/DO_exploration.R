# ------------------------------------------------------------------
# Download dissolved oxygen data from three sites along an elevation
# gradient in the Logan River Observatory network (headwater, mid,
# valley) and compare summer diel O2 patterns.
# ------------------------------------------------------------------

source("src/explore.R")

library(ggplot2)
library(dplyr)
library(lubridate)

# --- 1. List all sites and find DO datastreams -----------------------
sites <- list_sites()

# Pick three sites spanning the elevation gradient.
# High   – Franklin Basin (headwater, high elevation)
# Mid    – Tony Grove    (mid-network)
# Valley – Main Street   (valley floor in Logan)
#
# We search by partial name match; adjust if names differ on the server.
target_sites <- c(
  high   = "Franklin Basin",
  mid    = "Tony Grove",
  valley = "Main Street"
)

selected <- list()
for (pos in names(target_sites)) {
  pattern <- target_sites[[pos]]
  idx <- grep(pattern, sites$name, ignore.case = TRUE)
  if (length(idx) == 0) {
    warning(sprintf("Could not find a site matching '%s'. Available sites:", pattern))
    print(sites$name)
    stop("Please update the target site name.")
  }
  selected[[pos]] <- sites[idx[1], ]
  message(sprintf("%-6s site: %s (id: %s)", pos, selected[[pos]]$name, selected[[pos]]$id))
}

# --- 2. For each site, find the DO datastream and download data ------
all_do <- list()

for (pos in names(selected)) {
  sid   <- selected[[pos]]$id
  sname <- selected[[pos]]$name
  message(sprintf("\n=== %s: %s ===", toupper(pos), sname))

  ds <- list_datastreams(sid)
  if (nrow(ds) == 0) {
    warning(sprintf("No datastreams for %s – skipping.", sname))
    next
  }

  # Look for dissolved oxygen datastream (match on name)
  do_idx <- grep("dissolved oxygen|DO", ds$name, ignore.case = TRUE)
  if (length(do_idx) == 0) {
    warning(sprintf("No dissolved oxygen datastream found at %s. Available:", sname))
    print(ds$name)
    next
  }

  ds_row <- ds[do_idx[1], ]
  message(sprintf("  Datastream: %s (%s)", ds_row$name, ds_row$unit))

  obs <- get_observations(ds_row$id)
  if (nrow(obs) == 0) next

  obs$site     <- sname
  obs$position <- pos
  obs$unit     <- ds_row$unit
  all_do <- c(all_do, list(obs))
}

do_obs <- bind_rows(all_do)
message(sprintf("\nTotal DO observations downloaded: %d", nrow(do_obs)))

# Save raw data
write.csv(do_obs, "data/do_observations.csv", row.names = FALSE)
message("Saved data/do_observations.csv")

# --- 3. Prepare summer subset and diel summaries --------------------
do_obs <- do_obs %>%
  mutate(
    month = month(timestamp),
    hour  = hour(timestamp),
    date  = as.Date(timestamp),
    position = factor(position, levels = c("high", "mid", "valley"),
                      labels = c("Headwater", "Mid-network", "Valley"))
  )

summer <- do_obs %>% filter(month %in% 6:8)
message(sprintf("Summer (Jun–Aug) observations: %d", nrow(summer)))

diel_summary <- summer %>%
  group_by(position, site, hour) %>%
  summarise(
    mean_do = mean(value, na.rm = TRUE),
    sd_do   = sd(value, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  )

# --- 4. Plots --------------------------------------------------------

# 4a. Diel ribbon plot – mean ± 1 SD by hour of day
unit_label <- do_obs$unit[1]

p1 <- ggplot(diel_summary, aes(x = hour, y = mean_do, colour = position, fill = position)) +
  geom_ribbon(aes(ymin = mean_do - sd_do, ymax = mean_do + sd_do), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(
    title    = "Summer Diel Dissolved Oxygen Patterns",
    subtitle = "Mean ± 1 SD across all summer days (June–August)",
    x        = "Hour of Day (UTC)",
    y        = sprintf("Dissolved Oxygen (%s)", unit_label),
    colour   = "Position",
    fill     = "Position"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p1)
ggsave("figures/do_diel_ribbon.png", p1, width = 10, height = 6, dpi = 300)
message("Saved figures/do_diel_ribbon.png")

# 4b. Raw summer time-series faceted by position
p2 <- ggplot(summer, aes(x = timestamp, y = value, colour = position)) +
  geom_line(alpha = 0.4, linewidth = 0.3) +
  facet_wrap(~ position, ncol = 1, scales = "free_y") +
  labs(
    title = "Summer Dissolved Oxygen Time Series",
    x     = "Date",
    y     = sprintf("DO (%s)", unit_label)
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)
ggsave("figures/do_summer_timeseries.png", p2, width = 12, height = 8, dpi = 300)
message("Saved figures/do_summer_timeseries.png")

# 4c. Boxplot of DO by hour, faceted by position
p3 <- ggplot(summer, aes(x = factor(hour), y = value, fill = position)) +
  geom_boxplot(outlier.size = 0.3, alpha = 0.7) +
  facet_wrap(~ position, ncol = 1) +
  labs(
    title = "Summer Diel DO Distribution by Hour",
    x     = "Hour of Day (UTC)",
    y     = sprintf("DO (%s)", unit_label)
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p3)
ggsave("figures/do_diel_boxplots.png", p3, width = 12, height = 8, dpi = 300)
message("Saved figures/do_diel_boxplots.png")

message("\nDone. Data and plots saved.")
