# ------------------------------------------------------------------
# Quick exploration of Logan River Observatory data.
# Sources the API helper functions from explore.R, lists available
# sites and datastreams, downloads observations from the first
# datastream of the first site, and plots the result.
# ------------------------------------------------------------------

source("src/explore.R")

# --- 1. List all monitoring sites ------------------------------------
sites <- list_sites()
print(head(sites, 20))

# --- 2. Pick the first site and list its datastreams -----------------
canal_sites <- dplyr::filter(sites, site_type == "Canal")
storm_sites <- dplyr::filter(sites, site_type == "Storm sewer")

# --- 3. Download all observations from every canal site ---------------
library(ggplot2)

all_obs <- list()

for (i in seq_len(nrow(canal_sites))) {
  sid   <- canal_sites$id[i]
  sname <- canal_sites$name[i]
  message(sprintf("\n=== Site %d/%d: %s ===", i, nrow(canal_sites), sname))

  ds <- list_datastreams(sid)
  if (nrow(ds) == 0) next

  for (j in seq_len(nrow(ds))) {
    ds_id   <- ds$id[j]
    ds_name <- ds$name[j]
    ds_unit <- ds$unit[j]
    message(sprintf("  Datastream: %s (%s)", ds_name, ds_unit))

    obs <- get_observations(ds_id)
    if (nrow(obs) == 0) next

    obs$site      <- sname
    obs$variable  <- ds_name
    obs$unit      <- ds_unit
    obs$var_label <- sprintf("%s (%s)", ds_name, ds_unit)
    all_obs <- c(all_obs, list(obs))
  }
}

canal_obs <- do.call(rbind, all_obs)
message(sprintf("\nTotal canal observations downloaded: %d", nrow(canal_obs)))

# Save the combined data to CSV
write.csv(canal_obs, "data/canal_observations.csv", row.names = FALSE)
message("Saved canal observations to data/canal_observations.csv")

# --- 4. Quick summary ------------------------------------------------
print(summary(canal_obs))
message("\nObservations per variable:")
print(table(canal_obs$variable))
message("\nObservations per site:")
print(table(canal_obs$site))

# --- 5. Exploratory plots --------------------------------------------

# 5a. Time-series faceted by variable (all sites overlaid)
p1 <- ggplot(canal_obs, aes(x = timestamp, y = value, colour = site)) +
  geom_line(alpha = 0.7, linewidth = 0.4) +
  facet_wrap(~ var_label, scales = "free_y", ncol = 1) +
  labs(
    title  = "Canal Sites – All Variables Over Time",
    x      = "Date",
    y      = "Value",
    colour = "Site"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p1)
ggsave("figures/canal_timeseries_by_variable.png", p1, width = 12, height = 8, dpi = 300)
message("Saved figures/canal_timeseries_by_variable.png")

# 5b. Time-series faceted by site (variables overlaid)
p2 <- ggplot(canal_obs, aes(x = timestamp, y = value, colour = variable)) +
  geom_line(alpha = 0.7, linewidth = 0.4) +
  facet_wrap(~ site, scales = "free_y") +
  labs(
    title  = "Canal Sites – Variables by Site",
    x      = "Date",
    y      = "Value",
    colour = "Variable"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p2)
ggsave("figures/canal_timeseries_by_site.png", p2, width = 12, height = 8, dpi = 300)
message("Saved figures/canal_timeseries_by_site.png")

# 5c. Boxplots of each variable across sites
p3 <- ggplot(canal_obs, aes(x = site, y = value, fill = site)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  facet_wrap(~ var_label, scales = "free_y") +
  labs(
    title = "Canal Sites – Distribution by Variable",
    x     = NULL,
    y     = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
print(p3)
ggsave("figures/canal_boxplots.png", p3, width = 12, height = 8, dpi = 300)
message("Saved figures/canal_boxplots.png")

message("\nDone. Data and plots saved.")
