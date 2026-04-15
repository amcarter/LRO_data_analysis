# Helper functions for accessing Logan River Observatory data via the
# HydroServer SensorThings API.
#
# This file is intended to be source()'d by other scripts.

library(httr)
library(jsonlite)

BASE_URL <- "https://lro.hydroserver.org/api/sensorthings/v1.1"

#' Fetch a single API response as a parsed list
api_get <- function(path, params = list()) {
  url <- paste0(BASE_URL, "/", path)
  resp <- GET(url, query = params)
  stop_for_status(resp)
  content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
}

#' Follow @iot.nextLink pagination and return all results as a data.frame
fetch_all_pages <- function(path, params = list()) {
  params[["$top"]] <- 100
  url <- paste0(BASE_URL, "/", path)
  all_values <- list()

  repeat {
    resp <- GET(url, query = params)
    stop_for_status(resp)
    data <- content(resp, as = "text", encoding = "UTF-8") |>
      fromJSON(flatten = TRUE)

    values <- data[["value"]]
    if (!is.null(values) && nrow(values) > 0) {
      all_values <- c(all_values, list(values))
    }

    next_link <- data[["@iot.nextLink"]]
    if (is.null(next_link)) break
    # Subsequent pages use the full nextLink URL directly
    url <- next_link
    params <- list()
  }

  if (length(all_values) == 0) return(data.frame())
  do.call(rbind, all_values)
}

#' List all monitoring sites (Things)
#' Returns a data.frame with id, name, code, site_type, lon, lat
list_sites <- function() {
  things <- fetch_all_pages("Things", list(`$expand` = "Locations"))

  df <- data.frame(
    id   = things[["@iot.id"]],
    name = things[["name"]],
    code = things[["properties.samplingFeatureCode"]],
    site_type = things[["properties.siteType"]],
    stringsAsFactors = FALSE
  )

  # Extract coordinates from expanded Locations
  locs <- things[["Locations"]]
  coords <- lapply(locs, function(loc_list) {
    if (is.null(loc_list) || nrow(loc_list) == 0) {
      return(c(lon = NA_real_, lat = NA_real_))
    }
    crds <- loc_list$location.coordinates[[1]]
    if (is.null(crds)) return(c(lon = NA_real_, lat = NA_real_))
    c(lon = crds[1], lat = crds[2])
  })
  coords_df <- do.call(rbind, coords) |> as.data.frame()
  df$lon <- coords_df$lon
  df$lat <- coords_df$lat

  message(sprintf("Found %d sites", nrow(df)))
  df
}

#' List all datastreams for a given site (Thing ID)
#' Returns a data.frame with id, name, unit, value_count, phenomenon_time
list_datastreams <- function(thing_id) {
  path <- sprintf("Things('%s')/Datastreams", thing_id)
  ds <- fetch_all_pages(path)

  if (nrow(ds) == 0) {
    message("No datastreams found for this site.")
    return(data.frame())
  }

  df <- data.frame(
    id              = ds[["@iot.id"]],
    name            = ds[["name"]],
    unit            = ds[["unitOfMeasurement.symbol"]],
    value_count     = ds[["properties.valueCount"]],
    phenomenon_time = ds[["phenomenonTime"]],
    medium          = ds[["properties.sampledMedium"]],
    statistic       = ds[["properties.aggregationStatistic"]],
    stringsAsFactors = FALSE
  )

  message(sprintf("Found %d datastreams", nrow(df)))
  df
}

#' Download observations for a datastream
#' Returns a data.frame with timestamp and value columns
#'
#' @param datastream_id Character. The datastream UUID.
#' @param start Optional start date string (e.g. "2023-01-01").
#' @param end   Optional end date string (e.g. "2023-12-31").
get_observations <- function(datastream_id, start = NULL, end = NULL) {
  path <- sprintf("Datastreams('%s')/Observations", datastream_id)
  params <- list(
    `$orderby` = "phenomenonTime asc",
    `$top`     = 1000
  )

  # Build $filter for time range
  filters <- c()
  if (!is.null(start)) {
    filters <- c(filters, sprintf("phenomenonTime ge %sT00:00:00Z", start))
  }
  if (!is.null(end)) {
    filters <- c(filters, sprintf("phenomenonTime le %sT23:59:59Z", end))
  }
  if (length(filters) > 0) {
    params[["$filter"]] <- paste(filters, collapse = " and ")
  }

  url <- paste0(BASE_URL, "/", path)
  all_values <- list()
  page <- 1

  repeat {
    message(sprintf("Fetching page %d ...", page))
    resp <- GET(url, query = params)
    stop_for_status(resp)
    data <- content(resp, as = "text", encoding = "UTF-8") |>
      fromJSON(flatten = TRUE)

    values <- data[["value"]]
    if (!is.null(values) && nrow(values) > 0) {
      all_values <- c(all_values, list(values))
    }

    next_link <- data[["@iot.nextLink"]]
    if (is.null(next_link)) break
    url <- next_link
    params <- list()
    page <- page + 1
  }

  if (length(all_values) == 0) {
    message("No observations found.")
    return(data.frame(timestamp = character(), value = numeric()))
  }

  obs <- do.call(rbind, all_values)
  df <- data.frame(
    timestamp = as.POSIXct(obs[["phenomenonTime"]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    value     = as.numeric(obs[["result"]]),
    stringsAsFactors = FALSE
  )

  message(sprintf("Downloaded %d observations", nrow(df)))
  df
}
