#### Preamble ####
# Purpose: Prepares the small datasets the paper reads: map layers and ticket
#   locations for the 2025 time-of-day heatmap, and summary tables of the
#   tickets and construction sites
# Author: Furkan Danisman
# Date: 28 September 2026
# Contact: furkandanisman@gmail.com
# License: MIT
# Pre-requisites: Run scripts/01-download_data.R and scripts/02-clean_data.R


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(sf)


#### Settings ####
# Toronto's street grid is tilted about 16.7 degrees east of true north;
# rotating by that angle lines the grid up with the page
grid_angle <- 16.7 * pi / 180
metric_crs <- 32617 # UTM zone 17N, metres

time_periods <- tribble(
  ~time_period, ~hours, ~label,
  "Overnight", 6, "Overnight (00:00-05:59)",
  "Morning", 3, "Morning (06:00-08:59)",
  "Working hours", 8, "Working hours (09:00-16:59)",
  "After work", 7, "After work (17:00-23:59)"
)


#### Helpers ####
rotation <- matrix(c(cos(grid_angle), sin(grid_angle), -sin(grid_angle), cos(grid_angle)), 2)

rotate_sf <- function(layer, pivot) {
  geometry <- (st_geometry(layer) - pivot) * rotation
  st_geometry(layer) <- st_sfc(geometry, crs = NA_crs_)
  layer
}


#### Map layers ####
municipalities <-
  st_read("data/raw_data/raw_former_municipalities.gpkg", quiet = TRUE) |>
  st_transform(metric_crs) |>
  select(name = AREA_NAME)
pivot <- st_coordinates(st_centroid(st_union(municipalities)))[1, ]
municipalities <- rotate_sf(municipalities, pivot)
land <- st_union(municipalities)

parks <-
  st_read("data/raw_data/raw_green_spaces.gpkg", quiet = TRUE) |>
  st_transform(metric_crs) |>
  rotate_sf(pivot) |>
  st_make_valid() |>
  st_geometry() |>
  st_intersection(land)

main_roads <-
  st_read("data/raw_data/raw_centreline.gpkg", quiet = TRUE) |>
  filter(str_detect(road_class, "Arterial|Expressway")) |>
  st_transform(metric_crs) |>
  rotate_sf(pivot) |>
  st_geometry()

district_points <- st_coordinates(st_point_on_surface(st_geometry(municipalities)))
district_labels <- tibble(
  district = str_to_title(municipalities$name) |> str_replace("^Toronto$", "Old Toronto"),
  x = district_points[, 1],
  y = district_points[, 2]
)

heatmap_layers <- list(
  land = land,
  parks = parks,
  main_roads = main_roads,
  municipalities = st_geometry(municipalities),
  district_labels = district_labels
)


#### Ticket locations, 2025 ####
tickets_2025 <-
  read_parquet("data/analysis_data/parking_tickets/parking_tickets_2025.parquet") |>
  filter(!is.na(longitude), !is.na(time_period))

ticket_xy <-
  tickets_2025 |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(metric_crs) |>
  rotate_sf(pivot) |>
  st_coordinates()

# Each ticket is weighted by 1 / (hours in its window x 365 days), so a
# hexagon's value is its average tickets per hour in that window on a 2025
# day, which is comparable across windows of different lengths
heatmap_tickets <-
  tibble(x = ticket_xy[, 1], y = ticket_xy[, 2], time_period = tickets_2025$time_period) |>
  left_join(time_periods, by = "time_period") |>
  transmute(x, y, label, weight = 1 / (hours * 365))


#### Summary tables ####
tickets <- open_dataset("data/analysis_data/parking_tickets")

tickets_by_year <-
  tickets |>
  filter(!is.na(year)) |>
  group_by(year) |>
  summarise(
    tickets = n(),
    located = sum(!is.na(centreline_id)),
    mean_fine = mean(set_fine_amount, na.rm = TRUE),
    fines_total = sum(set_fine_amount, na.rm = TRUE)
  ) |>
  collect() |>
  arrange(year) |>
  mutate(share_located = located / tickets)

top_infractions_2025 <-
  tickets |>
  filter(year == 2025) |>
  count(infraction_code, infraction_description, set_fine_amount, time_period) |>
  collect() |>
  group_by(infraction_code) |>
  summarise(
    description = infraction_description[which.max(n)],
    fine = set_fine_amount[which.max(n)],
    tickets = sum(n),
    overnight_share = sum(n[time_period %in% "Overnight"]) / sum(n)
  ) |>
  mutate(share = tickets / sum(tickets)) |>
  slice_max(tickets, n = 8)

tickets_by_hour_2025 <-
  tickets |>
  filter(year == 2025, !is.na(hour)) |>
  count(hour, time_period) |>
  collect() |>
  arrange(hour)

construction_sites <- read_parquet("data/analysis_data/construction_sites.parquet")
construction_summary <-
  construction_sites |>
  filter(end_date >= as.Date("2010-01-01"), start_date <= as.Date("2025-12-31")) |>
  mutate(months_active = interval(start_date, end_date) %/% months(1)) |>
  group_by(work) |>
  summarise(
    sites = n(),
    located = mean(!is.na(centreline_id)),
    median_months_active = median(months_active[months_active >= 0])
  )


#### Save ####
dir.create("data/analysis_data/paper", recursive = TRUE, showWarnings = FALSE)
saveRDS(heatmap_layers, "data/analysis_data/paper/heatmap_layers.rds")
write_parquet(heatmap_tickets, "data/analysis_data/paper/heatmap_tickets_2025.parquet")
write_csv(tickets_by_year, "data/analysis_data/paper/tickets_by_year.csv")
write_csv(top_infractions_2025, "data/analysis_data/paper/top_infractions_2025.csv")
write_csv(tickets_by_hour_2025, "data/analysis_data/paper/tickets_by_hour_2025.csv")
write_csv(construction_summary, "data/analysis_data/paper/construction_summary.csv")
