#### Preamble ####
# Purpose: Downloads parking tickets (2010-2019, 2022-2025), building permits,
#   address points, and map layers from Open Data Toronto
# Author: Furkan Danisman
# Date: 27 September 2026
# Contact: furkandanisman@gmail.com
# License: MIT
# Pre-requisites: install.packages(c("opendatatoronto", "tidyverse", "arrow", "sf", "jsonlite"))


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(arrow)


#### Helpers ####
# opendatatoronto finds each resource; the file itself is downloaded directly.
# get_resource() silently truncates some large files (it returned 1.42M of
# the 2.75M tickets in 2012, and only the first 32,000 building permits), so
# every ZIP/CSV is read here with readr, which reports any parsing problems.
ckan_api <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/api/3/action/"

resource_url <- function(resource_id) {
  jsonlite::fromJSON(paste0(ckan_api, "resource_show?id=", resource_id))$result$url
}

# Reads a CSV as all-character columns; the 2010 tickets are UTF-16 encoded
read_raw_csv <- function(path) {
  byte_order_mark <- readBin(path, "raw", 2)
  if (identical(byte_order_mark, as.raw(c(0xff, 0xfe)))) {
    utf8_path <- tempfile(fileext = ".csv")
    system2("iconv", c("-f", "UTF-16LE", "-t", "UTF-8", shQuote(path)), stdout = utf8_path)
    path <- utf8_path
  }
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
}

download_csvs <- function(resource_id) {
  url <- resource_url(resource_id)
  path <- file.path(tempdir(), basename(url))
  download.file(url, path, mode = "wb", quiet = TRUE)
  if (str_detect(path, "\\.zip$")) {
    path <- unzip(path, exdir = tempfile())
    path <- path[str_detect(path, regex("\\.csv$", ignore_case = TRUE))]
  }
  path |>
    map(read_raw_csv) |>
    list_rbind()
}


#### Download parking tickets ####
# 2010-2019 and 2022-2025; 2020-2021 are left out because pandemic
# restrictions changed both parking behaviour and enforcement
ticket_years <- c(2010:2019, 2022:2025)

ticket_resources <-
  list_package_resources("parking-tickets") |>
  mutate(year = as.integer(str_extract(name, "[0-9]{4}"))) |>
  filter(year %in% ticket_years)

dir.create("data/raw_data/parking_tickets", recursive = TRUE, showWarnings = FALSE)
for (i in seq_len(nrow(ticket_resources))) {
  download_csvs(ticket_resources$id[i]) |>
    rename_with(tolower) |>
    write_parquet(sprintf(
      "data/raw_data/parking_tickets/raw_parking_tickets_%d.parquet",
      ticket_resources$year[i]
    ))
}


#### Download building permits ####
# Three files together cover permits from 2000 onward: still-open permits,
# permits closed 2000-2016, and permits closed since 2017
permit_files <- c(
  "building-permits-active-permits" = "building-permits-active-permits.csv",
  "building-permits-cleared-permits" = "Cleared Permits 2000 to 2016",
  "building-permits-cleared-permits" = "Cleared Building Permits since 2017.csv"
)

raw_building_permits <-
  imap(permit_files, \(resource_name, package) {
    list_package_resources(package) |>
      filter(name == resource_name) |>
      pull(id) |>
      download_csvs() |>
      mutate(source_file = resource_name)
  }) |>
  list_rbind()


#### Download address points ####
# One row per civic address in Toronto, with WGS84 coordinates; used to place
# each ticket's street address on the map
raw_address_points <-
  list_package_resources("address-points-municipal-toronto-one-address-repository") |>
  filter(name == "Address Points - 4326.csv") |>
  pull(id) |>
  get_resource()

# Keep only the address and its coordinates; geometry is a GeoJSON string
address_coords <- str_match(
  raw_address_points$geometry,
  "\\[\\[(-?[0-9.]+), (-?[0-9.]+)\\]"
)
raw_address_points <-
  tibble(
    address_full = raw_address_points$ADDRESS_FULL,
    longitude = as.numeric(address_coords[, 2]),
    latitude = as.numeric(address_coords[, 3])
  )


#### Download map layers ####
# Street network: every street segment ("block") between two intersections
raw_centreline <-
  list_package_resources("toronto-centreline-tcl") |>
  filter(name == "Centreline - Version 2 - 4326.geojson") |>
  pull(id) |>
  get_resource() |>
  select(
    centreline_id = CENTRELINE_ID,
    street_name = LINEAR_NAME_FULL,
    road_class = FEATURE_CODE_DESC
  )

# Boundaries of the six cities amalgamated in 1998 (Toronto, North York, ...)
raw_former_municipalities <-
  list_package_resources("former-municipality-boundaries") |>
  filter(name == "Former Municipality Boundaries Data - 4326.geojson") |>
  pull(id) |>
  get_resource()

# Parks and other green spaces
raw_green_spaces <-
  list_package_resources("green-spaces") |>
  filter(name == "Green Spaces - 4326.geojson") |>
  pull(id) |>
  get_resource() |>
  select(area_name = AREA_NAME)


#### Save data ####
dir.create("data/raw_data", recursive = TRUE, showWarnings = FALSE)
write_parquet(raw_building_permits, "data/raw_data/raw_building_permits.parquet")
write_csv(raw_address_points, "data/raw_data/raw_address_points.csv")
sf::st_write(raw_centreline, "data/raw_data/raw_centreline.gpkg", delete_dsn = TRUE, quiet = TRUE)
sf::st_write(raw_former_municipalities, "data/raw_data/raw_former_municipalities.gpkg", delete_dsn = TRUE, quiet = TRUE)
sf::st_write(raw_green_spaces, "data/raw_data/raw_green_spaces.gpkg", delete_dsn = TRUE, quiet = TRUE)
