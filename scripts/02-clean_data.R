#### Preamble ####
# Purpose: Cleans parking tickets (2010-2019, 2022-2025) and building permits,
#   and places both on the street network: each street address is matched to
#   Toronto's address points for coordinates, then to its nearest street
#   block (a centreline segment between two intersections)
# Author: Furkan Danisman
# Date: 28 September 2026
# Contact: furkandanisman@gmail.com
# License: MIT
# Pre-requisites: Run scripts/01-download_data.R


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(sf)


#### Address normalization ####
# Tickets, permits and address points spell street types differently
# (e.g. "AVENUE" vs "Ave", "BLVD." vs "Blvd"), so all are put into one
# uppercase, abbreviated form before joining
normalize_address <- function(x) {
  x |>
    toupper() |>
    str_replace_all("\\.", "") |>
    str_squish() |>
    # "31 A PARLIAMENT ST" -> "31A PARLIAMENT ST"
    str_replace("^([0-9]+) ([A-Z]) ", "\\1\\2 ") |>
    str_replace_all(c(
      "\\bAVENUE\\b" = "AVE",
      "\\bSTREET\\b" = "ST",
      "\\bROAD\\b" = "RD",
      "\\bBOULEVARD\\b" = "BLVD",
      "\\bDRIVE\\b" = "DR",
      "\\bCRESCENT\\b" = "CRES",
      "\\bCOURT\\b" = "CRT",
      "\\bPLACE\\b" = "PL",
      "\\bSQUARE\\b" = "SQ",
      "\\bTERRACE\\b" = "TER",
      "\\bPARKWAY\\b" = "PKWY",
      "\\bCIRCLE\\b" = "CIR",
      "\\bGARDENS\\b" = "GDNS",
      "\\bEAST\\b" = "E",
      "\\bWEST\\b" = "W",
      "\\bNORTH\\b" = "N",
      "\\bSOUTH\\b" = "S"
    ))
}


#### Address points -> coordinates and street block ####
# Streets where parking happens; laneways, expressways and trails excluded
parking_street_classes <- c("Local", "Collector", "Minor Arterial", "Major Arterial")

parking_streets <-
  st_read("data/raw_data/raw_centreline.gpkg", quiet = TRUE) |>
  filter(road_class %in% parking_street_classes) |>
  st_transform(32617)

address_points <-
  read_csv("data/raw_data/raw_address_points.csv", show_col_types = FALSE) |>
  mutate(address_key = normalize_address(address_full)) |>
  filter(!is.na(longitude)) |>
  distinct(address_key, .keep_all = TRUE)

nearest_street <-
  address_points |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(32617) |>
  st_nearest_feature(parking_streets)

address_points <-
  address_points |>
  mutate(centreline_id = parking_streets$centreline_id[nearest_street]) |>
  select(address_key, longitude, latitude, centreline_id)


#### Clean tickets ####
clean_tickets <- function(raw_tickets) {
  raw_tickets |>
    # A few addresses carry stray Latin-1 bytes (e.g. "100 I\xcd YORKVILLE AVE")
    mutate(across(
      where(is.character),
      \(x) if_else(validUTF8(x), x, iconv(x, from = "latin1", to = "UTF-8"))
    )) |>
    mutate(
      date_of_infraction = ymd(date_of_infraction),
      year = year(date_of_infraction),
      time_of_infraction = str_pad(time_of_infraction, 4, pad = "0"),
      # Hour of day; "24xx" and missing times are not valid clock times
      hour = as.integer(str_sub(time_of_infraction, 1, 2)),
      hour = if_else(hour %in% 0:23, hour, NA_integer_),
      time_period = case_when(
        hour < 6 ~ "Overnight",
        hour < 9 ~ "Morning",
        hour < 17 ~ "Working hours",
        !is.na(hour) ~ "After work"
      ),
      set_fine_amount = as.numeric(set_fine_amount),
      infraction_code = as.integer(infraction_code),
      address_key = normalize_address(location2)
    ) |>
    left_join(address_points, by = "address_key") |>
    select(-address_key, -tag_number_masked)
}

dir.create("data/analysis_data/parking_tickets", recursive = TRUE, showWarnings = FALSE)
raw_ticket_files <- list.files("data/raw_data/parking_tickets", full.names = TRUE)
for (raw_file in raw_ticket_files) {
  read_parquet(raw_file) |>
    clean_tickets() |>
    write_parquet(file.path(
      "data/analysis_data/parking_tickets",
      str_remove(basename(raw_file), "^raw_")
    ))
}


#### Clean building permits into construction sites ####
# Only new buildings and demolitions: the sites that bring hoarding, trucks
# and blocked curbs. Interior, plumbing and mechanical permits are dropped.
construction_work <- c("New Building", "Demolition")

# The city's "completed" date is often an administrative close-out long after
# work ends, and some permits never close, so each site's active period is
# capped at a typical duration from the date the permit was issued:
# 3 years for a new building, 6 months for a demolition
last_ticket_date <- as.Date("2025-12-31")

construction_sites <-
  read_parquet("data/raw_data/raw_building_permits.parquet") |>
  filter(WORK %in% construction_work) |>
  mutate(
    issued_date = as.Date(ymd(ISSUED_DATE, quiet = TRUE)),
    completed_date = as.Date(ymd(COMPLETED_DATE, quiet = TRUE)),
    # "20 230641 DRN" -> "20 230641": one project across revisions
    project = str_extract(PERMIT_NUM, "^[0-9]+ [0-9]+"),
    address_key = normalize_address(
      str_c(
        coalesce(STREET_NUM, ""), coalesce(STREET_NAME, ""),
        coalesce(STREET_TYPE, ""), coalesce(STREET_DIRECTION, ""),
        sep = " "
      )
    )
  ) |>
  filter(!is.na(issued_date)) |>
  group_by(project, work = WORK, address_key) |>
  summarise(
    start_date = min(issued_date),
    completed_date = if (all(is.na(completed_date))) as.Date(NA) else max(completed_date, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    capped_end = if_else(work == "Demolition", start_date %m+% months(6), start_date %m+% years(3)),
    end_date = pmin(coalesce(completed_date, last_ticket_date), capped_end)
  ) |>
  # About 3% of permits record a completion before their issue date, which
  # is a recording error; those sites are dropped
  filter(end_date >= start_date) |>
  left_join(address_points, by = "address_key") |>
  select(project, work, start_date, end_date, centreline_id, longitude, latitude)

write_parquet(construction_sites, "data/analysis_data/construction_sites.parquet")
