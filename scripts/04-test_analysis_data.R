#### Preamble ####
# Purpose: Tests the cleaned parking tickets, construction sites, and the
#   construction analysis outputs
# Author: Furkan Danisman
# Date: 28 September 2026
# Contact: furkandanisman@gmail.com
# License: MIT
# Pre-requisites: Run scripts 01, 02 and 06 (06 for the analysis outputs)


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(testthat)

study_years <- c(2010:2019, 2022:2025)
tickets <- open_dataset("data/analysis_data/parking_tickets")

by_year <-
  tickets |>
  group_by(year) |>
  summarise(
    tickets = n(),
    located = sum(!is.na(centreline_id)),
    min_date = min(date_of_infraction),
    max_date = max(date_of_infraction),
    min_fine = min(set_fine_amount, na.rm = TRUE),
    max_fine = max(set_fine_amount, na.rm = TRUE)
  ) |>
  collect() |>
  arrange(year)


#### Parking tickets ####
test_that("there is one cleaned file for each study year", {
  files <- list.files("data/analysis_data/parking_tickets")
  expect_setequal(files, sprintf("parking_tickets_%d.parquet", study_years))
})

test_that("cleaned tickets have the expected columns", {
  expect_true(all(c(
    "date_of_infraction", "year", "infraction_code", "infraction_description",
    "set_fine_amount", "time_of_infraction", "hour", "time_period",
    "location2", "province", "longitude", "latitude", "centreline_id"
  ) %in% names(tickets)))
})

test_that("each year has about two million tickets", {
  known <- filter(by_year, !is.na(year))
  expect_setequal(known$year, study_years)
  expect_true(all(between(known$tickets, 1.5e6, 3e6)))
})

test_that("almost every ticket has a valid date inside its year", {
  expect_lte(sum(by_year$tickets[is.na(by_year$year)]), 10)
  known <- filter(by_year, !is.na(year))
  expect_true(all(year(known$min_date) == known$year))
  expect_true(all(year(known$max_date) == known$year))
})

test_that("the published 2013 file stops on 18 November 2013", {
  expect_equal(by_year$max_date[by_year$year %in% 2013], as.Date("2013-11-18"))
})

test_that("hours are clock hours and match their time-of-day window", {
  periods <-
    tickets |>
    count(hour, time_period) |>
    collect()
  expect_true(all(periods$hour %in% c(0:23, NA)))
  expected_period <- case_when(
    periods$hour < 6 ~ "Overnight",
    periods$hour < 9 ~ "Morning",
    periods$hour < 17 ~ "Working hours",
    !is.na(periods$hour) ~ "After work"
  )
  expect_identical(periods$time_period, expected_period)
  expect_lt(sum(periods$n[is.na(periods$hour)]) / sum(periods$n), 0.001)
})

test_that("fines are between $0 and $1,000", {
  expect_true(all(by_year$min_fine >= 0, na.rm = TRUE))
  expect_true(all(by_year$max_fine <= 1000, na.rm = TRUE))
})

test_that("at least 80% of tickets are placed on a street block every year", {
  known <- filter(by_year, !is.na(year))
  expect_true(all(known$located / known$tickets > 0.8))
})

test_that("ticket coordinates fall inside Toronto", {
  extent <-
    tickets |>
    summarise(
      min_lon = min(longitude, na.rm = TRUE), max_lon = max(longitude, na.rm = TRUE),
      min_lat = min(latitude, na.rm = TRUE), max_lat = max(latitude, na.rm = TRUE)
    ) |>
    collect()
  expect_gte(extent$min_lon, -79.64)
  expect_lte(extent$max_lon, -79.11)
  expect_gte(extent$min_lat, 43.58)
  expect_lte(extent$max_lat, 43.86)
})

test_that("every located ticket is on a real parking street", {
  parking_streets <-
    sf::st_read("data/raw_data/raw_centreline.gpkg", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    filter(road_class %in% c("Local", "Collector", "Minor Arterial", "Major Arterial"))
  ticket_blocks <-
    tickets |>
    filter(!is.na(centreline_id)) |>
    distinct(centreline_id) |>
    collect()
  expect_true(all(ticket_blocks$centreline_id %in% parking_streets$centreline_id))
})


#### Construction sites ####
construction <- read_parquet("data/analysis_data/construction_sites.parquet")

test_that("construction sites are new buildings or demolitions", {
  expect_setequal(unique(construction$work), c("New Building", "Demolition"))
})

test_that("active periods respect the caps", {
  months_active <- interval(construction$start_date, construction$end_date) %/% months(1)
  expect_true(all(months_active[construction$work == "Demolition"] <= 6, na.rm = TRUE))
  expect_true(all(months_active[construction$work == "New Building"] <= 36, na.rm = TRUE))
})

test_that("no site ends before it starts", {
  expect_true(all(construction$end_date >= construction$start_date))
})

test_that("at least 85% of sites are placed on a street block", {
  expect_gt(mean(!is.na(construction$centreline_id)), 0.85)
})


#### Construction analysis outputs ####
groups <- read_csv("data/analysis_data/construction_did_groups.csv", show_col_types = FALSE)
effects <- read_csv("data/analysis_data/construction_did_effects.csv", show_col_types = FALSE)

test_that("both groups are observed in every month of the event window", {
  expect_equal(nrow(groups), 2 * length(-12:24))
  expect_setequal(unique(groups$event_month), -12:24)
})

test_that("estimates lie inside their confidence intervals and p-values are valid", {
  expect_true(all(effects$conf.low <= effects$estimate & effects$estimate <= effects$conf.high))
  expect_true(all(between(effects$p.value, 0, 1)))
})

test_that("treatment and control were not significantly different a year before", {
  pre <- filter(effects, comparison == "Nearby blocks, no construction in window", period == "-12 to -7")
  expect_gt(pre$p.value, 0.05)
})
