#### Preamble ####
# Purpose: Tests the simulated tickets, construction sites and block x month
#   panel, so the same checks can later be trusted on the real data
# Author: Furkan Danisman
# Date: 28 September 2026
# Contact: furkandanisman@gmail.com
# License: MIT
# Pre-requisites: Run scripts/00-simulate_data.R


#### Workspace setup ####
library(tidyverse)
library(testthat)

tickets <- read_csv(
  "data/simulated_data/simulated_tickets.csv",
  col_types = cols(.default = col_character())
)
construction <- read_csv("data/simulated_data/simulated_construction.csv", show_col_types = FALSE)
block_months <- read_csv("data/simulated_data/simulated_block_months.csv", show_col_types = FALSE)


#### Tickets ####
test_that("tickets have exactly the raw Open Data Toronto columns", {
  expect_identical(
    names(tickets),
    c(
      "tag_number_masked", "date_of_infraction", "infraction_code",
      "infraction_description", "set_fine_amount", "time_of_infraction",
      "location1", "location2", "location3", "location4", "province"
    )
  )
})

test_that("required ticket fields are never missing", {
  required <- c("tag_number_masked", "date_of_infraction", "infraction_code", "set_fine_amount", "time_of_infraction", "location2")
  expect_false(anyNA(tickets[required]))
})

test_that("tag numbers are masked to their last five digits", {
  expect_true(all(str_detect(tickets$tag_number_masked, "^\\*\\*\\*[0-9]{5}$")))
})

test_that("dates are real YYYYMMDD dates within one year", {
  dates <- ymd(tickets$date_of_infraction, quiet = TRUE)
  expect_false(anyNA(dates))
  expect_equal(n_distinct(year(dates)), 1)
})

test_that("times are valid HHMM clock times without leading zeros", {
  time <- as.integer(tickets$time_of_infraction)
  expect_true(all(time >= 0 & time <= 2359))
  expect_true(all(time %% 100 < 60))
  expect_false(any(str_detect(tickets$time_of_infraction, "^0[0-9]")))
})

test_that("each infraction code has one description and one fine", {
  per_code <- tickets |>
    distinct(infraction_code, infraction_description, set_fine_amount) |>
    count(infraction_code)
  expect_true(all(per_code$n == 1))
  expect_true(all(as.numeric(tickets$set_fine_amount) > 0))
})

test_that("overnight-permit tickets cluster overnight and meter tickets do not", {
  hour <- as.integer(tickets$time_of_infraction) %/% 100
  overnight_share <- tapply(hour < 6, tickets$infraction_code, mean)
  expect_gt(overnight_share[["29"]], 0.4)
  expect_lt(overnight_share[["207"]], 0.05)
})

test_that("weekends have fewer tickets per day than weekdays", {
  weekday <- wday(ymd(tickets$date_of_infraction), week_start = 1)
  per_day <- table(weekday) / c(rep(52, 5), rep(52, 2))
  expect_lt(mean(per_day[6:7]), mean(per_day[1:5]))
})

test_that("intersections have cross streets and addresses do not", {
  intersection <- !str_detect(tickets$location2, "^[0-9]")
  expect_true(all(!is.na(tickets$location4[intersection])))
  expect_true(all(is.na(tickets$location4[!intersection])))
})

test_that("provinces are two-letter codes and mostly Ontario", {
  expect_true(all(str_detect(tickets$province, "^[A-Z]{2}$")))
  expect_gt(mean(tickets$province == "ON"), 0.9)
})


#### Construction sites ####
test_that("construction sites are new buildings or demolitions", {
  expect_true(all(construction$work %in% c("New Building", "Demolition")))
})

test_that("sites end on or after they start, within their capped length", {
  months_active <- interval(construction$start_date, construction$end_date) %/% months(1)
  expect_true(all(construction$end_date >= construction$start_date))
  expect_true(all(months_active[construction$work == "Demolition"] <= 6))
  expect_true(all(months_active[construction$work == "New Building"] <= 36))
})

test_that("project identifiers are unique", {
  expect_equal(n_distinct(construction$project), nrow(construction))
})


#### Block x month panel ####
test_that("the panel has one row per block per month", {
  expect_equal(nrow(block_months), nrow(distinct(block_months, centreline_id, month)))
  expect_equal(nrow(block_months), n_distinct(block_months$centreline_id) * n_distinct(block_months$month))
})

test_that("ticket counts are non-negative whole numbers", {
  expect_true(all(block_months$tickets >= 0))
  expect_true(all(block_months$tickets == round(block_months$tickets)))
})

test_that("construction months match the construction sites", {
  active_blocks <- unique(block_months$centreline_id[block_months$construction_active])
  expect_setequal(active_blocks, unique(construction$centreline_id))
})

test_that("a Poisson regression recovers the built-in +8% construction effect", {
  model <- glm(
    tickets ~ construction_active + factor(centreline_id) + factor(year(month)),
    family = poisson, data = block_months
  )
  effect <- exp(coef(model)[["construction_activeTRUE"]]) - 1
  expect_gt(effect, 0.03)
  expect_lt(effect, 0.13)
})
