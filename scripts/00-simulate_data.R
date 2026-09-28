#### Preamble ####
# Purpose: Simulates the three datasets used in this project:
#   (1) individual parking tickets, in the same format as the raw Open Data
#       Toronto files;
#   (2) construction sites (new buildings and demolitions) from building
#       permits; and
#   (3) a street block x month panel of ticket counts with a known effect of
#       construction, to check the analysis can recover it.
# Author: Furkan Danisman
# Date: 28 September 2026
# Contact: furkandanisman@gmail.com
# License: MIT
# Pre-requisites: None


#### Workspace setup ####
library(tidyverse)
set.seed(853)


#### (1) Parking tickets ####
n_tickets <- 20000
simulated_year <- 2025

# Common infraction types with their fine and the share of tickets they take;
# overnight_weight is how strongly each type clusters between midnight and 6am
infractions <- tribble(
  ~infraction_code, ~infraction_description, ~set_fine_amount, ~share, ~overnight_weight,
  3, "PARK ON PRIVATE PROPERTY", 75, 0.20, 0.3,
  5, "PARK-SIGNED HWY-PROHIBIT DY/TM", 65, 0.15, 0.1,
  29, "PARK PROHIBITED TIME NO PERMIT", 45, 0.15, 0.6,
  207, "PARK MACHINE-REQD FEE NOT PAID", 50, 0.15, 0.0,
  9, "STOP-SIGNED HWY-PROHIBIT TM/DY", 120, 0.10, 0.0,
  2, "PARK - LONGER THAN 3 HOURS", 40, 0.10, 0.6,
  403, "STOP-SIGNED HIGHWAY-RUSH HOUR", 190, 0.10, 0.0,
  347, "PARK IN A FIRE ROUTE", 250, 0.05, 0.2
)

streets <- c(
  "KING ST W", "QUEEN ST W", "BLOOR ST W", "YONGE ST", "DUNDAS ST W",
  "DANFORTH AVE", "SPADINA AVE", "COLLEGE ST", "PORTLAND ST", "DUFFERIN ST"
)

# Days: fewer tickets on weekends
all_days <- seq(as.Date(str_c(simulated_year, "-01-01")), as.Date(str_c(simulated_year, "-12-31")), by = "day")
day_weight <- if_else(wday(all_days, week_start = 1) >= 6, 0.5, 1)

# Minutes after midnight: overnight tickets fall between 00:00 and 05:59,
# daytime tickets mostly between 08:00 and 20:00
simulate_minutes <- function(overnight) {
  if_else(
    overnight,
    sample(0:359, length(overnight), replace = TRUE),
    pmin(pmax(round(rnorm(length(overnight), mean = 13 * 60, sd = 3.5 * 60)), 360), 1439)
  )
}

simulated_tickets <-
  tibble(
    infraction_code = sample(infractions$infraction_code, n_tickets, replace = TRUE, prob = infractions$share)
  ) |>
  left_join(select(infractions, -share), by = "infraction_code") |>
  mutate(
    date = sample(all_days, n_tickets, replace = TRUE, prob = day_weight),
    overnight = runif(n_tickets) < overnight_weight,
    minutes = simulate_minutes(overnight),
    # Most tickets give a street address; about 10% an intersection instead
    is_intersection = runif(n_tickets) < 0.10,
    street = sample(streets, n_tickets, replace = TRUE),
    cross_street = sample(streets, n_tickets, replace = TRUE),
    number = sample(1:3000, n_tickets, replace = TRUE)
  ) |>
  transmute(
    tag_number_masked = str_c("***", str_pad(sample(0:99999, n_tickets, replace = TRUE), 5, pad = "0")),
    date_of_infraction = format(date, "%Y%m%d"),
    infraction_code,
    infraction_description,
    set_fine_amount,
    # Stored like the raw files: HHMM without leading zeros (00:05 -> "5")
    time_of_infraction = as.character((minutes %/% 60) * 100 + minutes %% 60),
    location1 = if_else(is_intersection, sample(c("E/S", "W/S", "N/S", "S/S"), n_tickets, replace = TRUE), sample(c("NR", "AT", "OPP"), n_tickets, replace = TRUE, prob = c(0.8, 0.15, 0.05))),
    location2 = if_else(is_intersection, street, str_c(number, " ", street)),
    location3 = if_else(is_intersection, sample(c("N/O", "S/O", "E/O", "W/O"), n_tickets, replace = TRUE), NA_character_),
    location4 = if_else(is_intersection, cross_street, NA_character_),
    province = sample(c("ON", "QC", "NY", "MI"), n_tickets, replace = TRUE, prob = c(0.95, 0.03, 0.01, 0.01))
  )


#### (2) Construction sites ####
n_sites <- 500
n_blocks <- 400

simulated_construction <-
  tibble(
    project = str_c(sample(10:25, n_sites, replace = TRUE), " ", sample(100000:999999, n_sites)),
    work = sample(c("New Building", "Demolition"), n_sites, replace = TRUE, prob = c(0.6, 0.4)),
    centreline_id = sample(seq_len(n_blocks), n_sites, replace = TRUE),
    start_date = sample(seq(as.Date("2011-01-01"), as.Date("2023-12-31"), by = "day"), n_sites, replace = TRUE)
  ) |>
  mutate(
    # Active periods are capped: 6 months for a demolition, 3 years for a
    # new building
    max_months = if_else(work == "Demolition", 6, 36),
    months_active = pmin(round(rexp(n_sites, rate = 1 / (max_months / 2))), max_months),
    end_date = start_date %m+% months(months_active)
  ) |>
  select(project, work, centreline_id, start_date, end_date)


#### (3) Block x month panel with a known construction effect ####
months <- seq(as.Date("2010-01-01"), as.Date("2025-12-01"), by = "month")
true_construction_effect <- 0.08 # +8% tickets while a site is active
yearly_trend <- -0.02 # tickets drift down 2% a year citywide

block_baseline <- tibble(
  centreline_id = seq_len(n_blocks),
  # Most blocks get few tickets; a few busy blocks get many
  baseline = rlnorm(n_blocks, meanlog = 0.5, sdlog = 1.2)
)

active <-
  simulated_construction |>
  mutate(month = map2(floor_date(start_date, "month"), floor_date(end_date, "month"), \(a, b) seq(a, b, by = "month"))) |>
  unnest(month) |>
  distinct(centreline_id, month) |>
  mutate(construction_active = TRUE)

simulated_block_months <-
  expand_grid(centreline_id = seq_len(n_blocks), month = months) |>
  left_join(block_baseline, by = "centreline_id") |>
  left_join(active, by = c("centreline_id", "month")) |>
  mutate(
    construction_active = replace_na(construction_active, FALSE),
    years_since_2010 = as.numeric(month - min(months)) / 365.25,
    expected = baseline * exp(yearly_trend * years_since_2010) *
      if_else(construction_active, 1 + true_construction_effect, 1),
    tickets = rpois(n(), expected)
  ) |>
  select(centreline_id, month, construction_active, tickets)


#### Save data ####
dir.create("data/simulated_data", recursive = TRUE, showWarnings = FALSE)
write_csv(simulated_tickets, "data/simulated_data/simulated_tickets.csv", na = "")
write_csv(simulated_construction, "data/simulated_data/simulated_construction.csv")
write_csv(simulated_block_months, "data/simulated_data/simulated_block_months.csv")
