#### Preamble ####
# Purpose: Matched difference-in-differences: do street blocks get more
#   parking tickets once construction (a new building or demolition) starts on
#   them? Each construction block is compared, over the same calendar months,
#   with nearby blocks of the same street type that had no construction in
#   that window and had the most similar ticket level and trend in the year
#   before construction started.
# Author: Furkan Danisman
# Date: 28 September 2026
# Contact: furkandanisman@gmail.com
# License: MIT
# Pre-requisites: Run scripts/01-download_data.R and scripts/02-clean_data.R


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(fixest)
library(sf)


#### Settings ####
study_years <- c(2010:2019, 2022:2025)
# The published 2013 file stops on 18 November 2013, so November and
# December 2013 are incomplete and left out
incomplete_months <- as.Date(c("2013-11-01", "2013-12-01"))
study_months <-
  seq(as.Date("2010-01-01"), as.Date("2025-12-01"), by = "month") |>
  keep(\(m) year(m) %in% study_years & !(m %in% incomplete_months))
event_window <- -12:24 # months relative to a block's first construction start

# Matching rules for comparison blocks
max_distance <- 1500 # metres: close enough to share local trends
min_distance <- 250 # metres: far enough to avoid spillover from the site
controls_per_block <- 5


#### Tickets per block per month ####
block_month_tickets <-
  open_dataset("data/analysis_data/parking_tickets") |>
  filter(!is.na(centreline_id), !is.na(date_of_infraction)) |>
  mutate(year = year(date_of_infraction), month = month(date_of_infraction)) |>
  count(centreline_id, year, month, name = "tickets") |>
  collect() |>
  mutate(month = make_date(year, month, 1)) |>
  select(-year)

# Every block that ever received a ticket, in every study month (zeros kept)
panel <-
  expand_grid(
    centreline_id = unique(block_month_tickets$centreline_id),
    month = study_months
  ) |>
  left_join(block_month_tickets, by = c("centreline_id", "month")) |>
  mutate(tickets = replace_na(tickets, 0L))


#### Construction per block per month ####
construction_sites <-
  read_parquet("data/analysis_data/construction_sites.parquet") |>
  filter(!is.na(centreline_id), end_date >= min(study_months))

# One row per site per month it was active
site_months <-
  construction_sites |>
  mutate(
    first_month = floor_date(start_date, "month"),
    last_month = floor_date(end_date, "month")
  ) |>
  filter(last_month >= first_month) |>
  mutate(month = map2(first_month, last_month, \(a, b) seq(a, b, by = "month"))) |>
  unnest(month) |>
  count(centreline_id, month, name = "active_sites")

# First construction start on each block, for the before/after picture
first_start <-
  construction_sites |>
  group_by(centreline_id) |>
  summarise(first_start = floor_date(min(start_date), "month"))

panel <-
  panel |>
  left_join(site_months, by = c("centreline_id", "month")) |>
  mutate(
    active_sites = replace_na(active_sites, 0L),
    construction_active = active_sites > 0
  ) |>
  left_join(first_start, by = "centreline_id") |>
  mutate(
    months_since_start = (year(month) - year(first_start)) * 12 +
      (month(month) - month(first_start))
  )


#### Block locations and street type ####
blocks <-
  st_read("data/raw_data/raw_centreline.gpkg", quiet = TRUE) |>
  filter(centreline_id %in% panel$centreline_id) |>
  distinct(centreline_id, .keep_all = TRUE) |>
  st_transform(32617)
block_points <- st_point_on_surface(st_geometry(blocks))
block_ids <- blocks$centreline_id
block_class <- blocks$road_class


#### Block x month matrices ####
# Tickets (NA in months outside the study period) and active construction,
# over every calendar month 2010-2025, with rows in the order of block_ids
all_months <- seq(as.Date("2010-01-01"), as.Date("2025-12-01"), by = "month")
n_months <- length(all_months)

tickets_matrix <- matrix(NA_real_, length(block_ids), n_months)
panel_index <- cbind(match(panel$centreline_id, block_ids), match(panel$month, all_months))
tickets_matrix[panel_index] <- panel$tickets

active_matrix <- matrix(FALSE, length(block_ids), n_months)
active_index <-
  site_months |>
  filter(centreline_id %in% block_ids, month %in% all_months) |>
  transmute(row = match(centreline_id, block_ids), col = match(month, all_months)) |>
  as.matrix()
active_matrix[active_index] <- TRUE

first_start_index <- match(
  first_start$first_start[match(block_ids, first_start$centreline_id)],
  all_months
)


#### Construction blocks ####
# First construction started with a fully observed year before it
pre_offsets <- -12:-1
treated <-
  tibble(row = seq_along(block_ids), start = first_start_index) |>
  filter(!is.na(start), start > 12) |>
  filter(map_lgl(row, \(r) {
    s <- first_start_index[r]
    !anyNA(tickets_matrix[r, s + pre_offsets])
  }))


#### Pre-period features for matching ####
# Level and trend of log(1 + tickets) over the 12 months before the start
pre_features <- function(rows, start) {
  y <- log1p(tickets_matrix[rows, start + pre_offsets, drop = FALSE])
  time <- pre_offsets - mean(pre_offsets)
  cbind(level = rowMeans(y), trend = as.vector(y %*% time) / sum(time^2))
}

treated_features <- t(mapply(\(r, s) pre_features(r, s), treated$row, treated$start))
feature_scale <- apply(treated_features, 2, sd)


#### Matching ####
nearby <- st_is_within_distance(block_points[treated$row], block_points, dist = max_distance)
too_close <- st_is_within_distance(block_points[treated$row], block_points, dist = min_distance)

match_controls <- function(require_later_construction) {
  map_dfr(seq_len(nrow(treated)), \(i) {
    r <- treated$row[i]
    s <- treated$start[i]
    window <- max(1, s + min(event_window)):min(n_months, s + max(event_window))
    candidates <- setdiff(nearby[[i]], too_close[[i]])
    candidates <- candidates[block_class[candidates] == block_class[r]]
    # No construction anywhere in the comparison window
    candidates <- candidates[rowSums(active_matrix[candidates, window, drop = FALSE]) == 0]
    if (require_later_construction) {
      later <- first_start_index[candidates]
      candidates <- candidates[!is.na(later) & later > s + max(event_window)]
    }
    if (length(candidates) == 0) {
      return(NULL)
    }
    candidate_features <- pre_features(candidates, s)
    distance <- sqrt(colSums(((t(candidate_features) - treated_features[i, ]) / feature_scale)^2))
    chosen <- candidates[order(distance)][seq_len(min(controls_per_block, length(candidates)))]
    tibble(
      stack = i,
      row = c(r, chosen),
      start = s,
      construction_block = c(TRUE, rep(FALSE, length(chosen))),
      # Each stack's comparison blocks together weigh as much as its construction block
      weight = c(1, rep(1 / length(chosen), length(chosen)))
    )
  })
}


#### Stacked panel ####
build_stacked_panel <- function(matches) {
  matches |>
    cross_join(tibble(event_month = event_window)) |>
    mutate(col = start + event_month) |>
    filter(col >= 1, col <= n_months) |>
    mutate(
      tickets = tickets_matrix[cbind(row, col)],
      centreline_id = block_ids[row],
      unit = str_c(stack, "-", row),
      started = construction_block & event_month >= 0,
      # Six-month bins; the six months just before the start are the baseline
      period = cut(
        event_month,
        breaks = c(-13, -7, -1, 6, 12, 18, 24),
        labels = c("-12 to -7", "-6 to -1", "0 to 6", "7 to 12", "13 to 18", "19 to 24")
      )
    ) |>
    filter(!is.na(tickets))
}

# Difference-in-differences on average tickets per block per month.
# Unit fixed effects remove each block's usual level; stack-by-month fixed
# effects remove everything a construction block shares with its own matched
# blocks in that calendar month. Each stack's matched blocks together weigh
# as much as its construction block, so the estimate is exactly the change in
# the gap between the two group averages plotted in the figure.
fit_did <- function(stacked) {
  list(
    overall = feols(
      tickets ~ started | unit + stack^event_month,
      data = stacked, weights = ~weight, cluster = ~centreline_id, lean = TRUE
    ),
    by_period = feols(
      tickets ~ i(period, construction_block, ref = "-6 to -1") | unit + stack^event_month,
      data = stacked, weights = ~weight, cluster = ~centreline_id, lean = TRUE
    )
  )
}

# Average tickets per construction block per month before the start, used to
# express the estimates as a percentage
pre_mean <- function(stacked) {
  mean(stacked$tickets[stacked$construction_block & stacked$event_month < 0])
}

tidy_effects <- function(models, stacked, comparison) {
  overall <-
    broom::tidy(models$overall, conf.int = TRUE) |>
    mutate(period = "All months after start")
  by_period <-
    broom::tidy(models$by_period, conf.int = TRUE) |>
    mutate(period = str_extract(term, "(?<=period::).+(?=:construction_block)"))
  bind_rows(overall, by_period) |>
    mutate(
      comparison = comparison,
      construction_blocks = n_distinct(stacked$stack),
      pre_mean = pre_mean(stacked),
      percent_of_pre_mean = estimate / pre_mean
    ) |>
    select(comparison, period, estimate, conf.low, conf.high, p.value, percent_of_pre_mean, construction_blocks)
}

# Main: nearby, same street type, no construction in the window
main_matches <- match_controls(require_later_construction = FALSE)
main_panel <- build_stacked_panel(main_matches)
main_models <- fit_did(main_panel)

# Check: comparison blocks must themselves get construction after the window
later_matches <- match_controls(require_later_construction = TRUE)
later_panel <- build_stacked_panel(later_matches)
later_models <- fit_did(later_panel)

# Robustness of the overall estimate to reasonable choices
balanced_stacks <-
  main_panel |>
  filter(construction_block) |>
  count(stack) |>
  filter(n == length(event_window)) |>
  pull(stack)
balanced_panel <- filter(main_panel, stack %in% balanced_stacks)

robustness <- list(
  "Only blocks observed in every month of the window" = list(
    model = feols(
      tickets ~ started | unit + stack^event_month,
      data = balanced_panel, weights = ~weight, cluster = ~centreline_id, lean = TRUE
    ),
    data = balanced_panel,
    percent_model = FALSE
  ),
  # Poisson: the average percentage change per block, which gives quiet
  # blocks (a few tickets a year) as much say as busy ones
  "Percentage change per block (Poisson)" = list(
    model = fepois(
      tickets ~ started | unit + stack^event_month,
      data = main_panel, weights = ~weight, cluster = ~centreline_id, lean = TRUE
    ),
    data = main_panel,
    percent_model = TRUE
  )
)

did_effects <- bind_rows(
  tidy_effects(main_models, main_panel, "Nearby blocks, no construction in window"),
  tidy_effects(later_models, later_panel, "Nearby blocks that get construction later"),
  imap_dfr(robustness, \(r, name) {
    broom::tidy(r$model, conf.int = TRUE) |>
      mutate(
        comparison = name,
        period = "All months after start",
        construction_blocks = n_distinct(r$data$stack),
        percent_of_pre_mean = if (r$percent_model) exp(estimate) - 1 else estimate / pre_mean(r$data),
        across(c(estimate, conf.low, conf.high), \(x) if (r$percent_model) exp(x) - 1 else x)
      ) |>
      select(comparison, period, estimate, conf.low, conf.high, p.value, percent_of_pre_mean, construction_blocks)
  })
)


#### Summaries ####
summarise_matches <- function(matches, comparison) {
  matches |>
    group_by(stack) |>
    summarise(controls = sum(!construction_block)) |>
    summarise(
      construction_blocks = n(),
      median_comparison_blocks = median(controls)
    ) |>
    mutate(comparison = comparison)
}
did_summary <- bind_rows(
  summarise_matches(main_matches, "Nearby blocks, no construction in window"),
  summarise_matches(later_matches, "Nearby blocks that get construction later")
)

# Pre-period balance: construction vs. matched comparison blocks
balance <-
  main_matches |>
  mutate(features = map2(row, start, \(r, s) as_tibble(pre_features(r, s)))) |>
  unnest(features) |>
  group_by(construction_block) |>
  summarise(pre_level = mean(level), pre_trend = mean(trend))

period_effects <-
  did_effects |>
  filter(comparison == "Nearby blocks, no construction in window", period != "All months after start") |>
  bind_rows(tibble(period = "-6 to -1", estimate = 0, conf.low = 0, conf.high = 0, p.value = NA)) |>
  mutate(
    period = factor(period, levels = levels(main_panel$period)),
    started = !(period %in% c("-12 to -7", "-6 to -1")),
    label = case_when(
      is.na(p.value) ~ "reference period",
      p.value < 0.001 ~ "p < 0.001",
      .default = str_c("p = ", format(round(p.value, 3), nsmall = 3))
    )
  )

# Treatment and control group averages: construction blocks, and their
# matched blocks (each construction block's matched blocks averaged, so every
# construction block and its control group count once)
did_groups <-
  main_panel |>
  group_by(event_month, construction_block) |>
  summarise(tickets = weighted.mean(tickets, weight), .groups = "drop") |>
  mutate(group = if_else(
    construction_block,
    "Treatment: blocks with construction",
    "Control: matched nearby blocks without construction"
  ))


#### Save ####
dir.create("models", showWarnings = FALSE)
saveRDS(main_models, "models/construction_did_models.rds")
saveRDS(later_models, "models/construction_did_later_models.rds")
write_csv(did_summary, "data/analysis_data/construction_did_summary.csv")
write_csv(did_effects, "data/analysis_data/construction_did_effects.csv")
write_csv(balance, "data/analysis_data/construction_did_balance.csv")
write_csv(did_groups, "data/analysis_data/construction_did_groups.csv")
write_csv(select(period_effects, -started, -label), "data/analysis_data/construction_did_period_effects.csv")
