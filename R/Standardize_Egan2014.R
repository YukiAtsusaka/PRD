library(dplyr)
library(tidyr)
library(here)
library(readr)
library(haven)

# 1. Load  
d10 <- read_dta(here("raw", "Egan2014_CCES2010.dta"))
d11 <- read_dta(here("raw", "Egan2014_CCES2011.dta"))
d12 <- read_dta(here("raw", "Egan2014_CCES2012.dta"))

# 2. Items
items <- c("ch_leftist", "ch_rightist", "ch_quo")

# 3. Helper: decode LQR categorical (1-6) into per-option ranks 1-3
decode_lqr <- function(cat_rank) {
  tibble(
    ch_leftist  = case_when(
      cat_rank == 1 ~ 1L, cat_rank == 2 ~ 2L, cat_rank == 3 ~ 1L,
      cat_rank == 4 ~ 3L, cat_rank == 5 ~ 3L, cat_rank == 6 ~ 2L,
      TRUE ~ NA_integer_
    ),
    ch_rightist = case_when(
      cat_rank == 1 ~ 2L, cat_rank == 2 ~ 1L, cat_rank == 3 ~ 3L,
      cat_rank == 4 ~ 1L, cat_rank == 5 ~ 2L, cat_rank == 6 ~ 3L,
      TRUE ~ NA_integer_
    ),
    ch_quo      = case_when(
      cat_rank == 1 ~ 3L, cat_rank == 2 ~ 3L, cat_rank == 3 ~ 2L,
      cat_rank == 4 ~ 2L, cat_rank == 5 ~ 1L, cat_rank == 6 ~ 1L,
      TRUE ~ NA_integer_
    )
  )
}

# 4. Build long-format frames per wave
# 2010: 4 issues with per-issue treat
build_2010 <- function(df) {
  issues <- c("educ", "guan", "imm", "oil")
  bind_rows(lapply(issues, function(iss) {
    rank_col  <- paste0(iss, "_rank")
    treat_col <- paste0(iss, "_treat")
    out <- decode_lqr(as.integer(df[[rank_col]]))
    out$wave  <- 2010L
    out$issue <- iss
    out$treat <- as.integer(df[[treat_col]])
    out$resp_id <- seq_len(nrow(df))
    out
  })) %>% filter(!is.na(ch_leftist))   # drop rows where respondent skipped this issue
}

# 2011: 4 issues, no per-issue treat (treat = 0 for all)
build_2011 <- function(df) {
  issues <- c("health", "unemp", "ab", "guns")
  bind_rows(lapply(issues, function(iss) {
    rank_col <- paste0(iss, "_rank")
    out <- decode_lqr(as.integer(df[[rank_col]]))
    out$wave  <- 2011L
    out$issue <- iss
    out$treat <- 0L
    out$resp_id <- seq_len(nrow(df))
    out
  })) %>% filter(!is.na(ch_leftist))
}

# 2012: 2 issues, no per-issue treat for the ranking
build_2012 <- function(df) {
  issues <- c("foreign", "debt")
  bind_rows(lapply(issues, function(iss) {
    rank_col <- paste0(iss, "_rank")
    out <- decode_lqr(as.integer(df[[rank_col]]))
    out$wave  <- 2012L
    out$issue <- iss
    out$treat <- 0L
    out$resp_id <- seq_len(nrow(df))
    out
  })) %>% filter(!is.na(ch_leftist))
}

dt_long <- bind_rows(build_2010(d10), build_2011(d11), build_2012(d12))

# 5. Build standardized frame.
# `id` is constructed as wave*10000 + resp_id so it is unique across waves
# (no respondent overlap across CCES years).
dt <- dt_long %>%
  arrange(wave, issue, resp_id) %>%
  mutate(unit  = row_number(),
         id    = wave * 10000L + resp_id,
         treat = as.integer(treat)) %>%
  select(unit, id, treat, all_of(items), wave, issue)

names(dt) <- tolower(names(dt))

cat("Total rows:", nrow(dt), "\n")
cat("Rows by wave:\n"); print(table(dt$wave))
cat("Rows by issue:\n"); print(table(dt$wave, dt$issue))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "egan-2014.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("egan-2014.csv")
