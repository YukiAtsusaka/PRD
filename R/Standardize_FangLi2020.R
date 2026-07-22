library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_dta(here("raw", "FangLi2020.dta"))

# 2. Build choice set and standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = as.integer(interaction(historical, powerful, valuable, island,
                                        drop = TRUE)) - 1L,
         ch_defense = as.integer(rank_defense),
         ch_economicdevelopment = NA_integer_,
         ch_socialstability = NA_integer_,
         ch_democracy = NA_integer_,
         ch_corruption = NA_integer_,
         ch_incomeinequality = NA_integer_,
         ch_environment = NA_integer_) %>%
  select(unit, id, treat, starts_with("ch_"), defense,
         historical, powerful, valuable, island,
         age, income, han, male, eastern, central, rural,
         college, SOE, ccp, socialstatus, news)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 3. Export
write_csv(dt, here("standardized", "fang-li-2020.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("fang-li-2020.csv")
