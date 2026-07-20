library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_dta(here("raw", "FangLi2020.dta"))

# 2. Items
# Ranking of national defense among 7 policy issues
# Only one item's rank is available: rank_defense (1-7)
# The other 6 issues' ranks are not in the data
# Items from the paper: national defense, economic development, social stability,
#   democracy, corruption, income inequality, environmental protection
#
# Treatment: 2x2x2x2 factorial experiment (Fang & Li 2020, p.4):
#   historical (binary): territory historically owned by China vs. not
#   powerful (binary): militarily strong vs. weak neighbor
#   valuable (binary): territory has economic value vs. unknown
#   island (binary): island vs. land border
# Construct treat as 0-indexed interaction of all 4 factors.

# 3. Build standardized frame (treatment, ch_*, covariates inline)
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

# 4. Export
write_csv(dt, here("standardized", "fang-li-2020.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("fang-li-2020.csv")
