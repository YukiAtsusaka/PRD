library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)  
library(haven)

# 1. Load
df <- read_dta(here("raw", "KrupnovLevine2019.dta"))

# 2. Items
items <- c("ch_federaldebt", "ch_unemployment", "ch_ineq", "ch_health", "ch_immigration", "ch_ethics")

# 3. Treatment (0-indexed: Control = 0, Treatment1 = 1, Treatment2 = 2, ...)
df <- df %>%
  mutate(healthtreat = healthtreat - 1L)

# 4. Choice columns — ONLY ineq_rank and health_rank exist in the public data;
#    the other 4 items are NA. Rename by NAME (not position) so the marginal
#    ranks land in the correct item columns. (A prior positional match() had
#    misrouted health_rank -> unemployment and ineq_rank -> federaldebt.)
df <- df %>%
  rename(ch_ineq = ineq_rank, ch_health = health_rank) %>%
  mutate(ch_federaldebt  = NA,
         ch_unemployment = NA,
         ch_immigration  = NA,
         ch_ethics       = NA)

head(df)

# 5. Build standardized frame (items selected in canonical order)
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = healthtreat,
         age = how_old,
  ) %>%
  select(unit, id, treat, all_of(items), everything()) %>%
  select(-c(healthtreat, humaninterest, statspercent, statsN, statsNdenom, control, how_old))

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "krupnov-levine-2019.csv"))

 
 

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("krupnov-levine-2019.csv")
