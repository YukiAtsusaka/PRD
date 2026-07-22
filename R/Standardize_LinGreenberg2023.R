library(dplyr)
library(stringr)
library(here)
library(readr)

# 1. Load
df <- read_csv(here("raw", "Expert Escalation.csv"))

# 2. Items
items <- c("ch_sofraid", "ch_sofraidonship", "ch_dronestrike",
           "ch_bomberattack", "ch_missilestrike", "ch_cyberattack",
           "ch_supporttoproxies", "ch_groundattack")

# 3. Choice columns (already in item-rank format: value = rank assigned)
rank_columns <- paste0("Escalation_Order_", 1:8)
stopifnot(all(rank_columns %in% names(df)))

df <- df %>%
  rename_with(~ items, all_of(rank_columns))

# 4. Treatment: Country is the grouping variable (which country scenario),
# not a traditional treatment. Recode to 0-indexed.
df <- df %>%
  mutate(treat = as.integer(Country) - 1L)

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number()) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-Country)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "lingreenberg-2023.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("lingreenberg-2023.csv")
