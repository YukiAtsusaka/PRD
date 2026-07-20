library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_sav(here("raw", "MSU, Common, CCES Dataset.sav"))

# 2. Items
# 7 American values ranked via sequential elimination (Plackett-Luce)
# pt_*ordx columns are already in item-rank format (value = rank 1-7)
items <- c("ch_freedom", "ch_equality", "ch_economicsecurity",
           "ch_morality", "ch_individualism", "ch_socialorder", "ch_patriotism")

# 3. Choice columns
rank_columns <- c("pt_freordx", "pt_eqordx", "pt_esordx",
                   "pt_morordx", "pt_indordx", "pt_socordx", "pt_patordx")

stopifnot(all(rank_columns %in% names(df)))

df <- df %>%
  rename_with(~ items, all_of(rank_columns))

# 4. Build standardized frame
# No experimental treatment (observational CCES data); treat = 0 for all rows.
# Paper restricts to 775 respondents with complete rank-orders (p.758)
dt <- df %>%
  mutate(across(all_of(items), as.integer)) %>%
  filter(if_all(all_of(items), ~ !is.na(.))) %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = 0L) %>%
  select(unit, id, treat, starts_with("ch_"),
         gender, race, educ, pid3, ideo5, inputstate)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "jacoby-2014.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("jacoby-2014.csv")
