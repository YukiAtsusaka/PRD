library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)
library(haven)

# 1. Load the raw World Bank Ease of Doing Business panel  
df <- read_dta(here("raw", "Doshi2019_WorldBank.dta"))

# 2. Items (economies as columns; treat each year as a row).
# (2005-2014; 2000-2004 and 2015 are all NA).
# Fix 3 spelling typos in the paper's raw .dta file so column names match
# the canonical World Bank spelling.
df <- df %>%
  filter(year %in% 2005:2014, !is.na(economy)) %>%
  select(economy, year, p_edb_rank) %>%
  mutate(
    p_edb_rank = as.integer(p_edb_rank),
    economy = recode(economy,
                     "Afghananistan"                = "Afghanistan",
                     "Surinama"                     = "Suriname",
                     "St. Vincent and the Gredines" = "St. Vincent and the Grenadines")
  )

# 3. Treatment (0 for all rows)

# 4. Choice columns  
df <- df %>%
  mutate(economy_std = economy %>%
           str_squish() %>%
           str_to_lower() %>%
           str_replace_all("[^a-z0-9]+", "_") %>%
           str_replace_all("^_|_$", ""))

wide <- df %>%
  select(economy_std, year, p_edb_rank) %>%
  pivot_wider(names_from = economy_std, values_from = p_edb_rank)

economy_cols <- sort(setdiff(names(wide), "year"))
ch_cols      <- paste0("ch_", economy_cols)

# 5. Build standardized frame
dt <- wide %>%
  arrange(year) %>%
  rename_with(~ ch_cols, all_of(economy_cols)) %>%
  mutate(unit = row_number(), id = row_number(), treat = 0L) %>%
  select(unit, id, treat, all_of(ch_cols), year)

names(dt) <- tolower(names(dt))
glimpse(dt)

# 6. Export the standardized CSV
write_csv(dt, here("standardized", "doshi-etal-2019.csv"))

# 7. Finalize schema (drop ch_ prefix, insert ranking column between items and
#    treat -- ranking is NA for this file per finalize special-case, since
#    k >> 9 exceeds the single-character-per-position format)
source(here::here("R", "finalize_schema.R"))
finalize_csv("doshi-etal-2019.csv")
