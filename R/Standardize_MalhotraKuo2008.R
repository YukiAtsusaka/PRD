library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_sav(here("raw", "Katrina_Blame_Data.sav"))

# 2. Items (item-rank columns)
rank_columns <- c("blanco", "brown", "bush", "chertoff", "landrieu", "nagin", "vitter")
items        <- c("ch_blanco", "ch_brown", "ch_bush", "ch_chertoff",
                  "ch_landrieu", "ch_nagin", "ch_vitter")
stopifnot(all(rank_columns %in% names(df)))

# 3. Treatment 
df <- df %>% mutate(treat = as.integer(cond) - 1L)

# 4. Choice columns 
df <- df %>%
  rename_with(~ items, all_of(rank_columns)) %>%
  mutate(across(all_of(items), ~ {
    x <- as.integer(.)
    ifelse(x < 0L, NA_integer_, x)
  }))

# 5. Build standardized frame 
dt <- df %>%
  mutate(unit   = row_number(),
         id     = as.integer(caseid),
         form   = as.integer(form),
         female = as.integer(ppgender == 2),
         age    = as.integer(ppage),
         educ   = as.integer(ppeduc),
         ideology = as.integer(ifelse(ideology %in% c(-1, -2, 9), NA, ideology)),
         partyid3 = as.integer(ifelse(partyid3 %in% c(-1, -2, 9), NA, partyid3))) %>%
  select(unit, id, treat, starts_with("ch_"),
         form, female, age, educ, ideology, partyid3)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "malhotra-kuo-2008.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("malhotra-kuo-2008.csv")
