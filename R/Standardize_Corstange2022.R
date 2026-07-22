library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)  
library(haven)

# 1. Load (two waves)
df_2015 <- read_sav(here("raw", "Syria-2015.sav"))
df_2017 <- read_sav(here("raw", "Syria-2017.sav"))

# 2. Items (shared across both waves)
items <- c("ch_freesyrianarmy", "ch_syriangovernment", "ch_syrianislamistgroups", "ch_foreignislamistgroups", "ch_kurdishgroups", "ch_hizbalah")

# === Wave 1: Syria 2015 ===

# 3. Treatment (0-indexed)
df_2015 <- df_2015 %>%
  mutate(Q43 = Q43 - 1L)

# 4. Choice columns. Convert the raw missing-code sentinels (-97/-98/-99 =
# item not ranked / no response) to NA so unranked factions are NA, not codes.
rank_columns <- grep("^Q28[a-z]+$", names(df_2015), value = TRUE)

df_2015 <- df_2015 %>%
  rename_with(~ items, all_of(rank_columns)) %>%
  mutate(across(all_of(items),
                ~ { v <- as.integer(.); v[v %in% c(-97L, -98L, -99L)] <- NA_integer_; v }))

head(df_2015)

# 5. Build standardized frame
dt_15 <- df_2015 %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = Q43,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(Q43))

names(dt_15) <- tolower(names(dt_15))

glimpse(dt_15)

# 6. Export
write_csv(dt_15, here("standardized", "corstange-2022-sy2015.csv"))

# === Wave 2: Syria 2017 ===

# 3. Treatment (0-indexed)
df_2017 <- df_2017 %>%
  mutate(Q35 = Q35 - 1L)

# 4. Choice columns. Convert the raw missing-code sentinels (-97/-98/-99 =
# item not ranked / no response) to NA so unranked factions are NA, not codes.
rank_cols <- grep("^Q24[a-z]+$", names(df_2017), value = TRUE)

df_2017 <- df_2017 %>%
  rename_with(~ items, all_of(rank_cols)) %>%
  mutate(across(all_of(items),
                ~ { v <- as.integer(.); v[v %in% c(-97L, -98L, -99L)] <- NA_integer_; v }))

head(df_2017)

# 5. Build standardized frame
dt_17 <- df_2017 %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = Q35,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(Q35))

names(dt_17) <- tolower(names(dt_17))

glimpse(dt_17)

# 6. Export
write_csv(dt_17, here("standardized", "corstange-2022-sy2017.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("corstange-2022-sy2015.csv")
finalize_csv("corstange-2022-sy2017.csv")
