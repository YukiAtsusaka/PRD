library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)  
library(readxl)

# 1. Load
df <- readxl::read_excel(here::here("raw", "McCauleyandPosner2019.xlsx"))

# 2. Items (raw labels first; standardized ch_* names mapped below)
items <- c("National", "Religion", "Ethnic", "Occupation", "Gender", "Other")

# 3. Choice columns: build top-1 indicator (1 where item == respondent's PrimID, NA otherwise)
df_out <- df %>%
  rowwise() %>%
  mutate(
    .flags = list(ifelse(items == PrimID, 1L, NA_integer_))
  ) %>%
  ungroup() %>%
  unnest_wider(.flags, names_sep = "_") %>%
  rename_with(~ items, starts_with(".flags_"))

# Map raw labels to standardized ch_* names
rename_map <- c(
  "National"   = "ch_Nationality",
  "Religion"   = "ch_Religion",
  "Ethnic"   = "ch_Ethnicity",
  "Occupation" = "ch_Occupation",
  "Gender"   = "ch_Gender",
  "Other" = "ch_Other"
)

df_out <- df_out %>%
  rename_with(
    .fn = ~ ifelse(.x %in% names(rename_map), rename_map[.x], .x),
    .cols = everything()
  )
final_names <- unname(rename_map[match(items, names(rename_map))])

# Augment ch_Religion with second-choice info from Relig1st2nd
df_out <- df_out %>%
  mutate(
    ch_Religion = case_when(
      PrimID == "Religion" ~ 1L,                               # top choice
      Relig1st2nd == 1L & is.na(ch_Religion) ~ 2L,             # second choice
      TRUE ~ ch_Religion
    )
  )

df_out

# 4. Build standardized frame (treat = Cote_divoire_10 country indicator)
dt <- df_out %>%
  mutate(unit = row_number(), 
         id = Observation,
         treat = Cote_divoire_10,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(Observation, Cote_divoire_10, Relig1st2nd, Cluster, PrimID, ReligID, EthID, NatlID, OccID, GenderID, OtherID))

names(dt) <- tolower(names(dt))



glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "mccauley-posner-2019.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("mccauley-posner-2019.csv")
