library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)  
library(readxl)

# 1. Load
df <- readxl::read_excel(here::here("raw", "NairandSambanis2019.xlsx"))

# 2. Items
items <- c("ch_Indian", "ch_Kashmiri", "ch_Religion", "ch_Occupation")

# 3. Treatment (0-indexed from string `group`)
df <- df %>%
  mutate(
    D_clean = str_squish(group),   # normalize spacing/case
    D_num = case_when(
      D_clean == "control" ~ 0L,
      D_clean == "protest" ~ 1L,
      D_clean == "economic growth" ~ 2L,
      D_clean == "army"  ~ 3L,
      D_clean == "map"   ~ 4L,
      TRUE ~ NA_integer_   # anything unrecognized
    )
  )

# 4. Choice columns
df$Kashmiri_rank <- NA
df$Religion_rank <- NA
df$Occupation_rank <- NA

choice_set <- grep("_rank$", names(df), value = TRUE)
names(df)[match(choice_set, names(df))] <- items

head(df)

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = D_num
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything())  %>%
  select(-c(group, starts_with("group_"), group1_control, group2_violence, group3_growth, group4_int_inst, group5_geography, D_clean, D_num))

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "nair-sambanis-2019.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("nair-sambanis-2019.csv")
