library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)  

# 1. Load
load(here::here("raw", "lgbtq_replication.RData"))
ls()
df <- iit2
names(df)

# 2. Items
items <- c("ch_Civil", "ch_Uncivil", "ch_Intolerant", "ch_Threatening")
D <- "treatment"

# 3. Choice columns
rank_columns <- grep("^rank_", names(df), value = TRUE)

stopifnot(length(rank_columns) == length(items))   # enforce same size between ranks and choice sets

df <- df %>%
  rename_with(~ items, all_of(rank_columns))

# 4. Treatment (0-indexed from string `treatment`)
df <- df %>%
  mutate(
    D_clean = str_squish(str_to_lower(.data[[D]])),   # normalize spacing/case
    D_num = case_when(
      D_clean == "control" ~ 0L,
      D_clean == "non-group-related control" ~ 1L,
      D_clean == "uncivil" ~ 2L,
      D_clean == "intolerant"  ~ 3L,
      D_clean == "threatening"   ~ 4L,
      D_clean == "threatening (new)"  ~ 5L,
      TRUE ~ NA_integer_   # anything unrecognized
    )
  )

head(df)

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = D_num,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(handle, remove, D_clean, D_num))

names(dt) <- tolower(names(dt))



glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "pradel-etal-2024.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("pradel-etal-2024.csv")
