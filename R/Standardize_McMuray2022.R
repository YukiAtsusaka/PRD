library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(writexl)
library(here)
library(readr)  
library(readxl)


# 1. Load
df <- readxl::read_excel(here::here("raw", "McMurry2022.xlsx"))

# 2. Items
ID <- "id"
treat <- "treat"
female <- "female"
items <- c("ch_Tribe", "ch_Nationality", "ch_Religion", "ch_Gender")

# 3. Choice columns
rank_columns <- grep("_rank$", names(df), value = TRUE)

df <- df %>%
  rename_with(~ items, all_of(rank_columns))

head(df)

# 4. Filter out 5 typo rows where the respondent gave duplicate rank values
# across the 4 items.
keep <- apply(df[, c("ch_Tribe", "ch_Nationality", "ch_Religion", "ch_Gender")],
              1, function(r) !anyNA(r) && length(unique(r)) == length(r))
df <- df[keep, ]

# 5. Build standardized frame (treat already 0-indexed in raw)
dt <- df %>%
  mutate(unit = row_number(),
          ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(ipra_factor, edu_hs))

names(dt) <- tolower(names(dt))



glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "mcmurry-2022.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("mcmurry-2022.csv")
