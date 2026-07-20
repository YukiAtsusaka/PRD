library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)  
library(haven)

# 1. Load
df <- read_dta(here("raw", "Deitrich2016.dta"))

# 2. Items
df <- df %>%
  mutate(id = row_number())
items <- c("ch_recipientgovernment", "ch_internationalorganization", "ch_internationalNGOs", "ch_localNGOs", "ch_privatesector")

# 3. Treatment: pivot the three treatment-question columns into long format
df <- df %>%
  pivot_longer(
    cols = c(q133, q126, q128),,
    names_to = "treatment",   # new column name for treatment
    values_to = "recipientgovernment_rank"
  )

# Numeric treatment codes (recipient-based)
df <- df %>%
  mutate(
    D_clean = str_squish(treatment),
    D_num = case_when(
      D_clean == "q133" ~ 1L,
      D_clean == "q126" ~ 2L,
      D_clean == "q128" ~ 3L,
      TRUE ~ NA_integer_   # anything unrecognized
    )
  )

# 4. Choice columns
df$internationalorganization_rank <- NA
df$internationalNGOs_rank <- NA
df$localNGOs_rank <- NA
df$privatesector_rank <- NA

choice_set <- grep("_rank$", names(df), value = TRUE)
names(df)[match(choice_set, names(df))] <- items

head(df)

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         treat = D_num,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(treatment, D_clean, q132, q135, q136, q139, q142, D_num))

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "deitrich-2016.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("deitrich-2016.csv")
