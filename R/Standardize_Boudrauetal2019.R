library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(writexl)
library(here)
library(readr)  
library(readxl)

# 1. Load
df <- readxl::read_excel(here::here("raw", "Boudreauetal2019.xlsx"))

# 2. Items
items <- c("ch_Clark", "ch_Officers", "ch_Hahn", "ch_Steinberg", "ch_Schubert", "ch_Brown", "ch_Senators")

# 3. Treatment (0-indexed: Control = 0, Treatment1 = 1, Treatment2 = 2)
df <- df %>%
  mutate(treat_info = treat_info - 1L)

# 4. Choice columns: rename all "blame*" columns to ch_*
choice_pattern <- "blame"
choice_columns <- grep(choice_pattern, names(df), value = TRUE)

df <- df %>%
  rename_with(~ items, all_of(choice_columns))

head(df)

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = idno,
         treat = treat_info,
         education = educ,
         work_for_law = work4law,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(treat_info, idno, educ, work4law))

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, "data/boudreau-etal-2019-police.csv")

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("boudreau-etal-2019-police.csv")
