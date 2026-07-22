library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_dta(here("raw", "Flavin_Hartney_AJPS_replication_board_member_survey.dta"))

# 2. Items
items <- c("ch_budgetfunding", "ch_teacherquality", "ch_learninggains",
           "ch_achievementgaps", "ch_commoncore")

# 3. Choice columns
rank_columns <- c("budget_urgent", "teaching_urgent", "learning_gains_urgent",
                   "achievementgaps_urgent", "commoncore_urgent")

stopifnot(all(rank_columns %in% names(df)))

df <- df %>%
  rename_with(~ items, all_of(rank_columns))

# 4. Build standardized frame  
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number()) %>%
  select(unit, id, treat, starts_with("ch_"), teacher,
         avghispgap, avgblkgap)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "flavin-hartney-2017.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("flavin-hartney-2017.csv")
