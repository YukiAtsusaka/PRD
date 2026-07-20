library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_dta(here("raw", "replicationfile_Stata12.dta"))

# 2. Items
# 3 education goals: well-being, academic achievement, inclusiveness.
# The replication data only record whether well-being was ranked first
# (`wellbeingtoppriority`, a 0/1 binary). We recover this as a MARGINAL rank:
# well-being = 1 when it was the top priority, NA otherwise. The positions of
# academic achievement and inclusiveness are not recorded, so both are NA.
items <- c("ch_wellbeing", "ch_achievement", "ch_inclusiveness")

# 3. Build standardized frame (treatment, ch_*, covariates inline)
dt <- df %>%
  mutate(unit = row_number(),
         id = schoolid,
         treat = as.integer(discretiontreatmentgroups) - 1L,
         ch_wellbeing = if_else(as.integer(wellbeingtoppriority) == 1L,
                                1L, NA_integer_),
         ch_achievement = NA_integer_,
         ch_inclusiveness = NA_integer_) %>%
  select(unit, id, treat, starts_with("ch_"),
         informationtreatment, discretiontreatment, group1)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 4. Export
write_csv(dt, here("standardized", "andersen-moynihan-2016.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("andersen-moynihan-2016.csv")
