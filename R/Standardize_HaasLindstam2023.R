library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)
library(readxl)

# 1. Load
df <- readxl::read_excel(here::here("raw", "HaasandLindstam2023.xlsx"))

# 2. Keep Hindu respondents only; drop Muslim respondents (Religion == 2).
df <- df %>% filter(Religion == 1L)

# 3. Demand columns are character with literal "NA"; coerce to integer.
to_num <- function(x) { x[x == "NA"] <- NA; as.integer(x) }
df <- df %>%
  mutate(hh = to_num(DemandHH),      # Hindu-partner demand
         hm = to_num(DemandHM)) %>%  # Muslim-partner demand
  # Keep only respondents who completed the ranking task (both partners scored).
  filter(!is.na(hh) & !is.na(hm))

# 4. Reconstruct the 3-item ranking (1 = most ideal)
df <- df %>%
  mutate(
    `ch_self`   = hh + hm - 2L,   # = 4 - (6 - hh - hm): respondent's own rank
    `ch_hindu`  = 4L - hh,        # another Hindu member
    `ch_muslim` = 4L - hm         # a Muslim member
  )
items <- c("ch_self", "ch_hindu", "ch_muslim")

# 5. Treatment
#    Gender: recode to Male = 0, Female = 1.
df <- df %>%
  mutate(Treat  = Treat - 1L,
         Gender = Gender - 2L)

# 6. Build standardized frame  
dt <- df %>%
  mutate(unit = row_number(), id = ID, treat = Treat) %>%
  select(unit, id, treat, all_of(items), everything()) %>%
  select(-c(ID, Treat, Religion, DemandHH, DemandHM, DemandMH, DemandMM, hh, hm))

names(dt) <- tolower(names(dt))

glimpse(dt)

# 7. Export
write_csv(dt, here("standardized", "haas-lindstam-2023.csv"))

# 8. Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("haas-lindstam-2023.csv")
