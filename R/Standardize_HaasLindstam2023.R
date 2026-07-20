library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)
library(readxl)

# Haas & Lindstam 2023, APSR:
# "My History or Our History? Historical Revisionism and Entitlement to Lead."
#
# RANKING TASK (reconstructed 2026-07-19).
# In a real-money group leadership game, each respondent ranks the THREE members
# of their (Hindu-majority) group from most ideal (1) to least ideal (3) as group
# leader. For a HINDU respondent the three members are:
#   1. self   - the respondent themselves (a Hindu member)
#   2. hindu  - another Hindu member (the Hindu partner)
#   3. muslim - a Muslim member (the Muslim partner)
#
# The raw file stores only the two PARTNERS' positions, reverse-coded as a
# "demand" score (DemandHH = Hindu partner, DemandHM = Muslim partner):
#   demand 3 = ranked 1st (most ideal), 2 = 2nd, 1 = 3rd (least ideal).
# (Confirmed against the authors' analysis, `raw/haas-02 - ANALYSIS.R` lines
# 452-454, whose Figure 5 proportions we reproduce exactly.)
# The respondent's own position is the remaining rank, so with the two partner
# demands {HH, HM} the self demand = 6 - HH - HM (ranks are a permutation of
# 1:3). We convert every demand to the PRD rank convention rank = 4 - demand.
#
# The raw file also contains Muslim respondents (Religion == 2), whose task uses
# DemandMH / DemandMM. Per project decision (2026-07-19) Muslim respondents are
# REMOVED for now; only Hindu respondents (Religion == 1) are standardized.

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

# 4. Reconstruct the 3-item ranking (1 = most ideal). Item columns use the
#    ch_ prefix so finalize_schema() strips it and adds the `ranking` summary.
df <- df %>%
  mutate(
    `ch_self`   = hh + hm - 2L,   # = 4 - (6 - hh - hm): respondent's own rank
    `ch_hindu`  = 4L - hh,        # another Hindu member
    `ch_muslim` = 4L - hm         # a Muslim member
  )
items <- c("ch_self", "ch_hindu", "ch_muslim")

# 5. Treatment (0-indexed): 1 Neutral -> 0, 2 Inclusive -> 1, 3 Exclusive -> 2.
#    Gender: recode to Male = 0, Female = 1.
df <- df %>%
  mutate(Treat  = Treat - 1L,
         Gender = Gender - 2L)

# 6. Build standardized frame (drop raw demand columns and constant Religion).
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
