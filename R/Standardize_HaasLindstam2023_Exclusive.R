library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)
library(readxl)

# Haas & Lindstam 2023, APSR:
# "My History or Our History? Historical Revisionism and Entitlement to Lead."
#
# Pairwise SPLIT: Neutral (control) vs Exclusive history (treatment).
# Sibling file: haas-lindstam-2023-inclusive.csv (Neutral vs Inclusive).
# The original 3-arm file (haas-lindstam-2023.csv) is kept too.
#
# Ranking reconstruction identical to Standardize_HaasLindstam2023.R: Hindu
# respondents only; three group members ranked 1 (most ideal) to 3 (least ideal)
# = self, another Hindu member, a Muslim member. See that script for details.
#
# 3 arms (after `Treat - 1L`): 0 Neutral, 1 Exclusive, 2 Inclusive. (Raw Treat
# coding verified against the authors' analysis data + recode_factor(Treat2 =
# "Exclusive", Treat3 = "Inclusive").)
# This file keeps treat 0 (Neutral) and Exclusive (already treat 1).

# 1. Load + keep Hindu respondents only
df <- readxl::read_excel(here::here("raw", "HaasandLindstam2023.xlsx")) %>%
  filter(Religion == 1L)

# 2. Reconstruct 3-item ranking (see main script)
to_num <- function(x) { x[x == "NA"] <- NA; as.integer(x) }
df <- df %>%
  mutate(hh = to_num(DemandHH), hm = to_num(DemandHM)) %>%
  filter(!is.na(hh) & !is.na(hm)) %>%
  mutate(`ch_self`   = hh + hm - 2L,
         `ch_hindu`  = 4L - hh,
         `ch_muslim` = 4L - hm,
         Treat  = Treat - 1L,
         Gender = Gender - 2L)
items <- c("ch_self", "ch_hindu", "ch_muslim")

# 3. Filter to Neutral (0) + Exclusive (already treat 1) only
df <- df %>% filter(Treat %in% c(0L, 1L))

# 4. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(), id = ID, treat = Treat) %>%
  select(unit, id, treat, all_of(items), everything()) %>%
  select(-c(ID, Treat, Religion, DemandHH, DemandHM, DemandMH, DemandMM, hh, hm))

names(dt) <- tolower(names(dt))
glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "haas-lindstam-2023-exclusive.csv"))

# 6. Finalize schema
source(here::here("R", "finalize_schema.R"))
finalize_csv("haas-lindstam-2023-exclusive.csv")
