library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)
library(readxl)

# Fielding, David. 2018. "Traditions of Tolerance: The Long-Run Persistence
# of Regional Variation in Attitudes towards English Immigrants."
# British Journal of Political Science 48(1): 167-188.
#
# PARTIAL RANKING study. In the BES 2010 imaginary AV ballot, respondents ranked
# 7 parties. Fielding derives reverse-coded ordinal variables: per the paper,
# UKIP-rank-10 "equals 6 if UKIP is ranked first, 5 if ranked second and so on
# down to 0 if UKIP is ranked seventh or unranked; BNP-rank-10 is constructed in
# an analogous way." Only the UKIP and BNP positions survive in this extract; the
# other 5 parties' ranks are not in the file.
#
# We convert to the PRD convention (rank 1 = top choice): rank = 7 - value for
# values 1..6, and value 0 -> NA (7th/unranked cannot be distinguished from
# missing, so it is not a recorded rank position). This yields a valid top-6
# partial ranking of 7 parties with only UKIP & BNP observed. Verified against
# the raw cross-tab: of 2,550 UKIP==BNP "ties", 2,546 are both-at-0 (both
# unranked, legitimate) and only 4 are nonzero ties (data anomalies, dropped).

# 1. Load
df <- readxl::read_excel(here::here("raw", "Fielding2018.xlsx"))

# 2. Items
items <- c("ch_UKIP", "ch_BNP", "ch_Conservative", "ch_Labour", "ch_Liberal", "ch_Democrat", "ch_GreenRespectParties")

# 3. Convert reverse-coded ordinal scores to PRD rank positions.
# rank = 7 - value for 1..6; value 0 (7th/unranked) -> NA.
# Rename EXPLICITLY (do not rely on grep column order: the raw file lists
# BNP_rank before UKIP_rank, which previously swapped the two columns).
to_rank <- function(x) ifelse(is.na(x) | x == 0, NA_integer_, 7L - as.integer(x))

df <- df %>%
  transmute(
    aaid, seat, region, `England-not-London`, archatown,
    `if-kids`, `if-beneficiary`, `if-graduate`, `if-low-quals`, `if-widowed`,
    `if-separated`, `if-divorced`, `if-single`, `if-female`, `if-religious`, age,
    ch_UKIP = to_rank(UKIP_rank),
    ch_BNP  = to_rank(BNP_rank),
    # The other 5 parties are not in the Fielding extract (positions unobserved).
    ch_Conservative = NA_integer_,
    ch_Labour = NA_integer_,
    ch_Liberal = NA_integer_,
    ch_Democrat = NA_integer_,
    ch_GreenRespectParties = NA_integer_
  ) %>%
  # Drop rows with no observed rank (both UKIP & BNP unranked/missing) and the
  # 4 improper rows where the two observed items share the same nonzero rank.
  filter(!(is.na(ch_UKIP) & is.na(ch_BNP))) %>%
  filter(is.na(ch_UKIP) | is.na(ch_BNP) | ch_UKIP != ch_BNP)

# 4. Build standardized frame (treatment from archatown, inline)
# archatown = "archa town" indicator (medieval Jewish chest registered in town)
dt <- df %>%
  mutate(unit = row_number(),
         id = aaid,
         treat = archatown,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(aaid, archatown))

names(dt) <- tolower(names(dt))

glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "fielding-2018.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("fielding-2018.csv")
