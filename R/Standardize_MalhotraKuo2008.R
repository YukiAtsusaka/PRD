library(dplyr)
library(here)
library(readr)
library(haven)

# Malhotra, Neil, and Alexander G. Kuo. 2008. "Attributing Blame: The Public's
# Response to Hurricane Katrina." Journal of Politics 70(1): 120-135.
#
# Survey experiment (TESS / Knowledge Networks, May 2006, N = 397 US adults).
# Respondents ranked SEVEN officials from 1 (most responsible/blameworthy) to
# 7 (least) for the loss of life and property in New Orleans after Katrina.
# (Note: the lit review listed "4 items" — that was wrong; the instrument
# ranks 7 officials.)
#
# Seven officials (item-rank columns blanco..vitter, value = rank 1-7):
#   blanco   = Louisiana Governor Kathleen Blanco        -> ch_blanco
#   brown    = FEMA Director Michael Brown               -> ch_brown
#   bush     = President George W. Bush                  -> ch_bush
#   chertoff = DHS Secretary Michael Chertoff            -> ch_chertoff
#   landrieu = Louisiana Senator Mary Landrieu           -> ch_landrieu
#   nagin    = New Orleans Mayor Ray Nagin               -> ch_nagin
#   vitter   = Louisiana Senator David Vitter            -> ch_vitter
# REFUSED (-1) / Not Asked (-2) are recoded to NA.
#
# Treatment = `cond` (cue manipulation), 0-indexed:
#   cond 1 -> 0  names only (no office, no party) [control]
#   cond 2 -> 1  + public office (job titles)
#   cond 3 -> 2  + political party
#   cond 4 -> 3  + both office and party
# `form` (A/B question version) is preserved as a covariate.

# 1. Load
df <- read_sav(here("raw", "Katrina_Blame_Data.sav"))

# 2. Items (item-rank columns)
rank_columns <- c("blanco", "brown", "bush", "chertoff", "landrieu", "nagin", "vitter")
items        <- c("ch_blanco", "ch_brown", "ch_bush", "ch_chertoff",
                  "ch_landrieu", "ch_nagin", "ch_vitter")
stopifnot(all(rank_columns %in% names(df)))

# 3. Treatment (0-indexed cue condition)
df <- df %>% mutate(treat = as.integer(cond) - 1L)

# 4. Choice columns: rename to ch_*, recode negatives (REFUSED/Not Asked) to NA
df <- df %>%
  rename_with(~ items, all_of(rank_columns)) %>%
  mutate(across(all_of(items), ~ {
    x <- as.integer(.)
    ifelse(x < 0L, NA_integer_, x)
  }))

# 5. Build standardized frame (demographics preserved as covariates)
dt <- df %>%
  mutate(unit   = row_number(),
         id     = as.integer(caseid),
         form   = as.integer(form),
         female = as.integer(ppgender == 2),
         age    = as.integer(ppage),
         educ   = as.integer(ppeduc),
         ideology = as.integer(ifelse(ideology %in% c(-1, -2, 9), NA, ideology)),
         partyid3 = as.integer(ifelse(partyid3 %in% c(-1, -2, 9), NA, partyid3))) %>%
  select(unit, id, treat, starts_with("ch_"),
         form, female, age, educ, ideology, partyid3)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "malhotra-kuo-2008.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("malhotra-kuo-2008.csv")
