library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load both sources
e <- new.env()
load(here("raw", "identity_ranking.Rda"), envir = e)
ir <- e$identity_ranking

sav <- read_sav(here("raw", "AmericanRanking_June_2023.sav"))
stopifnot(nrow(ir) == nrow(sav))

# Confirm the two files are row-aligned before column-binding covariates.
stopifnot(
  all(ir$app_party    == as.integer(sav$app_identity_1)),
  all(ir$app_religion == as.integer(sav$app_identity_2)),
  all(ir$app_gender   == as.integer(sav$app_identity_3)),
  all(ir$app_race     == as.integer(sav$app_identity_4))
)

# 2. Items
ir <- ir %>%
  rename(ch_partisan  = app_party,
         ch_religious = app_religion,
         ch_gender    = app_gender,
         ch_racial    = app_race) %>%
  mutate(across(c(ch_partisan, ch_religious, ch_gender, ch_racial), as.integer))

# 3. Build standardized frame  
covars <- sav %>%
  transmute(
    age         = as.integer(age),
    birthyr     = as.integer(birthyr),
    # ranking item is named `gender`; call the respondent's gender gender_resp
    # to avoid a column-name collision after finalize strips the ch_ prefix.
    gender_resp = as.character(as_factor(gender3)),
    female      = as.integer(gender3 == 2),
    race       = as.character(as_factor(race)),
    race4      = as.character(as_factor(race4)),
    hispanic   = as.character(as_factor(hispanic)),
    educ       = as.character(as_factor(educ)),
    educ4      = as.character(as_factor(educ4)),
    pid3       = as.character(as_factor(pid3)),
    pid7       = as.character(as_factor(pid7)),
    ideo7      = as.character(as_factor(ideo7)),
    religpew   = as.character(as_factor(religpew)),
    faminc_new = as.character(as_factor(faminc_new)),
    region     = as.character(as_factor(region)),
    inputstate = as.character(as_factor(inputstate)),
    item_order = as.character(as_factor(app_identity_row_rnd))
  )

# 5. Assemble - unit, id, treat, the four ranking items, covariates.
dt <- bind_cols(ir, covars) %>%
  mutate(unit = row_number(),
         id   = row_number(),
         treat = 0L) %>%
  select(unit, id, treat,
         ch_partisan, ch_religious, ch_gender, ch_racial,
         anc_federal, anc_state, anc_municipal, anc_school,
         anc_correct_identity, s_weight,
         everything())

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "atsusaka-kim-2025.csv"))

# Finalize schema: drop ch_ prefix, drop id, add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("atsusaka-kim-2025.csv")
