library(dplyr)
library(here)
library(readr)
library(haven)

# Atsusaka & Kim 2025, Political Analysis:
# "Addressing Measurement Errors in Ranking Questions for the Social Sciences."
# Political Analysis 33(4): 339-60.
#
# Original online survey (June 2023) of US adults. Methodology paper — the
# experimental design here is *item-order randomization* and an *anchor
# question* (with known correct answers) used to detect random responders.
# There is no substantive treatment, so `treat = 0` for all respondents.
#
# The central ranking question asks respondents to rank four identities from
# 1 (most important) to 4 (least important). Each rank 1-4 appears exactly once
# per respondent (full ranking):
#   partisan  = political party
#   religious = religion
#   gender    = gender
#   racial    = race / ethnicity
#
# Two data sources, both 1,082 respondents in identical row order (verified:
# identity_ranking$app_party == AmericanRanking$app_identity_1, etc.):
#   raw/identity_ranking.Rda           — cleaned analysis file with the four
#     ranking items (app_party/religion/gender/race), the anchor-question ranks
#     (anc_federal/state/municipal/school), the anchor-correct flag
#     (anc_correct_identity), and the survey weight (s_weight). ALL kept.
#   raw/AmericanRanking_June_2023.sav  — full Qualtrics export; source for the
#     respondent covariates (age, race, partisanship, education, income, etc.).

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

# 2. Items (standardized ch_* names -> the four identities; ch_ prefix is
#    stripped by finalize_csv() so downstream columns are partisan/religious/
#    gender/racial). Keep everything else in identity_ranking.Rda as-is.
ir <- ir %>%
  rename(ch_partisan  = app_party,
         ch_religious = app_religion,
         ch_gender    = app_gender,
         ch_racial    = app_race) %>%
  mutate(across(c(ch_partisan, ch_religious, ch_gender, ch_racial), as.integer))

# 3. Respondent covariates from the .sav (labelled -> readable factors; keep
#    age/birthyr as integers). item_order retains the item-order randomization.
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

# 4. Treatment — methodology paper, no substantive randomized treatment.
#    treat = 0 for all 1,082 respondents (same convention as other purely
#    observational ranking studies, e.g. jacoby_2014).
# 5. Assemble: unit, id, treat, the four ranking items, then every remaining
#    identity_ranking.Rda variable (anchors + weight), then the covariates.
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

# Finalize schema: drop ch_ prefix, drop id, add ranking summary column, and
# reorder to unit, <items>, ranking, treat, <covariates>.
source(here::here("R", "finalize_schema.R"))
finalize_csv("atsusaka-kim-2025.csv")
