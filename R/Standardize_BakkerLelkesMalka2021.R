library(dplyr)
library(here)
library(readr)
library(haven)

# Bakker, Lelkes & Malka 2021, APSR:
# "Reconsidering the Link Between Self-Reported Personality Traits and
#  Political Preferences"
#
# This is the sole standardized Bakker file. It covers the GSS panel
# portion: the 5-item Stenner-style child-rearing-values ranking, asked
# of the 2006 GSS panel respondents
# in each of three waves (2006, 2008, 2010 follow-ups within the same
# panel — Bakker et al. label these as _1, _2, _3 in `GSS_panel06w123_R6a`).
#
# Each respondent ranks the 5 child-rearing qualities from
# 1 = most important to 5 = least important. The raw value labels are
# "most important / 2nd important / 3rd important / 4th important /
# least important" coded 1..5. Empirically verified: every respondent
# with all 5 items has values {1,2,3,4,5} exactly once (row-sum 15).
#
# Five items (ranked, 1..5):
#   obey      -> ch_obey      ("to obey")
#   thnkself  -> ch_thnkself  ("to think for one's self")
#   workhard  -> ch_workhard  ("to work hard")
#   helpoth   -> ch_helpoth   ("to help others")
#   popular   -> ch_popular   ("to be well liked or popular")
#
# Treatment: this is observational panel data, not an experiment. The
# panel WAVE is the natural between-wave variation -- standardized to
# `treat = 0/1/2` for waves 1/2/3 (parallel to the Searing-Jacoby-Tyner
# 2019 wave-coded panel).
#
# Note on Bakker's use: Bakker, Lelkes & Malka 2021 themselves only
# extract `obey` and `thnkself` from this battery and combine them into
# a continuous authoritarianism index (their GSS_data_cleaning.R:22-24:
# `(6-obey + thnkself) / 2` rescaled to 0..1). They do not analyze the
# full 5-item ranking. The PRD standardizes the FULL ranking because
# the underlying GSS instrument IS a direct respondent-elicited 1..5
# ordering of the 5 qualities, and that is what qualifies under our
# inclusion rule.
#
# Scope: only the 2006-panel file is standardized in this script. The
# Bakker replication archive also includes GSS_panel08w123_R6.dta
# (2008-panel) and GSS_panel2010w123_R6.dta (2010-panel), each with the
# same 3-wave Stenner battery; those are easy follow-on extensions.

# 1. Load
df <- read_dta(here("raw", "GSS_panel06w123_R6a - Stata.dta"))

# 2. Items
items <- c("ch_obey", "ch_thnkself", "ch_workhard", "ch_helpoth", "ch_popular")
raw_items <- c("obey", "thnkself", "workhard", "helpoth", "popular")

# 3. Build long-format frame: one row per respondent x wave
make_wave <- function(df, w) {
  cols <- paste0(raw_items, "_", w)
  m <- as.data.frame(lapply(df[, cols], function(x) as.integer(x)))
  names(m) <- items
  tibble(
    id_panel = seq_len(nrow(df)),
    wave     = w,
    !!!m
  )
}

dt_long <- bind_rows(make_wave(df, 1L), make_wave(df, 2L), make_wave(df, 3L))

# 4. Keep only complete 1..5 rankings (drop respondent-waves who were not
# in the rotating Stenner sub-sample, marked IAP/NA in the raw data).
keep <- rowSums(!is.na(dt_long[, items])) == 5L
dt_long <- dt_long[keep, ]

# 5. Standardize
dt <- dt_long %>%
  arrange(id_panel, wave) %>%
  mutate(unit  = row_number(),
         id    = id_panel,
         treat = as.integer(wave - 1L)) %>%       # waves 1/2/3 -> treat 0/1/2
  select(unit, id, treat, all_of(items), wave)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "bakker-etal-2021.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("bakker-etal-2021.csv")
