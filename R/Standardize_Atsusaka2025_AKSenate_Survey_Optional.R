library(dplyr)
library(stringr)
library(here)
library(readr)

# Atsusaka, Yuki. 2025. "Analyzing Ballot Order Effects When Voters Rank
# Candidates." Political Analysis 33(1): 64-72.
#
# Alaska U.S. Senate Qualtrics survey -- *optional* ranking task
# (respondents may rank up to four candidates; values 1..K with K
# determined per-respondent). C5_* columns in
# qualtrics_Alaska_nonnumeric.csv. Sibling of the forced task in C6_*
# (atsusaka-2025-aksenate-survey-forced.csv). The order of the two
# tasks is randomized per the paper.
#
# This Qualtrics survey is INDEPENDENT of the CVR sibling files
# atsusaka_2025_aksenate_full.csv / _partial.csv (real cast ballots from
# the 2022 Alaska U.S. Senate election); the 4 candidates are the same.
#
# Treatment: ballot/display order. Randomization is per-respondent --
# `treat = 0` for all, with per-row permutation in `display_order`.
#
# Four candidates (column number -> standardized name; same labels as
# the CVR siblings):
#   C5_1 = Patricia R. Chesbro -> ch_chesbro
#   C5_2 = Buzz A. Kelley      -> ch_kelley
#   C5_3 = Lisa Murkowski      -> ch_murkowski
#   C5_4 = Kelly C. Tshibaka   -> ch_tshibaka
#
# Values in C5_* are 1..K (1 <= K <= 4); NA elsewhere.

# 1. Load
col_names <- names(read_csv(here("raw", "qualtrics_Alaska_nonnumeric.csv"),
                            n_max = 0, show_col_types = FALSE))
df <- read_csv(here("raw", "qualtrics_Alaska_nonnumeric.csv"),
               skip = 3, col_names = col_names, show_col_types = FALSE)

# 2. Items
items <- c("ch_chesbro", "ch_kelley", "ch_murkowski", "ch_tshibaka")
rank_columns <- c("C5_1", "C5_2", "C5_3", "C5_4")

stopifnot(all(rank_columns %in% names(df)))

# 3. Choice columns -> integer
df <- df %>%
  rename_with(~ items, all_of(rank_columns)) %>%
  mutate(across(all_of(items), ~ suppressWarnings(as.integer(.))))

# 4. Treatment
df <- df %>%
  mutate(treat = 0L)

# 5. Build standardized frame -- drop respondents who skipped this task
keep <- rowSums(!is.na(df[, items])) > 0
df <- df[keep, ]

dt <- df %>%
  mutate(unit            = row_number(),
         id              = row_number(),
         display_order   = C5_DO,
         # Respondent covariates (same B1-B6 demographic block in every
         # Atsusaka 2025 Qualtrics survey).
         age             = suppressWarnings(as.integer(B1)),  # B1: age in years
         gender          = B2,                                # B2: gender
         race            = B3,                                # B3: race/ethnicity
         partisanship    = B4,                                # B4: party ID
         education       = B5,                                # B5: highest education
         vote_likelihood = B6) %>%                            # B6: self-reported vote likelihood
  select(unit, id, treat, all_of(items),
         age, gender, race, partisanship, education, vote_likelihood, display_order)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "atsusaka-2025-senate-partial.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("atsusaka-2025-senate-partial.csv")
