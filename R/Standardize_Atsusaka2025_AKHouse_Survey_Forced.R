library(dplyr)
library(stringr)
library(here)
library(readr)

# Atsusaka, Yuki. 2025. "Analyzing Ballot Order Effects When Voters Rank
# Candidates." Political Analysis 33(1): 64-72.
#
# Alaska U.S. House (U.S. Representative) Qualtrics survey -- *forced*
# ranking task (rank ALL four candidates). C4_* columns in
# qualtrics_Alaska_nonnumeric.csv. Same 354 Lucid respondents complete
# both this forced task and the optional sibling task (C3_*, in
# atsusaka-2025-akhouse-survey-optional.csv) plus the AK Senate ranking
# tasks (C5_*/C6_*). Order of forced/optional within each election is
# randomized per the paper.
#
# This Qualtrics survey is INDEPENDENT of the CVR sibling files
# atsusaka_2025_akusrep_full.csv / _partial.csv (real cast ballots from
# the 2022 Alaska U.S. Representative election); 4 candidates are the same.
#
# Treatment: ballot/display order. Randomization is per-respondent --
# `treat = 0` for all, with per-row permutation in `display_order`.
#
# Four candidates (column number -> standardized name; same labels as
# CVR sibling files):
#   C4_1 = Nick Begich      (Registered Republican)   -> ch_begich
#   C4_2 = Chris Bye        (Registered Libertarian)  -> ch_bye
#   C4_3 = Sarah Palin      (Registered Republican)   -> ch_palin
#   C4_4 = Mary S. Peltola  (Registered Democrat)     -> ch_peltola
#
# Values in C4_* are 1..4 (forced full ranking); all 354 respondents
# rank all 4 candidates.

# 1. Load
col_names <- names(read_csv(here("raw", "qualtrics_Alaska_nonnumeric.csv"),
                            n_max = 0, show_col_types = FALSE))
df <- read_csv(here("raw", "qualtrics_Alaska_nonnumeric.csv"),
               skip = 3, col_names = col_names, show_col_types = FALSE)

# 2. Items
items <- c("ch_begich", "ch_bye", "ch_palin", "ch_peltola")
rank_columns <- c("C4_1", "C4_2", "C4_3", "C4_4")

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
         display_order   = C4_DO,
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
write_csv(dt, here("standardized", "atsusaka-2025-house-full.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("atsusaka-2025-house-full.csv")
