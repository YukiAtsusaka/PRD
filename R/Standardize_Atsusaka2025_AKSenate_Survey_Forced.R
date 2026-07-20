library(dplyr)
library(stringr)
library(here)
library(readr)

# Atsusaka, Yuki. 2025. "Analyzing Ballot Order Effects When Voters Rank
# Candidates." Political Analysis 33(1): 64-72.
#
# Alaska U.S. Senate Qualtrics survey -- *forced* ranking task (rank ALL
# four candidates). C6_* columns in qualtrics_Alaska_nonnumeric.csv.
# Same 354 Lucid respondents complete both this forced task and the
# optional sibling task (C5_*, in atsusaka-2025-aksenate-survey-optional.csv).
# The order of the two tasks is randomized (per the paper). The same
# 354 respondents also rank the four AK U.S. House candidates in C3_*/C4_*.
#
# This Qualtrics survey is INDEPENDENT of the cast-vote-record (CVR)
# files atsusaka_2025_aksenate_full.csv / _partial.csv, which come from
# actual cast ballots in the 2022 Alaska U.S. Senate election. The four
# candidates are the same, but the Qualtrics data is a between-subjects
# survey experiment on Lucid respondents while the CVR data is the
# corresponding real-election natural experiment via Alaska's ballot
# rotation procedure.
#
# Treatment: ballot/display order (the position each candidate is shown
# in). Randomization is per-respondent (one random permutation of the 4
# candidates per row), so there is no small-cardinality between-subjects
# arm -- `treat = 0` for all respondents, with the full per-row
# permutation preserved in `display_order` (pipe-separated candidate
# names) for downstream position-effect analysis.
#
# Four candidates (column number -> standardized name; same labels as the
# CVR sibling files atsusaka_2025_aksenate_full.csv / _partial.csv):
#   C6_1 = Patricia R. Chesbro (Registered Democrat)     -> ch_chesbro
#   C6_2 = Buzz A. Kelley      (Registered Republican)   -> ch_kelley
#   C6_3 = Lisa Murkowski      (Registered Republican)   -> ch_murkowski
#   C6_4 = Kelly C. Tshibaka   (Registered Republican)   -> ch_tshibaka
#
# Values in C6_* are 1..4 (forced full ranking); all 354 respondents
# rank all 4 candidates.
#
# Note on file format: Qualtrics CSVs have three header rows (variable
# names, question text, and ImportId metadata) before the actual data.

# 1. Load
col_names <- names(read_csv(here("raw", "qualtrics_Alaska_nonnumeric.csv"),
                            n_max = 0, show_col_types = FALSE))
df <- read_csv(here("raw", "qualtrics_Alaska_nonnumeric.csv"),
               skip = 3, col_names = col_names, show_col_types = FALSE)

# 2. Items (in the order they appear in the Qualtrics columns)
items <- c("ch_chesbro", "ch_kelley", "ch_murkowski", "ch_tshibaka")
rank_columns <- c("C6_1", "C6_2", "C6_3", "C6_4")

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
         display_order   = C6_DO,
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
write_csv(dt, here("standardized", "atsusaka-2025-senate-full.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("atsusaka-2025-senate-full.csv")
