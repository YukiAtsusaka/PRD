library(dplyr)
library(stringr)
library(here)
library(readr)

# Atsusaka, Yuki. 2025. "Analyzing Ballot Order Effects When Voters Rank
# Candidates." Political Analysis 33(1): 64-72.
#
# Oakland mayoral Qualtrics survey -- *forced* ranking task (rank ALL 10
# candidates). Companion to atsusaka-2025-oakland-optional.csv (the top-3
# task that matches real Oakland RCV election rules). Same 259 Lucid
# respondents complete both tasks; the order of the two tasks is randomized
# (per the paper). Yuki's *published* Figure 2 (main text) and Figure E1
# (online appendix) on Oakland use this forced task (C2_*).
#
# Treatment in the paper: ballot/display order (the position each candidate
# is shown in). For this survey experiment that randomization is per-
# respondent (one random permutation of the 10 candidates per row), so
# there is no small-cardinality between-subjects arm -- `treat = 0` for all
# respondents, with the full per-row permutation preserved in
# `display_order` (pipe-separated candidate names) for downstream analysis
# of position effects.
#
# Ten candidates (column number -> standardized name; same labels as in the
# C1/optional sibling file):
#   C2_1  = Seneca Scott               -> ch_scott
#   C2_2  = Gregory Hodge              -> ch_hodge
#   C2_3  = Loren Manuel Taylor        -> ch_taylor
#   C2_4  = Peter Y. Liu               -> ch_liu
#   C2_5  = Sheng Thao                 -> ch_thao
#   C2_6  = Ignacio De La Fuente       -> ch_delafuente
#   C2_7  = Allyssa Victory Villanueva -> ch_villanueva
#   C2_8  = John Reimann               -> ch_reimann
#   C2_17 = Tyron C. Jordan            -> ch_jordan
#   C2_18 = Treva D. Reid              -> ch_reid
#
# Values in C2_* are 1..10 (forced full ranking); all 259 respondents
# rank all 10 candidates.
#
# Note on file format: Qualtrics CSVs have three header rows (variable
# names, question text, and ImportId metadata) before the actual data.
# We read the variable names from the first row, then read the data rows
# with skip = 3.

# 1. Load
col_names <- names(read_csv(here("raw", "qualtrics_Oakland.csv"),
                            n_max = 0, show_col_types = FALSE))
df <- read_csv(here("raw", "qualtrics_Oakland.csv"),
               skip = 3, col_names = col_names, show_col_types = FALSE)

# 2. Items (in the order they appear in the Qualtrics columns)
items <- c("ch_scott", "ch_hodge", "ch_taylor", "ch_liu", "ch_thao",
           "ch_delafuente", "ch_villanueva", "ch_reimann",
           "ch_jordan", "ch_reid")

rank_columns <- c("C2_1", "C2_2", "C2_3", "C2_4", "C2_5",
                  "C2_6", "C2_7", "C2_8", "C2_17", "C2_18")

stopifnot(all(rank_columns %in% names(df)))

# 3. Choice columns: already in item-rank format (value = rank assigned,
# 1..10). Coerce to integer (the column type is character because the
# header rows contained JSON strings).
df <- df %>%
  rename_with(~ items, all_of(rank_columns)) %>%
  mutate(across(all_of(items), ~ suppressWarnings(as.integer(.))))

# 4. Treatment
# Per-respondent randomization (within-subject) -- no between-subject
# treatment. `treat = 0` for all respondents; the full display order is
# preserved in the `display_order` covariate.
df <- df %>%
  mutate(treat = 0L)

# 5. Build standardized frame. Drop respondents who didn't engage with the
# C2 ranking question (all-NA across ch_*).
keep <- rowSums(!is.na(df[, items])) > 0
df <- df[keep, ]

dt <- df %>%
  mutate(unit            = row_number(),
         id              = row_number(),
         display_order   = C2_DO,
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
write_csv(dt, here("standardized", "atsusaka-2025-oakland-full.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("atsusaka-2025-oakland-full.csv")
