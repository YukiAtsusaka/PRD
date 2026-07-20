library(dplyr)
library(stringr)
library(here)
library(readr)

# Atsusaka, Yuki. 2025. "Analyzing Ballot Order Effects When Voters Rank
# Candidates." Political Analysis 33(1): 64–72.
#
# Oakland mayoral Qualtrics survey -- *optional* (top-three) ranking task.
# This is the C1_* set of columns and matches real Oakland RCV election
# rules (voters rank up to three candidates). The forced full-1..10
# version of the same task lives in the sibling file
# atsusaka-2025-oakland-forced.csv (C2_* columns); both tasks are asked
# of the same 259 respondents in a randomized order. Yuki's *published*
# Oakland figure (Figure 2 / Figure E1) uses the forced sibling.
#
# Each respondent saw an independently randomized display order of 10
# candidates and was asked to rank their top three choices (RCV format).
#
# Ten candidates (same numbering as in the raw Qualtrics file C1_*):
#   C1_1  = Seneca Scott               -> ch_scott
#   C1_2  = Gregory Hodge              -> ch_hodge
#   C1_3  = Loren Manuel Taylor        -> ch_taylor
#   C1_4  = Peter Y. Liu               -> ch_liu
#   C1_5  = Sheng Thao                 -> ch_thao
#   C1_6  = Ignacio De La Fuente       -> ch_delafuente
#   C1_7  = Allyssa Victory Villanueva -> ch_villanueva
#   C1_8  = John Reimann               -> ch_reimann
#   C1_17 = Tyron C. Jordan            -> ch_jordan
#   C1_18 = Treva D. Reid              -> ch_reid
#
# Values in C1_* are 1 / 2 / 3 (top-three ranks), NA otherwise.
#
# Treatment: display-order randomization happens within-respondent — each
# respondent sees their own random permutation of the 10 candidates — so
# there is no between-respondent treatment to put in `treat`. Set
# `treat = 0` for all respondents and preserve the full per-respondent
# display order in the `display_order` covariate (pipe-separated candidate
# names) for downstream analysis of position effects.
#
# The second ranking task (C2_*) is the "forced" full 1..10 version of
# the same question and is standardized in the sibling script
# R/Standardize_Atsusaka2025_Oakland_Forced.R ->
# standardized/atsusaka-2025-oakland-forced.csv.
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

rank_columns <- c("C1_1", "C1_2", "C1_3", "C1_4", "C1_5",
                  "C1_6", "C1_7", "C1_8", "C1_17", "C1_18")

stopifnot(all(rank_columns %in% names(df)))

# 3. Choice columns: already in item-rank format (value = rank assigned,
# 1 / 2 / 3). Coerce to integer (the column type is character because the
# header rows contained JSON strings).
df <- df %>%
  rename_with(~ items, all_of(rank_columns)) %>%
  mutate(across(all_of(items), ~ suppressWarnings(as.integer(.))))

# 4. Treatment
# Per-respondent randomization (within-subject) — no between-subject
# treatment. `treat = 0` for all respondents; the full display order is
# preserved in the `display_order` covariate.
df <- df %>%
  mutate(treat = 0L)

# 5. Build standardized frame. Drop respondents who didn't engage with the
# C1 ranking question (all-NA across ch_*) — keeps the file focused on
# respondents who actually provided a top-3.
keep <- rowSums(!is.na(df[, items])) > 0
df <- df[keep, ]

dt <- df %>%
  mutate(unit            = row_number(),
         id              = row_number(),
         display_order   = C1_DO,
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
write_csv(dt, here("standardized", "atsusaka-2025-oakland-partial.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("atsusaka-2025-oakland-partial.csv")
