library(dplyr)
library(stringr)
library(here)
library(readr)
 

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
