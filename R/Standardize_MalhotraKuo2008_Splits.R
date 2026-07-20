library(dplyr)
library(here)
library(readr)
library(haven)

# Malhotra & Kuo 2008 (JoP) — "Attributing Blame: The Public's Response to
# Hurricane Katrina"
#
# PAIRWISE SPLITS of the 4-arm `cond` cue experiment (3 files).
# Sibling of the original 4-arm file `malhotra-kuo-2008.csv` (kept).
#
# Original cond -> treat (after `- 1L`):
#   treat = 0  Control (names only)
#   treat = 1  + Public office (e.g. "Louisiana Governor Kathleen Blanco")
#   treat = 2  + Political party (e.g. "Kathleen Blanco (Democrat)")
#   treat = 3  + Both office AND party
#
# Each split pairs the control (0) against ONE cue treatment, relabeled to 1.

# 1. Load raw
df <- read_sav(here("raw", "Katrina_Blame_Data.sav"))

# 2. Items
rank_columns <- c("blanco", "brown", "bush", "chertoff", "landrieu", "nagin", "vitter")
items        <- c("ch_blanco", "ch_brown", "ch_bush", "ch_chertoff",
                  "ch_landrieu", "ch_nagin", "ch_vitter")
stopifnot(all(rank_columns %in% names(df)))

# 3. Treatment (0-indexed cue condition)
df <- df %>% mutate(treat_cue = as.integer(cond) - 1L)

# 4. Rename + recode rank columns (negatives -> NA)
df <- df %>%
  rename_with(~ items, all_of(rank_columns)) %>%
  mutate(across(all_of(items), ~ {
    x <- as.integer(.)
    ifelse(x < 0L, NA_integer_, x)
  }))

# 5. Helper: filter to a pair, relabel treat to 0/1, build standardized frame
build_split <- function(df, base_arm, treat_arm, out_name) {
  d <- df %>%
    filter(treat_cue %in% c(base_arm, treat_arm)) %>%
    mutate(treat  = if_else(treat_cue == base_arm, 0L, 1L),
           unit   = row_number(),
           id     = as.integer(caseid),
           form   = as.integer(form),
           female = as.integer(ppgender == 2),
           age    = as.integer(ppage),
           educ   = as.integer(ppeduc),
           ideology = as.integer(ifelse(ideology %in% c(-1, -2, 9), NA, ideology)),
           partyid3 = as.integer(ifelse(partyid3 %in% c(-1, -2, 9), NA, partyid3))) %>%
    select(unit, id, treat, starts_with("ch_"),
           form, female, age, educ, ideology, partyid3)
  names(d) <- tolower(names(d))
  write_csv(d, here("standardized", out_name))
  cat(sprintf("  %s: %d rows (control -> treat=0, cue arm %d -> treat=1)\n",
              out_name, nrow(d), treat_arm))
  invisible(d)
}

# 6. Build the 3 pairwise splits
cat("Building 3 pairwise splits of Malhotra & Kuo 2008:\n")
build_split(df, base_arm = 0L, treat_arm = 1L,
            out_name = "malhotra-kuo-2008-office.csv")
build_split(df, base_arm = 0L, treat_arm = 2L,
            out_name = "malhotra-kuo-2008-party.csv")
build_split(df, base_arm = 0L, treat_arm = 3L,
            out_name = "malhotra-kuo-2008-both.csv")

# 7. Finalize schema for all 3 splits
source(here::here("R", "finalize_schema.R"))
finalize_csv("malhotra-kuo-2008-office.csv")
finalize_csv("malhotra-kuo-2008-party.csv")
finalize_csv("malhotra-kuo-2008-both.csv")
