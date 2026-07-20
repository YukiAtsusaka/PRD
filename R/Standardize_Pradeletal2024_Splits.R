library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)

# Pradel, Zilinsky, Kosmidis & Theocharis 2024 (APSR):
# "Toxic Speech and Limited Demand for Content Moderation on Social Media"
#
# PAIRWISE SPLITS of the LGBTQ-replication study (5 files).
# Sibling of the original 6-arm file pradel-etal-2024.csv (kept).
#
# Pradel's design has two control tiers:
#   - "non-group-related control"  : non-LGBTQ content    (treat = 1 in original)
#   - "control"                    : civil LGBTQ content  (treat = 0 in original)
# Plus 4 toxic-speech treatments (treat = 2, 3, 4, 5).
#
# Per the authors' analysis (see 1_main.R in the replication archive), the
# meaningful pairwise comparisons are:
#   - 4 toxic-speech effects: control vs each of uncivil / intolerant /
#     threatening / threatening (new). Each splits 0=control / 1=treatment.
#   - 1 group-membership effect: non-group-related control vs control.
#     Splits 0=non-group-related-control / 1=control.

# 1. Load raw
load(here("raw", "lgbtq_replication.RData"))
df <- iit2

# 2. Items + rank columns
items <- c("ch_Civil", "ch_Uncivil", "ch_Intolerant", "ch_Threatening")
rank_columns <- grep("^rank_", names(df), value = TRUE)
stopifnot(length(rank_columns) == length(items))
df <- df %>% rename_with(~ items, all_of(rank_columns))

# 3. Recode treatment string to numeric (matches original script)
df <- df %>%
  mutate(
    D_clean = str_squish(str_to_lower(treatment)),
    D_num = case_when(
      D_clean == "control"                       ~ 0L,
      D_clean == "non-group-related control"     ~ 1L,
      D_clean == "uncivil"                       ~ 2L,
      D_clean == "intolerant"                    ~ 3L,
      D_clean == "threatening"                   ~ 4L,
      D_clean == "threatening (new)"             ~ 5L,
      TRUE                                       ~ NA_integer_
    )
  )

# 4. Helper: filter to a pair, relabel treat to 0/1, build standardized frame
build_split <- function(df, base_arm, treat_arm, out_name) {
  d <- df %>%
    filter(D_num %in% c(base_arm, treat_arm)) %>%
    mutate(treat = if_else(D_num == base_arm, 0L, 1L)) %>%
    mutate(unit = row_number(),
           id   = row_number()) %>%
    select(unit, id, treat, starts_with("ch_"), everything()) %>%
    select(-c(handle, remove, D_clean, D_num))
  names(d) <- tolower(names(d))
  write_csv(d, here("standardized", out_name))
  cat(sprintf("  %s: %d rows (base arm %d -> treat=0, treat arm %d -> treat=1)\n",
              out_name, nrow(d), base_arm, treat_arm))
  invisible(d)
}

# 5. Build the 5 splits
cat("Building 5 pairwise splits of Pradel LGBTQ-replication:\n")
build_split(df, base_arm = 0L, treat_arm = 2L, out_name = "pradel-etal-2024-uncivil.csv")
build_split(df, base_arm = 0L, treat_arm = 3L, out_name = "pradel-etal-2024-intolerant.csv")
build_split(df, base_arm = 0L, treat_arm = 4L, out_name = "pradel-etal-2024-threatening.csv")
build_split(df, base_arm = 0L, treat_arm = 5L, out_name = "pradel-etal-2024-threatening-new.csv")
build_split(df, base_arm = 1L, treat_arm = 0L, out_name = "pradel-etal-2024-groupeffect.csv")
