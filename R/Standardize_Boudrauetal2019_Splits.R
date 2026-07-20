library(dplyr)
library(here)
library(readr)
library(readxl)

# Boudreau, MacKenzie & Simmons 2019 (JoP) — "Police Violence" study.
#
# PAIRWISE SPLITS of the 3-arm `treat_info` experiment (2 files).
# Sibling of the original 3-arm file `boudreau-etal-2019-police.csv` (kept).
#
# The original treat_info encoding (after `- 1L`):
#   treat = 0 : Control          — no extra info shown
#   treat = 1 : Pattern           — info framing the shooting as part of a
#                                   broader pattern of police violence
#   treat = 2 : Reform            — info framing the shooting as reformable
#                                   through policy change
#
# Each split pairs the control (0) against one treatment, relabeled to 1.

# 1. Load raw
df <- readxl::read_excel(here::here("raw", "Boudreauetal2019.xlsx"))

# 2. Items
items <- c("ch_Clark", "ch_Officers", "ch_Hahn", "ch_Steinberg",
           "ch_Schubert", "ch_Brown", "ch_Senators")

# 3. Recode treatment (matches original script)
df <- df %>% mutate(treat_info = treat_info - 1L)

# 4. Rename choice columns
choice_columns <- grep("blame", names(df), value = TRUE)
df <- df %>% rename_with(~ items, all_of(choice_columns))

# 5. Helper: filter to a pair, relabel treat to 0/1, build standardized frame
build_split <- function(df, base_arm, treat_arm, out_name) {
  d <- df %>%
    filter(treat_info %in% c(base_arm, treat_arm)) %>%
    mutate(treat = if_else(treat_info == base_arm, 0L, 1L)) %>%
    mutate(unit = row_number(),
           id   = idno,
           education    = educ,
           work_for_law = work4law) %>%
    select(unit, id, treat, starts_with("ch_"), everything()) %>%
    select(-c(treat_info, idno, educ, work4law))
  names(d) <- tolower(names(d))
  write_csv(d, here("standardized", out_name))
  cat(sprintf("  %s: %d rows (control -> treat=0, treat arm %d -> treat=1)\n",
              out_name, nrow(d), treat_arm))
  invisible(d)
}

# 6. Build the 2 splits
cat("Building 2 pairwise splits of Boudreau et al 2019 (police):\n")
build_split(df, base_arm = 0L, treat_arm = 1L,
            out_name = "boudreau-etal-2019-police-pattern.csv")
build_split(df, base_arm = 0L, treat_arm = 2L,
            out_name = "boudreau-etal-2019-police-reform.csv")

# 7. Finalize schema for both splits
source(here::here("R", "finalize_schema.R"))
finalize_csv("boudreau-etal-2019-police-pattern.csv")
finalize_csv("boudreau-etal-2019-police-reform.csv")
