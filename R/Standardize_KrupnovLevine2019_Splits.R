library(dplyr)
library(here)
library(readr)
library(haven)

# Krupnikov & Levine 2019 (JoP) — "Political Issues, Evidence, and Citizen
# Engagement: The Case of Unequal Access to Affordable Health Care"
#
# PAIRWISE SPLITS of the 5-arm evidence-format experiment (4 files).
# Sibling of the original 5-arm file `krupnov-levine-2019.csv` (kept).
#
# Treatment manipulated which form of evidence respondents saw about health
# care inequality, then asked them to rank 6 policy issues (health, inequality,
# federal debt, unemployment, immigration, ethics) by importance. Verified
# from SSI.do (`prtest health_rank1 if (healthtreat==1 | healthtreat==2),
# by(healthtreat)` etc.) that the ranking IS the post-treatment outcome.
#
# Original healthtreat -> treat (after `- 1L`):
#   treat = 0  Control (no evidence about health care)
#   treat = 1  Human-interest story (sympathetic individual case)
#   treat = 2  Statistics as percentages
#   treat = 3  Statistics as raw counts (N)
#   treat = 4  Statistics as N with denominator
#
# CAVEAT: only health_rank and ineq_rank are in the public data; the other
# 4 items (federal debt, unemployment, immigration, ethics) are NA columns.

# 1. Load raw
df <- read_dta(here("raw", "KrupnovLevine2019.dta"))

# 2. Items + treatment recoding
items <- c("ch_federaldebt", "ch_unemployment", "ch_ineq", "ch_health",
           "ch_immigration", "ch_ethics")

df <- df %>% mutate(treat_evidence = healthtreat - 1L)

# 3. Choice columns — ONLY ineq_rank and health_rank exist in the public data;
#    the other 4 items are NA. Rename by NAME (not position) so the marginal
#    ranks land in the correct item columns. (A prior positional match() had
#    misrouted health_rank -> unemployment and ineq_rank -> federaldebt.)
df <- df %>%
  rename(ch_ineq = ineq_rank, ch_health = health_rank) %>%
  mutate(ch_federaldebt  = NA,
         ch_unemployment = NA,
         ch_immigration  = NA,
         ch_ethics       = NA)

# 4. Helper: filter to a pair, relabel treat to 0/1
build_split <- function(df, base_arm, treat_arm, out_name) {
  d <- df %>%
    filter(treat_evidence %in% c(base_arm, treat_arm)) %>%
    mutate(treat = if_else(treat_evidence == base_arm, 0L, 1L),
           unit  = row_number(),
           id    = row_number(),
           age   = how_old) %>%
    select(unit, id, treat, all_of(items), everything()) %>%
    select(-c(healthtreat, treat_evidence, humaninterest, statspercent,
              statsN, statsNdenom, control, how_old))
  names(d) <- tolower(names(d))
  write_csv(d, here("standardized", out_name))
  cat(sprintf("  %s: %d rows (control -> treat=0, evidence arm %d -> treat=1)\n",
              out_name, nrow(d), treat_arm))
  invisible(d)
}

# 5. Build the 4 pairwise splits
cat("Building 4 pairwise splits of Krupnikov & Levine 2019:\n")
build_split(df, base_arm = 0L, treat_arm = 1L,
            out_name = "krupnov-levine-2019-humaninterest.csv")
build_split(df, base_arm = 0L, treat_arm = 2L,
            out_name = "krupnov-levine-2019-statspct.csv")
build_split(df, base_arm = 0L, treat_arm = 3L,
            out_name = "krupnov-levine-2019-statsn.csv")
build_split(df, base_arm = 0L, treat_arm = 4L,
            out_name = "krupnov-levine-2019-statsndenom.csv")

# 6. Finalize schema for all 4 splits
source(here::here("R", "finalize_schema.R"))
finalize_csv("krupnov-levine-2019-humaninterest.csv")
finalize_csv("krupnov-levine-2019-statspct.csv")
finalize_csv("krupnov-levine-2019-statsn.csv")
finalize_csv("krupnov-levine-2019-statsndenom.csv")
