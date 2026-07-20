library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)

# Costa 2020, AJPS: "Ideology, Not Affect: What Americans Want from Political
# Representation" -- Study 3 (partial ranking of 10 policy issues).
#
# Design: respondents ranked their top-3 issues from a list of 10; only
# ranking1 (top-1) and ranking3 (3rd place) were stored. Positions 2 and 4-10
# are unobserved.
#
# Treatment: 3x3 factorial (3 quote conditions x 3 issue conditions),
# encoded in 8 binary indicators + two *_quote_text columns. Derived labels
# (quotecondition, issuecondition) follow the construction in study3_analysis.R.

# 1. Load
df <- read_csv(here("raw", "costa_study3.csv"), show_col_types = FALSE)

# 2. Items
# 10 policy issues, with exact CSV labels -> ch_* names
# CSV label order (alphabetical): Crime, Drug addiction, Education, Environment,
# Health care, Immigration, Jobs, National security, Social security, Terrorism
label_to_ch <- c(
  "National security" = "ch_nationalsecurity",
  "Health care"       = "ch_healthcare",
  "Education"         = "ch_education",
  "Jobs"              = "ch_jobs",
  "Crime"             = "ch_crime",
  "Immigration"       = "ch_immigration",
  "Drug addiction"    = "ch_drugaddiction",
  "Social security"   = "ch_socialsecurity",
  "Terrorism"         = "ch_terrorism",
  "Environment"       = "ch_environment"
)
items <- unname(label_to_ch)

# 3. Choice columns: rank 1 if ranking1 matches, rank 3 if ranking3 matches,
# NA otherwise (positions 2, 4-10 unobserved).
for (lbl in names(label_to_ch)) {
  col <- label_to_ch[[lbl]]
  df[[col]] <- dplyr::case_when(
    df$ranking1 == lbl ~ 1L,
    df$ranking3 == lbl ~ 3L,
    TRUE ~ NA_integer_
  )
}

# 4. Treatment: derive quotecondition (3 levels) from the 8 binary indicators + *_quote_text.
# Matches the construction in study3_analysis.R line-by-line.
df <- df %>%
  mutate(
    quotecondition = dplyr::case_when(
      Repub_neither == 1 | Repub_issue == 1 | Dem_issue == 1 | Dem_neither == 1 ~ "No quote",
      (Repub_quote == 1 | Repub_both == 1) &
        Repub_quote_text == "We should do everything it takes to make sure Democrats lose the next election." ~ "They lose",
      (Repub_quote == 1 | Repub_both == 1) &
        Repub_quote_text == "We should do everything it takes to make sure Republicans win the next election." ~ "We win",
      (Dem_quote == 1 | Dem_both == 1) &
        Dem_quote_text == "We should do everything it takes to make sure Republicans lose the next election." ~ "They lose",
      (Dem_quote == 1 | Dem_both == 1) &
        Dem_quote_text == "We should do everything it takes to make sure Democrats win the next election." ~ "We win",
      TRUE ~ NA_character_
    ),
    # Derive issuecondition (3 levels): No issue / Top issue / 3rd issue
    issuecondition = dplyr::case_when(
      Repub_neither == 1 | Repub_quote == 1 | Dem_quote == 1 | Dem_neither == 1 ~ "No issue",
      (Repub_issue == 1 | Repub_both == 1 | Dem_issue == 1 | Dem_both == 1) &
        issue == ranking1 ~ "Top issue",
      (Repub_issue == 1 | Repub_both == 1 | Dem_issue == 1 | Dem_both == 1) &
        issue == ranking3 ~ "3rd issue",
      TRUE ~ NA_character_
    )
  )

# Combine into single 0-indexed treat: 9 cells (3 quote x 3 issue)
df <- df %>%
  mutate(
    quote_f = factor(quotecondition, levels = c("No quote", "They lose", "We win")),
    issue_f = factor(issuecondition, levels = c("No issue", "Top issue", "3rd issue")),
    treat = as.integer(interaction(quote_f, issue_f, drop = FALSE)) - 1L
  )

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number()) %>%
  select(unit, id, treat, starts_with("ch_"),
         issue, quotecondition, issuecondition,
         pid, ideo, gender, age, race)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "costa-2020.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("costa-2020.csv")
