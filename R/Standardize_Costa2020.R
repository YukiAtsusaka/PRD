library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)

# 1. Load
df <- read_csv(here("raw", "costa_study3.csv"), show_col_types = FALSE)

# 2. Items
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

# 3. Choice columns
for (lbl in names(label_to_ch)) {
  col <- label_to_ch[[lbl]]
  df[[col]] <- dplyr::case_when(
    df$ranking1 == lbl ~ 1L,
    df$ranking3 == lbl ~ 3L,
    TRUE ~ NA_integer_
  )
}

# 4. Treatment 
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

# Combine into single 0-indexed treat
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
