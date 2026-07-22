library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)
library(readxl)

# 1. Load
df <- read_excel(here("raw", "Niemeyer_2023_FNQCJ.xlsx"),
                 sheet = "Sheet1")

# 2. Items (5 road-management policy options)
ID <- "PNum_Dbase"
D <- "Deliberation"
Gender <- "Gender"
Education <- "Education"
Age <- "Age"
items <- c("ch_upgrade", "ch_maintain",
           "ch_close", "ch_dirtroad",
           "ch_stabalize")

# 3. Treatment (Deliberation indicator used as-is: pre/post-deliberation stage)

# 4. Choice columns (invert from choice-order Pref1..Pref5 to item-rank format)
choice_pattern <- "^Pref\\d+$"
choice_columns <- grep(choice_pattern, names(df), value = TRUE)
choice_columns <- choice_columns[order(as.integer(str_extract(choice_columns, "\\d+")))]

stopifnot(length(choice_columns) == length(items))

df_out <- df %>%
  rowwise() %>%
  mutate(
    .ranking = list(as.integer(c_across(all_of(choice_columns)))),
    # For each row, build a vector giving each item its assigned rank
    .inv = list({
      R <- .ranking
      I <- length(items)
      if (length(R) != I || any(is.na(R)) || !setequal(R, 1:I)) {
        rep(NA_integer_, I)
      } else {
        out <- integer(I)
        out[R] <- seq_len(I)
        out
      }
    })
  ) %>%
  ungroup() %>%
  select(all_of(c(ID, D, Gender, Education, Age)), .inv) %>%
  unnest_wider(.inv, names_sep = "_") %>%
  rename_with(~ items, starts_with(".inv_")) %>%
  arrange(across(all_of(c(ID, D, Gender, Education, Age))))

# 5. Build standardized frame
dt_out <- df_out %>%
  mutate(unit = row_number(),
         id = PNum_Dbase,
         treat = Deliberation) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(PNum_Dbase, Deliberation))

names(dt_out) <- tolower(names(dt_out))
glimpse(dt_out)

# 6. Export
write_csv(dt_out, here("standardized", "niemeyer-etal-2023.csv"))

# 7. Finalize schema (drop ch_ prefix, add ranking summary column)
source(here::here("R", "finalize_schema.R"))
finalize_csv("niemeyer-etal-2023.csv")
