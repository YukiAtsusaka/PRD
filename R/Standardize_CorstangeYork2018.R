library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_sav(here("raw", "syria-refugees-sept-10-2015_framing.sav"))

# 2. Items
items <- c("ch_freesyrianarmy", "ch_syriangovernment", "ch_syrianislamistgroups",
           "ch_foreignislamistgroups", "ch_kurdishgroups", "ch_hizbalah")

# 3. Choice columns
rank_columns <- c("Q28a", "Q28b", "Q28c", "Q28d", "Q28e", "Q28f")

stopifnot(all(rank_columns %in% names(df)))

df <- df %>%
  rename_with(~ items, all_of(rank_columns))

# 4. Treatment
q30_int <- as.integer(df$Q30)
df$treat <- dplyr::case_when(
  q30_int == 1L                ~ 0L,
  q30_int %in% c(2L, 3L)       ~ 1L,
  q30_int == 4L                ~ 2L,
  q30_int == 5L                ~ 3L,
  q30_int == 6L                ~ 4L,
  q30_int %in% c(7L, 10L)      ~ 5L,
  q30_int %in% c(8L, 11L)      ~ 6L,
  q30_int %in% c(9L, 12L)      ~ 7L,
  TRUE                         ~ NA_integer_
)

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = QUEST_ID,
         age = as.integer(Q7),
         female = as.integer(Q8)) %>%
  select(unit, id, treat, starts_with("ch_"), age, female)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "corstange-york-2018.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("corstange-york-2018.csv")
