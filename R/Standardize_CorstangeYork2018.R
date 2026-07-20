library(dplyr)
library(here)
library(readr)
library(haven)

# Corstange & York 2018, AJPS: "Sectarian Framing in the Syrian Civil War"
# Syrian refugees in Lebanon ranked top-3 from 6 factions by sympathy
# Q28a-Q28f: rank each faction received (1=most, 2=second, 3=third, NA=not in top 3)
# Items: FSA, Syrian government, Syrian Islamist groups, foreign Islamist groups,
#        Kurdish groups, Hizballah
# Treatment: framing experiment (8 conditions)

# 1. Load
df <- read_sav(here("raw", "syria-refugees-sept-10-2015_framing.sav"))

# 2. Items
# 6 factions — Q28a-Q28f are in item-rank format (value = rank assigned, 1-3)
items <- c("ch_freesyrianarmy", "ch_syriangovernment", "ch_syrianislamistgroups",
           "ch_foreignislamistgroups", "ch_kurdishgroups", "ch_hizbalah")

# 3. Choice columns
rank_columns <- c("Q28a", "Q28b", "Q28c", "Q28d", "Q28e", "Q28f")

stopifnot(all(rank_columns %in% names(df)))

df <- df %>%
  rename_with(~ items, all_of(rank_columns))

# 4. Treatment: Q30 is the 12-level random framing assignment, collapsed to
# 8 analytical conditions per the paper (framing-ajps-cleandat.R:439-449):
#   1 ctrl                -> 0 control
#   2 smany, 3 sfew       -> 1 sect
#   4 dem                 -> 2 dem
#   5 rel                 -> 3 secular
#   6 for                 -> 4 foreign
#   7 svd, 10 dvs         -> 5 sect-vs-dem
#   8 svr, 11 rvs         -> 6 sect-vs-secular
#   9 svf, 12 fvs         -> 7 sect-vs-foreign
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
