library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)

# 1. Load
df <- read.table(here("raw", "MP Values, Both Years, with names.txt"),
                 header = TRUE, sep = "", na.strings = c("NA", "NA "),
                 strip.white = TRUE)

# 2. Items
value_names <- c("auth", "commun", "econeq", "freed", "freeent",
                 "prop", "soceq", "sochier", "unity")

items <- c("ch_authority", "ch_community", "ch_econequality",
           "ch_freedom", "ch_freeenterprise", "ch_property",
           "ch_socialequality", "ch_socialhierarchy", "ch_unity")

# 3. Choice columns + treatment: reshape to long, with treat indicating wave
# (treat = 0 for 1973 pre-wave, treat = 1 for 2013 post-wave)
cols_73 <- paste0(value_names, ".73")
cols_13 <- paste0(value_names, ".13")

df <- df %>% mutate(mp_id = row_number())

df_73 <- df %>%
  select(mp_id, party, all_of(cols_73)) %>%
  rename_with(~ items, all_of(cols_73)) %>%
  mutate(treat = 0L)  # 0 = 1973 (pre)

df_13 <- df %>%
  select(mp_id, party, all_of(cols_13)) %>%
  rename_with(~ items, all_of(cols_13)) %>%
  mutate(treat = 1L)  # 1 = 2013 (post)

# 4. Build standardized frame
# Keep only rows that are a valid full ranking. Only complete, tie-free rankings remain.
combined <- bind_rows(df_73, df_13)
valid_full <- apply(as.matrix(combined[, items]), 1, function(x) {
  !anyNA(x) && isTRUE(all.equal(sort(as.integer(x)), 1:9))
})

dt <- combined[valid_full, ] %>%
  arrange(mp_id, treat) %>%
  mutate(unit = row_number(),
         id = mp_id) %>%
  select(unit, id, treat, starts_with("ch_"), party)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "searing-etal-2019.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("searing-etal-2019.csv")
