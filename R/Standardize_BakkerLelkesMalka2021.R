library(dplyr)
library(here)
library(readr)
library(haven)

# 1. Load
df <- read_dta(here("raw", "GSS_panel06w123_R6a - Stata.dta"))

# 2. Items
items <- c("ch_obey", "ch_thnkself", "ch_workhard", "ch_helpoth", "ch_popular")
raw_items <- c("obey", "thnkself", "workhard", "helpoth", "popular")

# 3. Build long-format frame: one row per respondent x wave
make_wave <- function(df, w) {
  cols <- paste0(raw_items, "_", w)
  m <- as.data.frame(lapply(df[, cols], function(x) as.integer(x)))
  names(m) <- items
  tibble(
    id_panel = seq_len(nrow(df)),
    wave     = w,
    !!!m
  )
}

dt_long <- bind_rows(make_wave(df, 1L), make_wave(df, 2L), make_wave(df, 3L))

# 4. Keep only complete 1-5 rankings
keep <- rowSums(!is.na(dt_long[, items])) == 5L
dt_long <- dt_long[keep, ]

# 5. Build standardized frame  
dt <- dt_long %>%
  arrange(id_panel, wave) %>%
  mutate(unit  = row_number(),
         id    = id_panel,
         treat = as.integer(wave - 1L)) %>%       # waves 1/2/3 -> treat 0/1/2
  select(unit, id, treat, all_of(items), wave)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 6. Export
write_csv(dt, here("standardized", "bakker-etal-2021.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("bakker-etal-2021.csv")
