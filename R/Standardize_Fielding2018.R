library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(here)
library(readr)
library(readxl)

# 1. Load
df <- readxl::read_excel(here::here("raw", "Fielding2018.xlsx"))

# 2. Items
items <- c("ch_UKIP", "ch_BNP", "ch_Conservative", "ch_Labour", "ch_Liberal", "ch_Democrat", "ch_GreenRespectParties")

# 3. Recode rankings
# rank = 7 - value for 1-6; value 0 (7th/unranked) -> NA.
to_rank <- function(x) ifelse(is.na(x) | x == 0, NA_integer_, 7L - as.integer(x))

df <- df %>%
  transmute(
    aaid, seat, region, `England-not-London`, archatown,
    `if-kids`, `if-beneficiary`, `if-graduate`, `if-low-quals`, `if-widowed`,
    `if-separated`, `if-divorced`, `if-single`, `if-female`, `if-religious`, age,
    ch_UKIP = to_rank(UKIP_rank),
    ch_BNP  = to_rank(BNP_rank),
    ch_Conservative = NA_integer_,
    ch_Labour = NA_integer_,
    ch_Liberal = NA_integer_,
    ch_Democrat = NA_integer_,
    ch_GreenRespectParties = NA_integer_
  ) %>%
  # Drop rows with no observed rank (both UKIP & BNP unranked/missing) and the
  # 4 improper rows where the two observed items share the same nonzero rank.
  filter(!(is.na(ch_UKIP) & is.na(ch_BNP))) %>%
  filter(is.na(ch_UKIP) | is.na(ch_BNP) | ch_UKIP != ch_BNP)

# 4. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = aaid,
         treat = archatown,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(aaid, archatown))

names(dt) <- tolower(names(dt))

glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "fielding-2018.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("fielding-2018.csv")
