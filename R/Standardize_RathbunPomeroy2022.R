library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(writexl)
library(here)
library(readr)  
library(readxl)

# 1. Load
df <- readxl::read_excel(here::here("raw", "RathbunandPomeroy2022.xlsx"))

# 2. Items
items <- c("ch_Honest", "ch_Intelligent", "ch_Fair", "ch_Friendly", "ch_Resolved", "ch_Organized", "ch_Powerful", "ch_Culture", "ch_Generous")

# 3. Treatment: actor group (no traditional control arm — two cross-cutting groups)
df <- df %>%
  mutate(
    D = actor,
    clean_D = str_to_lower(str_trim(actor))
    )
df <- df %>%
  mutate(
    actor = case_when(
      clean_D %in% c("individuals")  ~ 1L,
      clean_D %in% c("countries")  ~ 2L,
      )
    )

# 4. Choice columns: rename trait labels to ch_*
rename_map <- c(
  "Honest"   = "ch_Honest",
  "Intelligent"   = "ch_Intelligent",
  "Fair"   = "ch_Fair",
  "Friendly" = "ch_Friendly",
  "Resolved"   = "ch_Resolved",
  "Organized" = "ch_Organized",
  "Powerful" = "ch_Powerful",
  "Culture" = "ch_Culture",
  "Generous" = "ch_Generous"
)

df <- df %>%
  rename_with(
    .fn = ~ ifelse(.x %in% names(rename_map), rename_map[.x], .x),
    .cols = everything()
  )
final_names <- unname(rename_map[match(items, names(rename_map))])

head(df)

# 5. Build standardized frame
dt <- df %>%
  mutate(unit = row_number(),
         id = row_number(),
         treat = actor,
         condition = D,
  ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(D, clean_D, actor))

names(dt) <- tolower(names(dt))



glimpse(dt)

# 6. Export
write_csv(dt, "data/rathbun-pomeroy-2022.csv")

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("rathbun-pomeroy-2022.csv")
