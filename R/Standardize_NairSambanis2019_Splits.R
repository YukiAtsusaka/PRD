library(dplyr)
library(stringr)
library(here)
library(readr)
library(readxl)

# 1. Load raw
df <- readxl::read_excel(here::here("raw", "NairandSambanis2019.xlsx"))

# 2. Items
items <- c("ch_Indian", "ch_Kashmiri", "ch_Religion", "ch_Occupation")

# 3. Treatment  
df <- df %>%
  mutate(
    D_clean = str_squish(group),
    treat_prime = case_when(
      D_clean == "control"         ~ 0L,
      D_clean == "protest"         ~ 1L,
      D_clean == "economic growth" ~ 2L,
      D_clean == "army"            ~ 3L,
      D_clean == "map"             ~ 4L,
      TRUE                         ~ NA_integer_
    )
  )

# 4. Choice columns (only Indian_rank is real; others are NA placeholders)
df$Kashmiri_rank   <- NA
df$Religion_rank   <- NA
df$Occupation_rank <- NA

choice_set <- grep("_rank$", names(df), value = TRUE)
names(df)[match(choice_set, names(df))] <- items

# 5. Helper: filter to a pair, relabel treat to 0/1
build_split <- function(df, base_arm, treat_arm, out_name) {
  d <- df %>%
    filter(treat_prime %in% c(base_arm, treat_arm)) %>%
    mutate(treat = if_else(treat_prime == base_arm, 0L, 1L),
           unit  = row_number(),
           id    = row_number()) %>%
    select(unit, id, treat, starts_with("ch_"), everything()) %>%
    select(-c(group, starts_with("group_"),
              group1_control, group2_violence, group3_growth,
              group4_int_inst, group5_geography, D_clean, treat_prime))
  names(d) <- tolower(names(d))
  write_csv(d, here("standardized", out_name))
  cat(sprintf("  %s: %d rows (control -> treat=0, prime arm %d -> treat=1)\n",
              out_name, nrow(d), treat_arm))
  invisible(d)
}

# 6. Build the 4 pairwise splits
cat("Building 4 pairwise splits of Nair & Sambanis 2019:\n")
build_split(df, base_arm = 0L, treat_arm = 1L,
            out_name = "nair-sambanis-2019-protest.csv")
build_split(df, base_arm = 0L, treat_arm = 2L,
            out_name = "nair-sambanis-2019-growth.csv")
build_split(df, base_arm = 0L, treat_arm = 3L,
            out_name = "nair-sambanis-2019-army.csv")
build_split(df, base_arm = 0L, treat_arm = 4L,
            out_name = "nair-sambanis-2019-map.csv")

# 7. Finalize schema for all 4 splits
source(here::here("R", "finalize_schema.R"))
finalize_csv("nair-sambanis-2019-protest.csv")
finalize_csv("nair-sambanis-2019-growth.csv")
finalize_csv("nair-sambanis-2019-army.csv")
finalize_csv("nair-sambanis-2019-map.csv")
