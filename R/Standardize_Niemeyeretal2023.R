

library(tidyverse)
library(readxl)
library(stringr)

# 1. Load
df <- read_excel(here::here("raw", "Niemeyer_2023_FNQCJ.xlsx"),
                 sheet = "Sheet1")

# 2. Items
ID <- "PNum_Dbase"
D <- "Deliberation"
Gender <- "Gender"
Education <- "Education"
Age <- "Age"
# items <- c("ch_Upgrade the track to a bitumen road", "ch_Maintain the road in its current condition as 4WD track", "ch_Close the road and rehabilitate it", "ch_Upgrade the road to a dirt road", "ch_Stabalize the specific trouble spots but leave as 4WD track")
items <- c("ch_upgrade", "ch_maintain",
           "ch_close", "ch_dirtroad",
           "ch_stabalize")

# 3. Treatment: Deliberation indicator used as-is (pre/post stage)
# (Optional StageID 1/2 -> 0/1 recoding kept commented out from the original.)
#df <- df %>%
  #mutate(StageID = recode(StageID,
                        #`1` = 0,   # pre-deliberation
                        #`2`  = 1))  # post-deliberation

# 4. Choice columns: invert from choice-order (Pref1..Pref5) to item-rank format
choice_pattern <- "^Pref\\d+$"
choice_columns <- grep(choice_pattern, names(df), value = TRUE)
choice_columns <- choice_columns[order(as.integer(str_extract(choice_columns, "\\d+")))]
choice_columns

stopifnot(length(choice_columns) == length(items))

df_out <- df %>%
  rowwise() %>% # each row is one ID and we go row by row
  mutate(
    .ranking = list(as.integer(c_across(all_of(choice_columns)))), 
    
    # Create a new vector which will give for each observation the rank each item got
    .inv = list({
      R <- .ranking
      I <- length(items)
      
       
      if (length(R) != I || any(is.na(R)) || !setequal(R, 1:I)) {
        rep(NA_integer_, I)
      } else {
        out <- integer(I)
        out[R] <- seq_len(I)   # This should give the format we want
        out
      }
    })
  ) %>%
  ungroup() %>%
  # keep ID, D, covariates, and expand the items as columns
  select(all_of(c(ID, D, Gender, Education, Age)), .inv) %>%
  unnest_wider(.inv, names_sep = "_") %>%
  # rename inverse columns to the choice set names
  rename_with(~ items, starts_with(".inv_")) %>%
  arrange(across(all_of(c(ID, D, Gender, Education, Age))))



# 5. Build standardized frame
dt_out <- df_out %>%
  mutate(unit = row_number(),
         id = PNum_Dbase,
         treat = Deliberation,
         ) %>%
  select(unit, id, treat, starts_with("ch_"), everything()) %>%
  select(-c(PNum_Dbase, Deliberation))

names(dt_out) <- tolower(names(dt_out))

glimpse(dt_out)

# 6. Export
write_csv(dt_out, here("standardized", "niemeyer-etal-2023.csv"))


# Let's write a README files



# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("niemeyer-etal-2023.csv")
