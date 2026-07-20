library(dplyr)
library(tidyr)
library(here)
library(readr)
library(haven)

# Boudreau, Elmendorf & MacKenzie 2019, AJPS: "Racial or Spatial Voting?"
# 2011 San Francisco mayoral election exit poll with RCV (rank choice voting)
# Voters ranked up to 3 candidates from 11 serious candidates
# mayor1/mayor2/mayor3 = choice-order format (value = candidate ID)
# Need to invert to item-rank format (which rank each candidate got)
#
# 11 serious candidates (from paper p.9 Figure 1):
#   Avalos (Latino), Yee (Chinese Am), Adachi (Chinese Am), Chiu (Chinese Am),
#   Herrera (Latino), Rees (White), Dufty (White), Lee (Chinese Am),
#   Hall (White), Alioto-Pier (White), plus others
# Treatment: ethnic endorsement experiment (treat 1-5, from exit_1_voters.dta)

# 1. Load
df <- read_dta(here("raw", "exit_1_voters.dta"))

# 2. Items
# Candidate IDs from the candidates file (exit_1_cands-1.dta):
# 200001=Adachi, 200002=Alioto-Pier, 200004=Avalos, 200006=Chiu,
# 200008=Dufty, 200009=Hall, 200010=Herrera, 200012=Lee,
# 200014=Rees, 200015=Ting, 200016=Yee
# The 11 "serious" candidates per the paper

# Build candidate name mapping (ID to name)
cand_map <- c(
  "1" = "ch_adachi", "2" = "ch_aliotopier", "4" = "ch_avalos",
  "6" = "ch_chiu", "8" = "ch_dufty", "9" = "ch_hall",
  "10" = "ch_herrera", "12" = "ch_lee", "14" = "ch_rees",
  "15" = "ch_ting", "16" = "ch_yee"
)

# 3. Choice columns: invert choice-order to item-rank
# mayor1 = 1st choice candidate ID, mayor2 = 2nd, mayor3 = 3rd
# Recode 88/99 as NA (don't know / refused)

df <- df %>%
  mutate(across(c(mayor1, mayor2, mayor3),
                ~ ifelse(. %in% c(88, 99), NA_real_, .)))

# For each candidate, determine what rank (1, 2, 3) the voter assigned
for (cid in names(cand_map)) {
  cid_num <- as.numeric(cid)
  col_name <- cand_map[cid]
  df[[col_name]] <- case_when(
    df$mayor1 == cid_num ~ 1L,
    df$mayor2 == cid_num ~ 2L,
    df$mayor3 == cid_num ~ 3L,
    TRUE ~ NA_integer_
  )
}

# 4. Build standardized frame (treatment recoded to 0-indexed inline)
dt <- df %>%
  mutate(unit = row_number(),
         id = id,
         treat = as.integer(treat) - 1L) %>%
  select(unit, id, treat, starts_with("ch_"),
         race, gender, party, age, educ, income,
         ideo, interest, retro)

names(dt) <- tolower(names(dt))

glimpse(dt)

# 5. Export
write_csv(dt, here("standardized", "boudreau-etal-2019-mayoral.csv"))

# Finalize schema: drop ch_ prefix and add ranking summary column
source(here::here("R", "finalize_schema.R"))
finalize_csv("boudreau-etal-2019-mayoral.csv")
