library(dplyr)
library(readr)
library(stringr)
library(here)

# finalize_csv(): post-processing step that applies the new schema
# (drop ch_ prefix, add ranking column) to a freshly-written CSV.
# Called at the end of each Standardize_*.R script.
#
# Usage at end of any Standardize_X.R:
#   source(here::here("R", "finalize_schema.R"))
#   finalize_csv("X.csv")
#
# This function is idempotent — running it again on an already-finalized
# file is a no-op (or just re-computes the ranking, which should be
# identical).

# rank_specs (mirrors R/validate_all.R)
.rank_specs <- tribble(
  ~file,                              ~rank_type,   ~k,
  "andersen-moynihan-2016.csv",        "marginal",    3L,
  "atsusaka-2025-senate-full.csv",             "full",    4L,
  "atsusaka-2025-senate-partial.csv",     "partial", 4L,
  "atsusaka-2025-house-full.csv",              "full",    4L,
  "atsusaka-2025-house-partial.csv",      "partial", 4L,
  "atsusaka-2025-oakland-full.csv",            "full",   10L,
  "atsusaka-2025-oakland-partial.csv",    "partial",10L,
  "atsusaka-kim-2025.csv",             "full",        4L,
  "bakker-etal-2021.csv",       "full",        5L,
  "boudreau-etal-2019-police.csv",            "full",        7L,
  "boudreau-etal-2019-police-pattern.csv",       "full",     7L,  # pairwise split: control vs pattern
  "boudreau-etal-2019-police-reform.csv",        "full",     7L,  # pairwise split: control vs reform
  "boudreau-etal-2019-mayoral.csv", "partial", 11L,
  "corstange-2022-sy2015.csv",        "partial",     6L,
  "corstange-2022-sy2017.csv",        "partial",     6L,
  "corstange-york-2018.csv",           "partial",     6L,
  "costa-2020.csv",                   "partial",    10L,
  "deitrich-2016.csv",                "partial",     5L,
  "doshi-etal-2019.csv",              "partial",   189L,
  "egan-2014.csv",                    "full",        3L,
  "fang-li-2020.csv",                  "marginal",    7L,
  "fielding-2018.csv",                "partial",     7L,
  "flavin-hartney-2017.csv",           "full",        5L,
  "haas-lindstam-2023.csv",         "full",        3L,
  "haas-lindstam-2023-inclusive.csv", "full",      3L,
  "haas-lindstam-2023-exclusive.csv", "full",      3L,
  "jacoby-2014.csv",                  "full",        7L,
  "krupnov-levine-2019.csv",           "partial",     6L,  # only ineq + health ranked; other 4 items NA throughout
  "krupnov-levine-2019-humaninterest.csv", "partial", 6L,  # pairwise split: control vs human-interest
  "krupnov-levine-2019-statspct.csv",      "partial", 6L,  # pairwise split: control vs stats %
  "krupnov-levine-2019-statsn.csv",        "partial", 6L,  # pairwise split: control vs stats N
  "krupnov-levine-2019-statsndenom.csv",   "partial", 6L,  # pairwise split: control vs stats N+denom
  "lingreenberg-2023.csv",            "full",        8L,
  "malhotra-kuo-2008.csv",             "full",        7L,
  "malhotra-kuo-2008-office.csv",         "full",     7L,  # pairwise split: control vs +office cue
  "malhotra-kuo-2008-party.csv",          "full",     7L,  # pairwise split: control vs +party cue
  "malhotra-kuo-2008-both.csv",           "full",     7L,  # pairwise split: control vs +office +party
  "mccauley-posner-2019.csv",         "partial",     6L,
  "mcmurry-2022.csv",                 "full",        4L,
  "nair-sambanis-2019.csv",         "partial",     4L,
  "nair-sambanis-2019-protest.csv",   "partial",   4L,  # pairwise split: control vs protest prime
  "nair-sambanis-2019-growth.csv",    "partial",   4L,  # pairwise split: control vs economic growth prime
  "nair-sambanis-2019-army.csv",      "partial",   4L,  # pairwise split: control vs army prime
  "nair-sambanis-2019-map.csv",       "partial",   4L,  # pairwise split: control vs map prime
  "niemeyer-etal-2023.csv",            "full",        5L,
  "pradel-etal-2024.csv",              "full",        4L,
  "pradel-etal-2024-uncivil.csv",         "full",     4L,
  "pradel-etal-2024-intolerant.csv",      "full",     4L,
  "pradel-etal-2024-threatening.csv",     "full",     4L,
  "pradel-etal-2024-groupeffect.csv",     "full",     4L,
  "rathbun-pomeroy-2022.csv",          "full",        9L,
  "searing-etal-2019.csv",      "full",        9L
)

.build_ranking <- function(df, item_cols, rank_type) {
  vals <- as.matrix(df[, item_cols, drop = FALSE])
  vals_num <- suppressWarnings(matrix(as.numeric(vals), nrow = nrow(vals)))
  vals_num[vals_num %in% c(-97, -98, -99)] <- NA
  apply(vals_num, 1, function(row) {
    chars <- sapply(row, function(v) {
      if (is.na(v))                               return("-")
      if (rank_type == "indicator" && v == 0)     return("-")
      vint <- as.integer(v)
      if (vint >= 0 && vint <= 9)                 return(as.character(vint))
      return("?")
    })
    paste0(chars, collapse = "")
  })
}

finalize_csv <- function(filename) {
  spec <- .rank_specs[.rank_specs$file == filename, ]
  if (nrow(spec) != 1) {
    warning(sprintf("finalize_csv: no rank_specs entry for '%s' — skipping",
                    filename))
    return(invisible(NULL))
  }
  path <- here("standardized", filename)
  df <- read_csv(path, show_col_types = FALSE, name_repair = "minimal")

  # Identify item columns. Three possible incoming layouts:
  #   (a) legacy: unit, id, treat, ch_<items>, ...
  #   (b) old new: unit, id, treat, <items>, ranking, <covariates>
  #   (c) current: unit, <items>, ranking, treat, <covariates>   (re-run case)
  ch_cols <- grep("^ch_", names(df), value = TRUE)
  if (length(ch_cols) > 0) {
    item_cols <- ch_cols
  } else {
    pos_treat <- which(names(df) == "treat")
    pos_rank  <- which(names(df) == "ranking")
    if (length(pos_rank) == 1 && length(pos_treat) == 1 && pos_rank > pos_treat + 1) {
      # Layout (b): items between treat and ranking
      item_cols <- names(df)[(pos_treat + 1):(pos_rank - 1)]
    } else if (length(pos_rank) == 1) {
      # Layout (c): items between unit (col 1) and ranking
      item_cols <- names(df)[2:(pos_rank - 1)]
    } else if (length(pos_treat) == 1) {
      # No ranking col yet — fall back to k items after treat
      item_cols <- names(df)[(pos_treat + 1):(pos_treat + spec$k)]
    } else {
      # No treat, no ranking — k items after unit
      item_cols <- names(df)[2:(2 + spec$k - 1)]
    }
  }

  # Build ranking string (NA for files whose k > 9 exceeds the single-character-
  # per-position format: Oakland full k=10; Doshi et al 2019 k=189)
  ranking_vals <- if (filename %in% c("atsusaka-2025-oakland-full.csv",
                                       "doshi-etal-2019.csv")) {
    rep(NA_character_, nrow(df))
  } else {
    .build_ranking(df, item_cols, spec$rank_type)
  }

  # Drop ch_ prefix from item col names
  new_item_cols <- sub("^ch_", "", item_cols)
  names(df)[match(item_cols, names(df))] <- new_item_cols

  # New canonical layout: unit, <items>, ranking, treat, <covariates>
  # 1. Drop `id` if present.
  df$id <- NULL
  # 2. Save and drop `treat` (re-inserted after ranking).
  treat_vals <- if ("treat" %in% names(df)) df$treat else NA
  df$treat <- NULL
  # 3. Drop any pre-existing ranking col (idempotent).
  df$ranking <- NULL

  # 4. Reorder: unit + items first, then covariates after.
  last_item_pos <- which(names(df) == new_item_cols[length(new_item_cols)])
  before <- names(df)[seq_len(last_item_pos)]
  others <- setdiff(names(df), before)

  # 5. Insert ranking and treat between items and covariates.
  df$ranking <- ranking_vals
  df$treat   <- treat_vals
  df <- df[, c(before, "ranking", "treat", others)]

  write_csv(df, path)
  invisible(df)
}
