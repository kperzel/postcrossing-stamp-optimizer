# R/optimizer.R
library(dplyr)

#' Find optimal stamp combinations to meet a postage target
#'
#' @param owned_stamps Named numeric vector of stamp values in dollars
#'                     e.g. c("Forever" = 0.78, "Global Forever" = 1.70)
#' @param target_cents Integer target postage in CENTS (e.g. 170 for $1.70)
#' @param max_stamps   Max number of stamps per combination (default 5)
#' @param top_n        Number of results to return (default 10)
#'
#' @return A data frame of top combinations, ordered by optimality

find_combinations <- function(owned_stamps,
                              target_cents,
                              max_stamps = 5,
                              top_n      = 10) {
  
  # ── Work in cents to avoid floating point errors ───────────────────────────
  # e.g. 0.78 + 0.29 in binary floating point != 1.07 exactly
  stamp_cents <- round(owned_stamps * 100)
  stamp_names <- names(owned_stamps)
  n_types     <- length(stamp_cents)
  
  results <- list()
  
  # ── Generate all combinations (repetition allowed) up to max_stamps ───────
  for (n in 1:max_stamps) {
    
    # Get all combinations with replacement of size n
    idx_combos <- as.data.frame(
      t(combn(rep(seq_len(n_types), each = n), n))
    )
    
    for (i in seq_len(nrow(idx_combos))) {
      idx   <- as.integer(idx_combos[i, ])
      vals  <- stamp_cents[idx]
      total <- sum(vals)
      
      # Only keep combinations that meet or exceed the target
      if (total >= target_cents) {
        results[[length(results) + 1]] <- list(
          stamps     = paste(sort(stamp_names[idx]), collapse = " + "),
          total_cents = total,
          overage    = total - target_cents,
          num_stamps = n
        )
      }
    }
  }
  
  if (length(results) == 0) {
    return(data.frame(
      Message = "No valid combinations found. Try adding more stamp denominations."
    ))
  }
  
  # ── Rank and format results ────────────────────────────────────────────────
  # Primary sort: smallest overage (closest to target without going under)
  # Secondary sort: fewest stamps used
  df <- bind_rows(results) %>%
    distinct(stamps, .keep_all = TRUE) %>%
    arrange(overage, num_stamps) %>%
    slice_head(n = top_n) %>%
    mutate(
      Combination  = stamps,
      Total        = paste0("$", formatC(total_cents / 100, format = "f", digits = 2)),
      Overage      = paste0("+$", formatC(overage / 100,    format = "f", digits = 2)),
      `# Stamps`   = num_stamps
    ) %>%
    select(Combination, Total, Overage, `# Stamps`)
  
  df
}
