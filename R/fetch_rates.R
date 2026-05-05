# R/fetch_rates.R
# USPS postage rates and all purchasable stamp denominations
# Rates effective: July 13, 2025
# Source: USPS.com, walzpostal.com, stamps.com
# ⚠️ USPS typically changes rates in January and July — verify at:
#    https://www.usps.com/business/postage-rates.htm

library(tibble)
library(dplyr)

fetch_usps_rates <- function() {

  # Single source of truth for all rates and stamps
  # display_only = TRUE means shown in the rates panel but not a purchasable stamp
stamps <- tribble(
  ~id,                    ~category,       ~name,                              ~value,

  "makeup_1c",            "Makeup",        "1¢ (Fringed Tulip)",               0.01,
  "makeup_2c",            "Makeup",        "2¢ (Daffodils)",                   0.02,
  "makeup_3c",            "Makeup",        "3¢ (Peonies)",                     0.03,
  "makeup_4c",            "Makeup",        "4¢ (Angel's Trumpets)",             0.04,
  "makeup_5c",            "Makeup",        "5¢ (Red Tulips)",                   0.05,
  "makeup_10c",           "Makeup",        "10¢ (Poppies)",                     0.10,
  "makeup_40c",           "Makeup",        "40¢ (Additional Postage)",          0.40,
  "domestic_forever",     "Domestic",      "Forever / 1oz Letter ($0.78)",      0.78,
  "domestic_postcard",    "Domestic",      "Postcard ($0.61)",                   0.61,
  "domestic_2oz",         "Domestic",      "Two-Ounce Letter ($1.07)",           1.07,
  "domestic_3oz",         "Domestic",      "Three-Ounce Letter ($1.36)",         1.36,
  "domestic_addl_oz",     "Domestic",      "Additional Ounce ($0.29)",           0.29,
  "domestic_nonmach",     "Domestic",      "Non-Machinable Surcharge ($0.40)",   0.40,
  "intl_global_forever",  "International", "Global Forever ($1.70)",             1.70
)
  list(stamps = stamps)
}

