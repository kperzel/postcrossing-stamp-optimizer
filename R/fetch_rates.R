# R/fetch_rates.R
# USPS postage rates and all purchasable stamp denominations
# Rates effective: July 13, 2025
# Source: USPS.com, walzpostal.com, stamps.com
# ⚠️ USPS typically changes rates in January and July — verify at:
#    https://www.usps.com/business/postage-rates.htm

library(tibble)

fetch_usps_rates <- function() {

  rates <- list(
    domestic_letter_1oz          = 0.78,
    domestic_letter_2oz          = 1.07,
    domestic_letter_3oz          = 1.36,
    domestic_postcard            = 0.61,
    domestic_additional_oz       = 0.29,
    domestic_flat_1oz            = 1.63,
    domestic_flat_additional_oz  = 0.29,
    nonmachinable_surcharge      = 0.40,
    international_letter_1oz     = 1.70
  )

  stamps <- tribble(
    ~id,                      ~category,       ~name,                              ~value,

    # Make-up / additional postage stamps
    "makeup_1c",              "Makeup",        "1¢ (Fringed Tulip)",               0.01,
    "makeup_2c",              "Makeup",        "2¢ (Daffodils)",                   0.02,
    "makeup_3c",              "Makeup",        "3¢ (Peonies)",                     0.03,
    "makeup_4c",              "Makeup",        "4¢ (Angel's Trumpets)",             0.04,
    "makeup_5c",              "Makeup",        "5¢ (Red Tulips)",                   0.05,
    "makeup_10c",             "Makeup",        "10¢ (Poppies)",                     0.10,
    "makeup_40c",             "Makeup",        "40¢ (Additional Postage)",          0.40,

    # Domestic stamps
    "domestic_forever",       "Domestic",      "Forever / 1oz Letter ($0.78)",      0.78,
    "domestic_postcard",      "Domestic",      "Postcard ($0.61)",                   0.61,
    "domestic_2oz",           "Domestic",      "Two-Ounce Letter ($1.07)",           1.07,
    "domestic_3oz",           "Domestic",      "Three-Ounce Letter ($1.36)",         1.36,
    "domestic_addl_oz",       "Domestic",      "Additional Ounce ($0.29)",           0.29,
    "domestic_nonmach",       "Domestic",      "Non-Machinable Surcharge ($0.40)",   0.40,

    # International stamps
    "intl_global_forever",    "International", "Global Forever ($1.70)",             1.70
  )

  list(rates = rates, stamps = stamps)
}
