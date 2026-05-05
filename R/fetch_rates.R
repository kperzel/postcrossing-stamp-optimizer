# R/fetch_rates.R
# USPS postage rates and all purchasable stamp denominations
# Rates effective: July 13, 2025
# Source: USPS.com, walzpostal.com, stamps.com
# ⚠️ USPS typically changes rates in January and July — verify at:
#    https://www.usps.com/business/postage-rates.htm

library(tibble)

fetch_usps_rates <- function() {
  
  # ── Current postage rates ──────────────────────────────────────────────────
  rates <- list(
    # Domestic
    domestic_letter_1oz          = 0.78,  # Forever stamp
    domestic_letter_2oz          = 1.07,  # Two-ounce stamp
    domestic_letter_3oz          = 1.36,  # Three-ounce stamp
    domestic_postcard            = 0.61,  # Postcard stamp
    domestic_additional_oz       = 0.29,  # Each additional oz over 1oz
    domestic_flat_1oz            = 1.63,  # Large envelope/flat, 1oz
    domestic_flat_additional_oz  = 0.29,  # Each additional oz for flats
    nonmachinable_surcharge      = 0.40,  # Square/rigid/unusual envelopes
    # International
    international_letter_1oz     = 1.70   # Global Forever stamp (any country)
  )
  
  # ── All purchasable stamp denominations ───────────────────────────────────
  stamps <- tribble(
    ~category,      ~name,                             ~value,
    
    # Small make-up / additional postage stamps
    "Makeup",       "1¢ (Fringed Tulip)",               0.01,
    "Makeup",       "2¢ (Daffodils)",                   0.02,
    "Makeup",       "3¢ (Peonies)",                     0.03,
    "Makeup",       "4¢ (Angel's Trumpets)",             0.04,
    "Makeup",       "5¢ (Red Tulips)",                   0.05,
    "Makeup",       "10¢ (Poppies)",                     0.10,
    "Makeup",       "40¢ (Additional Postage)",          0.40,
    
    # Standard domestic stamps
    "Domestic",     "Forever / 1oz Letter ($0.78)",      0.78,
    "Domestic",     "Postcard ($0.61)",                   0.61,
    "Domestic",     "Two-Ounce Letter ($1.07)",           1.07,
    "Domestic",     "Three-Ounce Letter ($1.36)",         1.36,
    "Domestic",     "Additional Ounce ($0.29)",           0.29,
    "Domestic",     "Non-Machinable Surcharge ($0.40)",   0.40,
    
    # International stamps
    "International","Global Forever ($1.70)",             1.70
  )
  
  list(rates = rates, stamps = stamps)
}
