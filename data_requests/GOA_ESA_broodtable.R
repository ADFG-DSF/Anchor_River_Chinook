# Generate a brood table for a NOAA data request associated with the GOA ESA petition.

# Author: Adam Reimer
# Version: 2024-09-09

# Packages
packs <- c("tidyverse", "coda", "writexl")
lapply(packs, require, character.only = TRUE)

# Source functions
function_files <- list.files(path=".\\functions")
lapply(function_files, function(x) source(paste0(".\\functions\\", x)))
  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Read data ---------------------------------------------------------------
post <- readRDS(".\\output\\post_1977on")

#  * Format data ----------------------------------------------------------

# Analysis ----------------------------------------------------------------

# Results -----------------------------------------------------------------
tbl_brood <- table_brood(stats_dat = get_summary(post$samples))
knitr::kable(tbl_brood, escape = FALSE, align = "r")
write_xlsx(list(
  "Anchor" = tbl_brood),
  ".\\data_requests\\GOA_ESA_brood.xlsx")
