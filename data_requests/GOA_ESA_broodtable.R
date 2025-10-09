# Generate brood and state tables for a NOAA data request associated with the GOA ESA petition.

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
tbl_brood <- table_brood(stats_dat = get_summary(post$samples))

tbl_state <- post[["summary"]][, c("50%", "mean", "sd")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(median = "50%") %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+|S\\[\\d+|N\\[\\d+|IR\\[\\d+"), rowname)) %>%
    dplyr::mutate(name = gsub("(.*)\\[\\d+\\]", "\\1", rowname),
                  index = as.numeric(gsub(".*\\[(\\d+)\\]", "\\1", rowname)),
                  year = (name != c("R")) * (1977 - 1 + index) + (name == "R") * (1977 - 1 - 6 + index),
                  cv = sd/mean) %>%
    dplyr::select(year, name, median, cv) %>%
    dplyr::filter(name %in% c("N", "S", "R")) %>%
    tidyr::pivot_wider(names_from = name, values_from = c("median", "cv")) %>% 
  arrange(year)

# Results -----------------------------------------------------------------
knitr::kable(tbl_brood, escape = FALSE, align = "r")
write_xlsx(list(
  "Anchor" = tbl_brood),
  ".\\data_requests\\GOA_ESA_brood.xlsx")

knitr::kable(tbl_state, escape = FALSE, align = "r")
write_xlsx(list(
  "Anchor" = tbl_state),
  ".\\data_requests\\GOA_ESA_state.xlsx")
