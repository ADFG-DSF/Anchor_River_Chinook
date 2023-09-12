# Used to format a column of numbers with very different ranges (table_parms.
digits <- function(p){
  ps <- ifelse(p < 0.01, format(p, digits = 3, scientific = TRUE),
               ifelse(p < 1, format(round(p, 2), nsmall = 2),
                      ifelse(p < 100, format(round(p, 1), nsmall = 1), format(round(p, 0), nsmall = 0, scientific = FALSE, big.mark = ","))))
  return(ps)
}

# # Used to replace NA with a single dash in table_state.
 nareplace <- function(value){if(stringr::str_detect(value, "NA") | is.na(value)) "-" else value}

