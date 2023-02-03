#' Pipe
#'
#' Use the pipe function \code{\%>\%} to turn function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A object and a function to apply to it
NULL

# Used to format a column of numbers with very different ranges (table_parms.
digits <- function(p){
  ps <- ifelse(p < 0.01, format(p, digits = 3, scientific = TRUE),
               ifelse(p < 1, format(round(p, 2), nsmall = 2),
                      ifelse(p < 100, format(round(p, 1), nsmall = 1), format(round(p, 0), nsmall = 0, scientific = FALSE, big.mark = ","))))
  return(ps)
}

# # Used to replace NA with a single dash in table_state.
# nareplace <- function(value){if(stringr::str_detect(value, "NA") | is.na(value)) "-" else value}
# 
# # estimated cumulative response for posterior parameters and a single predictor
# cr <- function(dat){
#   out <- dat$value * plogis(dat$day, dat$median, dat$delta / log(81))# / plogis(dat$last, dat$median, dat$delta / log(81))
#   out
# }
# 
# # estimated daily response for posterior parameters and a single predictor
# dr <- function(dat){
#   out <- dat$value * (plogis(dat$day, dat$median, dat$delta / log(81)) - plogis(dat$day - 1, dat$median, dat$delta / log(81)))
#   out
# }
# 
# # estimated daily response for posterior parameters and a single predictor
# dr_scpue <- function(dat){
#   out <- dat$value * (plogis(dat$day, dat$median, dat$delta / log(81)) - plogis(dat$day - 1, dat$median, dat$delta / log(81))) *
#     exp(dat$a1 * dat$secchi + dat$a2 * dat$flow + dat$a3 * (dat$reg_f == "Bait") + dat$a4 * (dat$reg_f == "Bait") * dat$secchi)
#   out
# }
# 
# #Creates a cumulative and daily data for plotting in plot_fit() from a tbl created by get_jagsdaily()
# get_plotdata <- function(dat){
#   stopifnot(exists("index_id", .GlobalEnv),
#             exists("year_id", .GlobalEnv))
#   
#   dat %>%
#     dplyr::rename(daily = obs) %>%
#     dplyr::group_by(yr) %>%
#     dplyr::mutate(index = names(index_id[index]),
#                   year = names(year_id[yr]),
#                   day_plot = as.Date(paste0("2015", " ", day + 135), "%Y %j"),
#                   cum = cumsum(daily)) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(year, day_plot, index, daily, cum, day)
# }
# 
# #Creates a tbl with the posterior median fit for plot_fit().  Several Methods exist.
# get_fit <- function(post, stat = "q50"){
#   stopifnot(exists("indices_to_include", .GlobalEnv),
#             exists("index_id", .GlobalEnv),
#             exists("year_id", .GlobalEnv),
#             exists("loc_id", .GlobalEnv),
#             exists("years_index", .GlobalEnv),
#             exists("psi_id", .GlobalEnv)
#   )
#   if(length(psi_id) != dim(post[[stat]][["psi"]])[2]) stop("You have not named all posterior psi parameters, rerun get_ids() with a psi_id argument")
#   
#   params <- names(post$mean)
#   
#   psi <- function(loc){
#     post[[stat]][["psi"]][, , loc] %>%
#       as.data.frame(row.names = names(year_id)) %>%
#       setNames(names(psi_id)) %>%
#       tibble::rownames_to_column("year") %>%
#       tidyr::gather(name, value, -year) %>%
#       dplyr::mutate(location = loc,
#                     stock = gsub("(.*)_.*", "\\1", name),
#                     param = gsub(".*_(.*)", "\\1", name)) %>%
#       dplyr::select(param, year, location, stock, value) %>%
#       tidyr::spread(param, value) %>%
#       dplyr::as.tbl()
#   }
#   
#   psi <- dplyr::bind_rows(psi(1), psi(2))
#   
#   index <- function(stock){
#     post[["q50"]][[stock]] %>%
#       as.data.frame() %>%
#       tibble::rowid_to_column(var = "yr") %>%
#       dplyr::mutate(year = names(year_id[yr])) %>%
#       dplyr::select(-yr) %>%
#       setNames(c(names(index_id), "year")) %>%
#       tidyr::gather(index, value, -year) %>%
#       dplyr::mutate(first = years_index[index, 1],
#                     last = years_index[index, 2]) %>%
#       dplyr::mutate(value = ifelse(year < first | year > last, NA, value),
#                     stock = stock,
#                     location = loc_id[index_id[index]]) %>%
#       dplyr::select(-first, - last) %>%
#       dplyr::filter(!is.na(value))
#   }
#   
#   # phi <- function(){
#   #   post[[stat]][["phi"]] %>%
#   #     as.data.frame(row.names = names(year_id)) %>%
#   #     setNames("phi") %>%
#   #     tibble::rownames_to_column("year") %>%
#   #     dplyr::as.tbl()
#   # } For use with Hierarchal MA1 term
#   
#   out <- function(x) UseMethod("out", x)
#   
#   out.default <- function(post){
#     dplyr::bind_rows(index("trib"), index("main")) %>%
#       dplyr::left_join(psi, by = c("year", "location", "stock"))
#   }
#   
#   out.MA1 <- function(post){
#     dplyr::bind_rows(index("trib"), index("main")) %>%
#       dplyr::left_join(psi, by = c("year", "location", "stock")) %>%
#       #dplyr::left_join(phi()), by = "year")
#       dplyr::mutate(phi_index = post[[stat]][["phi_index"]],
#                     a1 = post[[stat]][["nu"]][1],
#                     a2 = post[[stat]][["nu"]][2],
#                     a3 = post[[stat]][["nu"]][3],
#                     a4 = post[[stat]][["nu"]][4])
#   }
#   
#   out(post)
# }
# 
# #used by plot_age and table_age
# get_array <- function(dat, node, run, statistic = "Mean"){
#   pattern <- paste0(node, "\\[\\d+,\\d,", run)
#   df <- 
#     tibble::rownames_to_column(dat) %>%
#     dplyr::filter(grepl(pattern, rowname)) %>%
#     dplyr::mutate(year = as.numeric(gsub(".*\\[(\\d+),\\d,\\d\\]", "\\1", rowname)),
#                   age = paste0("age", gsub(".*\\[\\d+,(\\d),\\d\\]", "\\1", rowname))) %>%
#     dplyr::select_("year", "age", stat = statistic) %>%
#     tidyr::spread(age, stat)
#   yname <- ifelse(node == "p", "byear", "cyear")
#   colnames(df)[colnames(df) == 'year'] <- yname
#   df
# }
