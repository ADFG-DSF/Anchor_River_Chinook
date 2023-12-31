
# Total Run by Age --------------------------------------------------------
#LFW modifactions: Changed the -> "\\[\\d+,\\d\\]" structure to only be node[X,Y]. Before it was node[X,Y,Z].
# There is most likely a more intuitive way to extract Year and ages from nodes besides case matching character strings. 
# Changed the node N.tas to N.ta 



#'
#' Produces a table of total run by age along with cv's.
#'
#' @param post_dat The SRA model JagsUI output
#' @param node The posterior node of interest as a character string; p(age at maturity), q(age at return) or N.tas(Number at return)
#' @param run numeric. 1 for the first run, 2 for the second run
#'
#' @return A table
#'
#' @examples
#' table_age(post, 1)
#'
#' @export
table_age <- function(post_dat, node, firstyr = 1977){
  stopifnot(node %in% c("p", "q", "N.ta"))

  get_array <- function(post_dat, node, statistic){
    pattern <- paste0("^", node, "\\[\\d+,\\d\\]")
    df <-
      post_dat[["summary"]] %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      dplyr::filter(grepl(pattern, rowname)) %>%
      dplyr::mutate(yr = as.numeric(gsub(".*\\[(\\d+),\\d\\]", "\\1", rowname)),
                    age = as.numeric(gsub(".*\\[\\d+,(\\d)\\]", "\\1", rowname))) %>%
      dplyr::select_("yr", "age", stat = statistic)
    colnames(df) <- c("year_id", "age", statistic)
    df
  }

  mean <- get_array(post_dat, node, "mean")

  sd <- get_array(post_dat, node, statistic = "sd")

  yname <- names(mean)[grepl("year", names(mean))]

  temp <-
    dplyr::left_join(mean, sd, by = c(yname, "age")) %>%
    dplyr::mutate(CV = sd/mean,
                  print = paste0(digits(mean), " (", digits(if(node == "N.ta") CV else sd), ")")) %>%
    dplyr::select(which(grepl(paste0(yname, "|age|print"), names(.)))) %>%
    tidyr::spread(age, print) %>%
    dplyr::mutate(year = if(node == "p"){firstyr - 1 - 7 + year_id} else{firstyr - 1 + year_id}) %>%
    select(-year_id) %>%
    relocate(year)

  colnames(temp) <- c(if(node == "p") "Brood Year" else("Calendar Year"),
                      paste0(c("Age-3 (", "Age-4 (", "Age-5 (", "Age-6 ("), if(node == "N.ta") "CV)" else("sd)")))

  knitr::kable(temp, escape = FALSE, align = "r")
}
#dplyr::funs(if(node == "p") {1985 - 7 + .} else(1985 + .))



# Brood Table -------------------------------------------------------------


#' Brood table
#'
#' Produces an brood tables based on the posterior means form the state space model.  Most of the table functions in this package are intended
#'  for output into word via markdown.  This one produces a tibble object that can be output to excel using WriteXLS::WriteXLS.
#'
#' @param stats_dat The output from get_summary() for the SRA model mcmc.list output
#' @param firstyr numeric. inherited from table_age format. Makes code more versatile for other projects? 
#'
#' @return A tibble
#'
#' @examples
#' table_brood(get_summary(post), 1)
#'
#' @export
table_brood <- function(stats_dat,firstyr = 1977){
  N_ta <-
    stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("^N.ta\\[\\d+,\\d\\]"), rowname)) %>%
    dplyr::select(rowname = "rowname", mean = "Mean") %>%
    dplyr::mutate(age_n = 2 + as.numeric(gsub("N.ta\\[\\d+,(\\d)\\]", "\\1", rowname)),
                  year = firstyr + as.numeric(gsub("N.ta\\[(\\d+),\\d\\]", "\\1", rowname)) - age_n,
                  age_c = paste0("age-", age_n)) %>%
    dplyr::select(year, age_c, mean) %>%
    #tidyr::spread(age_c, mean)
    pivot_wider(names_from = age_c, values_from = mean)

  stats_dat %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(grepl(paste0("^S\\[\\d+|^R\\[\\d+"), rowname)) %>%
    dplyr::select(rowname = "rowname", mean = "Mean") %>%
    dplyr::mutate(name = stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
                  index = as.numeric(gsub(".\\[(\\d+)\\]", "\\1", rowname)),
                  year = (name == "S") * (firstyr + index) + (name == "R") * (firstyr - 7 + index)) %>%
    dplyr::select(year, mean, name) %>%
    tidyr::spread(name, mean) %>%
    dplyr::select(year, S, R) %>%
    dplyr::full_join(N_ta, by = "year") %>%
    dplyr::mutate_all(as.integer) 
}

# Index of abundance table ------------------------------------------------


#' Abundance information for early run
#'
#' Produces a table of indices of abundance for the Kenai River early run.
#'
#' @param dat_erinput
#'
#' @return A table
#'
#' @examples
#' table_erindex(dat_erinput)
#'
#' @export
# table_erindex <- function(dat_input){
#   Nlr <- get_summary(post_lr) %>%
#     dplyr::select_(Nlr = as.name("50%")) %>%
#     tibble::rownames_to_column() %>%
#     dplyr::filter(grepl("N\\[", rowname)) %>%
#     dplyr::select(-rowname)  #make sure N^lr numbers match those in table_state()
# 
#   dat_input %>%
#     dplyr::bind_cols(Nlr) %>%
#     dplyr::mutate(Year = 1986:2015,
#                   didson = paste0(format(round(DLge75, 0), big.mark = ","), " (", round(cv.DS, 3), ")"),
#                   aris = paste0(format(round(ALge75, 0), big.mark = ","), " (", round(cv.AR, 3), ")"),
#                   ir = paste0(format(round(IR.hat, 0), big.mark = ","), " (", round(cv.IR, 2), ")")) %>%
#     dplyr::select(Year, NCPUE = ncpue, NASB = nasb, SCPUE = scpue, Nlr, didson, aris, ir) %>%
#     pixiedust::dust() %>%
#     pixiedust::sprinkle_colnames(Nlr = "${\\text{N}^{LR}}$", didson = "DIDSON (CV)", aris = "${\\text{ARIS (CV)}^a}$", ir = "${\\text{CR (CV)}^a}$") %>%
#     pixiedust::sprinkle(cols = 5, fn = quote(KenaiSRA:::digits(value))) %>%
#     pixiedust::sprinkle(fn = quote(KenaiSRA:::nareplace(value)))
# }
#needs tto be updated to creaate appb...
#formatting moved prior to pixiedust so that numbers are passes as characters with desired format.  markdown looses format choice somehow
#format each column individually for better readability


# SR parameter estimates (reduced model) --------------------------------------------------

#' Table of SR analysis paramerater estimates
#'
#' Produces a table of paramerater estimates for the SRA.
#'
#' @param stats_dat The output from KenaiSRA::get_summary()
#' @param error The variability parameter to tabulate. "CI" for a 90 percent credibility interval or "CV" for coefficient of variation.
#'
#' @return A table
#'
#' @examples
#' table_params(get_summary(post))
#'
#' @export
table_params_red <- function(post_dat, error = "CI"){
  stopifnot(error %in% c("CI", "CV"))
  
  lut <- data.frame(rowname = c("lnalpha",
                                "alpha",
                                "beta",
                                "phi",
                                "sigma.white",
                                "S.max",
                                "S.eq",
                                "S.msy",
                                "U.msy",
                                "D.sum",
                                "pi[1]",
                                "pi[2]",
                                "pi[3]",
                                "pi[4]",
                                "q.AS"),
                    Parameter = factor(
                                  c("ln($\\alpha$)",
                                  "$\\alpha$",
                                  "$\\beta$",
                                  "$\\phi$",
                                  "$\\sigma_{w}$",
                                  "$S_{MSR}$",
                                  "$S_{EQ}$",
                                  "$S_{MSY}$",
                                  "$U_{MSY}$",
                                  "D",
                                  "$\\pi_{1}$",
                                  "$\\pi_{2}$",
                                  "$\\pi_{3}$",
                                  "$\\pi_{4}$",
                                  "$\\lambda_{survey}$"),
                                  levels = c("ln($\\alpha$)", "$\\alpha$", "$\\beta$", "$\\phi$", "$\\sigma_{w}$",
                                             "$S_{MSR}$", "$S_{EQ}$", "$S_{MSY}$", "$U_{MSY}$",
                                             "D", "$\\pi_{1}$", "$\\pi_{2}$", "$\\pi_{3}$", "$\\pi_{4}$", "$\\lambda_{survey}$")),
                    stringsAsFactors = FALSE)
  
  temp <-
    post_dat[["summary"]][, c("50%", "sd", "2.5%", "97.5%")]  %>% 
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      dplyr::right_join(lut, by = "rowname") %>%
      dplyr::rename(median = "50%", q02.5 = "2.5%", q97.5 = "97.5%") %>%
      dplyr::mutate(cv = ifelse(grepl("^S.", rowname),
                                sqrt(exp(((log(q97.5)-log(abs(q02.5)))/1.645/2)^2)-1), #Geometric CV for lognormals, abs(q02.5) to suppresses NaN warning on phi
                                sd / abs(median))) %>%
      dplyr::mutate_at(c("median", "q02.5", "q97.5", "cv"), digits) %>%
      dplyr::mutate(print1 = paste0(median, " (", trimws(q02.5), " - ", trimws(q97.5), ")"),
                    print2 = paste0(median, " (", trimws(cv), ")"))
    
  if(error == "CI"){
    table <- 
      temp %>%
      dplyr::select(Parameter, print1)
    
    colnames(table)  <- c("Parameter", "Median (95% CI)")
  } else{
    table <- 
      temp %>%
      dplyr::select(Parameter, print2)
    
    colnames(table)  <- c("Parameter", "Median (CV)")
  }
  
  knitr::kable(table, align = "r", escape = FALSE)
}

# SR parameter estimates (full model) -------------------------------------

table_params_full <- function(post_dat, error = "CI", output = "kable"){
  stopifnot(error %in% c("CI", "CV"))
  stopifnot(output %in% c("df", "kable"))
  
  lut <- data.frame(rowname = c("lnalpha",
                                "alpha",
                                "beta",
                                "phi",
                                "sigma.white",
                                "S.max",
                                "S.eq",
                                "S.msy",
                                "U.msy",
                                "D.sum",
                                "pi[1]",
                                "pi[2]",
                                "pi[3]",
                                "pi[4]",
                                "q.ASearly",
                                "q.ASlate"),
                    Parameter = factor(
                      c("ln($\\alpha$)",
                        "$\\alpha$",
                        "$\\beta$",
                        "$\\phi$",
                        "$\\sigma_{w}$",
                        "$S_{MSR}$",
                        "$S_{EQ}$",
                        "$S_{MSY}$",
                        "$U_{MSY}$",
                        "D",
                        "$\\pi_{1}$",
                        "$\\pi_{2}$",
                        "$\\pi_{3}$",
                        "$\\pi_{4}$",
                        "$\\lambda_{survey77-88}$",
                        "$\\lambda_{survey89-07}$"),
                      levels = c("ln($\\alpha$)", "$\\alpha$", "$\\beta$", "$\\phi$", "$\\sigma_{w}$",
                                 "$S_{MSR}$", "$S_{EQ}$", "$S_{MSY}$", "$U_{MSY}$",
                                 "D", "$\\pi_{1}$", "$\\pi_{2}$", "$\\pi_{3}$", "$\\pi_{4}$",
                                 "$\\lambda_{survey77-88}$", "$\\lambda_{survey89-07}$")),
                    stringsAsFactors = FALSE)
  
  temp <-
    post_dat[["summary"]][, c("50%", "sd", "2.5%", "97.5%")]  %>% 
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::right_join(lut, by = "rowname") %>%
    dplyr::rename(median = "50%", q02.5 = "2.5%", q97.5 = "97.5%") %>%
    dplyr::mutate(cv = ifelse(grepl("^S.", rowname),
                              sqrt(exp(((log(q97.5)-log(abs(q02.5)))/1.645/2)^2)-1), #Geometric CV for lognormals, abs(q02.5) to suppresses NaN warning on phi
                              sd / abs(median))) %>%
    dplyr::mutate_at(c("median", "q02.5", "q97.5", "cv"), digits) %>%
    dplyr::mutate(print1 = paste0(median, " (", trimws(q02.5), " - ", trimws(q97.5), ")"),
                  print2 = paste0(median, " (", trimws(cv), ")"))
  
  if(error == "CI"){
    table <- 
      temp %>%
      dplyr::select(Parameter, print1)
    
    colnames(table)  <- c("Parameter", "Median (95% CI)")
  } else{
    table <- 
      temp %>%
      dplyr::select(Parameter, print2)
    
    colnames(table)  <- c("Parameter", "Median (CV)")
  }
  
  if(output == "df"){temp} else{knitr::kable(table, align = "r", escape = FALSE)}
}

# State Variables ---------------------------------------------------------
# Changed the regex formula to match not having a run argument. Changed the "Inriver.Run" to 
# just being "IR" to match model. 

#' State Variable Table
#'
#' Produces a table of escapement, recruitment, total run, and inriver run along with cv's.
#'
#' @param post_dat The SRA model jagsUI output
#' @param run numeric. 1 for the first run, 2 for the second run
#'
#' @return A table
#'
#' @examples
#' table_state(post, 1)
#'
#' @export
table_state <- function(post_dat, firstyr = 1977){
  temp <-
  post_dat[["summary"]][, c("50%", "mean", "sd")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(median = "50%") %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+|S\\[\\d+|N\\[\\d+|IR\\[\\d+"), rowname)) %>%
    dplyr::mutate(name = gsub("(.*)\\[\\d+\\]", "\\1", rowname),
                  index = as.numeric(gsub(".*\\[(\\d+)\\]", "\\1", rowname)),
                  year = (name != c("R")) * (firstyr - 1 + index) + (name == "R") * (firstyr - 1 - 7 + index),
                  cv = sd/mean,
                  print = paste0(format(as.integer(median), big.mark = ","), " (", format(round(cv, 2), nsmall = 2), ")")) %>%
    dplyr::select(year, print, name) %>%
    tidyr::spread(name, print) %>%
    dplyr::select(year, N, IR, S, R) %>%
    dplyr::rowwise() %>%
    dplyr::mutate_all(nareplace) %>%
    as.data.frame()

  colnames(temp)  <- c(year = "Year", N = "Total Run (CV)", IR = "Inriver Run (CV)", S = "Escapement (CV)", R = "Recruitment (CV)")

  knitr::kable(temp, align = "r", escape = FALSE)
}