#' Scale Statewide BEGs bounds by Kenai Smsy for plotting
#'
#' Create a data frame for geom_rug in OYP, ORP and EY plots
#'
#' @param Comp_Smsy Smsy for the stock to be plotted
#'
#' @return Data frame with columns lb_Kenai and ub_Kenai representing statewide bounds relative to Kenai Smsy.
#'
#' @examples
#' get_BEGbounds(5000)
#'
#' @export
get_BEGbounds <- function(Comp_Smsy){
  dat_chinBEGs %>% dplyr::mutate(lb_Kenai = Comp_Smsy*lb/Smsy,
                                 ub_Kenai = Comp_Smsy*ub/Smsy)
}

#' #' Lookup tables for index, year and location
#' #'
#' #' Creates lookup tables to switch between names jags array locations.
#' #'
#' #' @param indicies character vector with the indices of abundacne included in the analysis.
#' #' @param psi_id named vector of psi names and psi column position.
#' #' @param year_range 2 element numeric vector.  Range of years to use.
#' #'
#' #' @return writes names vectors to R's Global Environemnet where the names are informative
#' #' and the elements are jags array locations.
#' #'
#' #' @examples
#' #' get_ids(c("aris", "ncpue", "nasb", "didson", "scpue"))
#' #'
#' #' @export
#' get_ids <- function(indicies,
#'                     psi_id = c("trib_delta" = 1 , "trib_median" = 2, "main_delta" = 3, "main_median" = 4),
#'                     year_range = NULL) {
#'   index_sort <- sort(indicies)
#'   
#'   index_id <- 1:length(indicies)
#'   names(index_id) <- index_sort
#'   
#'   years_index <- plyr::laply(index_sort, function(x) {range(as.integer(get(x)$year))})
#'   if(!is.null(year_range)){
#'     years_index[, 1] <- ifelse(years_index[, 1] < year_range[1], year_range[1], years_index[, 1])
#'     years_index[, 2] <- ifelse(years_index[, 2] > year_range[2], year_range[2], years_index[, 2])
#'   }
#'   rownames(years_index) <- index_sort
#'   colnames(years_index) <- c("first", "last")
#'   
#'   year_id <- 1:(max(years_index) - min(years_index) + 1)
#'   names(year_id) <- min(years_index):max(years_index)
#'   
#'   loc_id <- ifelse(index_sort == "aris", 2, 1)
#'   names(loc_id) <- index_sort
#'   loc_id
#'   
#'   
#'   years_loc <- data.frame(first = aggregate(years_index, list(loc_id[rownames(years_index)]), min)[, 2],
#'                           last = aggregate(years_index, list(loc_id[rownames(years_index)]), max)[, 3],
#'                           row.names = aggregate(years_index, list(loc_id[rownames(years_index)]), min)[, 1]) %>%
#'     as.matrix()
#'   
#'   psi_id <- psi_id
#'   
#'   list <- list(indices_to_include = indicies,
#'                index_id = index_id,
#'                years_index = years_index,
#'                years_loc = years_loc,
#'                year_id = year_id,
#'                loc_id = loc_id,
#'                psi_id = psi_id)
#'   list2env(list, .GlobalEnv)
#' }
#' 
#' #Initial values SRA model
#' get_SRAinits <- function(){
#'   list(D.scale = runif(2, .1, .5),
#'        beta = rlnorm(2, log(3.2e-5), 0.4),
#'        lnalpha = rlnorm(2, log(1.6), 0.4),
#'        log.resid.0 = rnorm(2, 0, 1),
#'        mean.log.R = rnorm(2, 11.3, 0.5),
#'        p.MR = runif(2, 0.6, 0.9),
#'        phi = runif(2, 0.25, 0.75),
#'        tau.R = runif(1, 1, 25),
#'        tau.white = runif(2, 1, 25),
#'        log.qer = c(-8.4,0.094,-12),
#'        tau.ier = c(3.199,21.02,32.57),
#'        log.qlr = c(-8.4,0.094,-12,-4),
#'        tau.ilr = c(3,21,32,32),
#'        log.R = matrix(c(rnorm(jags_dat$Y + 2, 9, 1), rnorm(jags_dat$Y + 2, 11, 1)), nrow = jags_dat$Y + 2, ncol = 2),
#'        mu.Habove = matrix(runif(2 * jags_dat$Y, 0.1, 0.5), nrow = jags_dat$Y, ncol = 2),
#'        psi = runif(2, 0, 0.5)
#'   )
#' }
#' 
#' #' Initial values
#' #'
#' #' Currently for psi, phi and sigma_fk
#' #'
#' #' @return a list of inital values
#' #'
#' #' @examples
#' #' get_inits()
#' #'
#' #' @export
#' get_inits <- function(){
#'   stopifnot(exists("year_id", .GlobalEnv),
#'             exists("years_index", .GlobalEnv))
#'   
#'   n_yr <- length(year_id)
#'   n_index <- length(index_id)
#'   
#'   get_indexinit <- function(dat_index, dat_size = NULL, expand = FALSE, run){
#'     stopifnot(exists("index_id", .GlobalEnv),
#'               exists("year_id", .GlobalEnv))
#'    
#'     dat <- get(as.character(dat_index)) %>% dplyr::filter(year %in% years_index[dat_index, 1]:years_index[dat_index, 2])
#'     if(is.null(dat_size) == TRUE) {start <- dplyr::rename_(dat, temp = names(dat)[3])}
#'     else(start <- dat %>% dplyr::left_join(get(as.character(dat_size)), by = c("year", "day")) %>% dplyr::mutate(temp = dat[[3]] * pct.fit))
#'     
#'     out <- start %>%
#'       dplyr::filter(if(run == 1) day <= 177 else(day > 177)) %>%
#'       dplyr::group_by(year) %>%
#'       dplyr::summarise(samp = sum(!is.na(temp)),
#'                        eos0 = sum(temp)) %>%
#'       dplyr::mutate(eos = if(expand == TRUE) eos0 *  77 / samp else(eos0)) %>%
#'       dplyr::right_join(data.frame(year = names(year_id), stringsAsFactors = FALSE), by = "year") %>%
#'       dplyr::select(eos) %>%
#'       dplyr::mutate_all(dplyr::funs(ifelse(is.na(.), 1, .))) %>%
#'       as.matrix()
#'     names(out) <- NULL
#'     out
#'   }
#'   
#'   list(D.scale = runif(2, .1, .5),
#'        beta = rlnorm(2, log(3.2e-5), 0.4),
#'        lnalpha = rlnorm(2, log(1.6), 0.4),
#'        log.resid.0 = rnorm(2, 0, 1),
#'        mean.log.R = rnorm(2, 11.3, 0.5),
#'        p.MR = runif(2, 0.6, 0.9),
#'        phi = runif(2, 0.25, 0.75),
#'        tau.R = runif(1, 1, 25),
#'        tau.white = runif(2, 1, 25),
#'        log.qtrib = c(-8.4,0.094,-13),
#'        tau.itrib = c(3.199,21.02,32.57),
#'        log.qmain = c(-8.4,0.094,-13,-4),
#'        tau.imain = c(3,21,32,32),
#'        log.R = matrix(c(rnorm(length(year_id) + 2, 9, 1), rnorm(length(year_id) + 2, 11, 1)), nrow = length(year_id) + 2, ncol = 2),
#'        mu.Habove = matrix(runif(2 * length(year_id), 0.1, 0.5), nrow = length(year_id), ncol = 2),
#'        trib =   Map(get_indexinit,
#'                     dat_index = indices_to_include,
#'                     dat_size = c(list(NULL), rep("dat_gt750net", 4)),
#'                     expand = c(rep(FALSE, 4), TRUE),
#'                     run = 1) %>%
#'                 do.call(cbind, .),
#'        main =   Map(get_indexinit,
#'                     dat_index = indices_to_include,
#'                     dat_size = c(list(NULL), rep("dat_gt750net", 4)),
#'                     expand = c(rep(FALSE, 4), TRUE),
#'                     run = 2) %>%
#'                 do.call(cbind, .),
#'        phi_index = runif(1, 0.25, 0.5),
#'        sigma_fk = runif(1, 50, 500),
#'        lnpsi = log(matrix(c(runif(n_yr, 22, 60),
#'                              runif(n_yr, 15, 40),
#'                              runif(n_yr, 22, 60),
#'                              runif(n_yr, 60, 75)),
#'                            n_yr, 4))
#'        )
#' }
#' 
#' #' Daily index of abundance data for jags
#' #'
#' #' Creates a tbl with columns that serve as input data for the jags code.
#' #'
#' #' @param dat_index Name of the index dataset as a character string.  Expects 3 columns: day (integer), year (character), index (double)
#' #' @param dat_size Name of the >= 750 dataset as a character string.  Expects 4 columns: day (integer), year (character), pct.fit (double), pct.se.fit (double).
#' #'  Defaults to NULL which assumes dat_index is for large fish.
#' #'
#' #' @return A tbl with columns: yr, day, index, loc, dat.  THe first 4 columns are for nested indexing, the last column is data.
#' #'
#' #' @examples
#' #' get_jagsdaily("aris")
#' #'
#' #' get_jagsdaily("ncpue", "dat_gt750net")
#' #'
#' #' Map(get_jagsdaily,
#' #'     dat_index = c("aris", "ncpue", "nasb", "didson", "scpue"),
#' #'     dat_size = c(list(NULL), rep("dat_gt750net", 4)) %>%
#' #' do.call(rbind, .)
#' #'
#' #' @export
#' get_jagsdaily <- function(dat_index, dat_size = NULL){
#'   stopifnot(exists("index_id", .GlobalEnv),
#'             exists("year_id", .GlobalEnv),
#'             exists("loc_id", .GlobalEnv))
#'   
#'   dat <- get(as.character(dat_index)) %>% dplyr::filter(year %in% years_index[dat_index, 1]:years_index[dat_index, 2])
#'   
#'   if(is.null(dat_size)) {start <- dplyr::rename_(dat, temp = names(dat)[3])
#'   } else start <- dat %>% dplyr::left_join(get(as.character(dat_size)), by = c("year", "day")) %>% dplyr::mutate(temp = dat[[3]] * pct.fit)
#'   
#'   out <- start %>%
#'     dplyr::mutate(obs = ifelse(temp == 0, min(temp[temp > 0]), temp)) %>%
#'     dplyr::rename(day0 = day) %>%
#'     dplyr::mutate(yr = unname(year_id[year]),
#'                   day = day0 - 135,
#'                   index = index_id[dat_index],
#'                   loc = loc_id[dat_index]
#'     ) %>%
#'     dplyr::select(yr, day, index, loc, obs)
#'   out
#' }
#' 
#' #' Last day of sampling by index and year
#' #'
#' #' A utility function that needs to be exported for data prep
#' #'
#' #' @param indices Names of the index datasets as a character vector.
#' #' @param format A character string. "long" or "rectangle"
#' #'
#' #' @return If format = "long" for a tibble with columns; year, index and last.  If format = "rectangle" an array with years
#' #' as rows and index of abundance as columns.
#' #'
#' #' @examples
#' #' get_lastday(c("aris", "ncpue", "nasb", "didson", "scpue"), "long")
#' #'
#' #' @export
#' get_lastday <- function(format, indices = indices_to_include, years = year_id){
#'   stopifnot(exists("indices_to_include", .GlobalEnv),
#'             exists("year_id", .GlobalEnv),
#'             format %in% c("long", "rectangle"))
#'   
#'   long <- lapply(indices, function(x){
#'     name <- as.character(x)
#'     
#'     get(x) %>%
#'       dplyr::filter(year %in% names(year_id)) %>%
#'       dplyr::group_by(year) %>%
#'       dplyr::summarise(last = max(day) - 135) %>%
#'       dplyr::mutate(index = name)
#'   }) %>%
#'     do.call(rbind, .)
#'   
#'   rect <- long %>%
#'     tidyr::spread(index, last) %>%
#'     dplyr::select(-year) %>%
#'     dplyr::mutate_all(dplyr::funs(ifelse(is.na(.), 30, .))) %>%
#'     as.matrix()
#'   
#'   
#'   switch(format, "long" = return(long), "rectangle" = return(rect))
#' }

#' Creates a dataset for plotting OYP, ORP and EY plots
#'
#' This function creates a dataframe that can be used by plot_profile()
#'
#' @param post_dat An jagsUI object with nodes lnalpha, beta, S.msy, lnalpha.c from which you want to simulate SR relationships.
#' @param run numeric. 1 for the first run, 2 for the second run
#'
#' @return A data.frame
#'
#' @examples
#' get_profile(post, 1)
#' lapply(1:2, get_profile, post_dat = post)
#'
#' @export
get_profile <- function(post_dat){
  samples <- post_dat$mcmc.info$n.chains * post_dat$mcmc.info$n.samples 
  
  temp <-
    data.frame(beta = post_dat$sims.list[["beta"]], 
               lnalpha = post_dat$sims.list[["lnalpha"]],
               S.msy = post_dat$sims.list[["S.msy"]],
               lnalpha.c = post_dat$sims.list[["lnalpha.c"]]) %>%
    as.data.frame() %>%
    dplyr::mutate(R.msy = S.msy * exp(lnalpha.c - beta * S.msy),
                  R.max = 1/beta * exp(lnalpha.c - 1),
                  MSY = R.msy - S.msy) %>%
    dplyr::as_tibble() %>%
    tibble::rownames_to_column(var = "id_var")
  
  s <- seq(0, median(temp$S.msy) * 4, by = median(temp$S.msy) * 4 / 1000)
  
  dplyr::inner_join(temp,
                    data.frame(id_var = as.character(rep(1:samples, each = length(s))), 
                               s = rep(s, samples), stringsAsFactors = FALSE),
                    by = "id_var",
                    multiple = "all") %>%
    dplyr::mutate(Rs = s  * exp(lnalpha.c  - beta * s),
                  #Rsr = s * exp(lnalpha.c.recent - beta * s),
                  SY = Rs - s,
                  #SYr = Rsr - s,
                  OYP70 = (SY - 0.7 * MSY) > 0,
                  OYP80 = (SY - 0.8 * MSY) > 0,
                  OYP90 = (SY - 0.9 * MSY) > 0,
                  ORP70 = (Rs - 0.7 * R.max) > 0,
                  ORP80 = (Rs - 0.8 * R.max) > 0,
                  ORP90 = (Rs - 0.9 * R.max) > 0,
                  OFP70 = (SY - 0.7 * MSY) < 0 & (s < S.msy),
                  OFP80 = (SY - 0.8 * MSY) < 0 & (s < S.msy),
                  OFP90 = (SY - 0.9 * MSY) < 0 & (s < S.msy)) %>%
    dplyr::select(s, dplyr::starts_with("O")) %>%
    dplyr::group_by(s) %>%
    dplyr::summarise(across(starts_with("O"), function(x) mean(x, na.rm = TRUE)))
}

#' #' Identify parameters with questionable convergence.
#' #'
#' #' Return parameters from a RJags posterior object with an Rhat that exceed a user specified cutoff.
#' #'
#' #' @param post JagsUI posterior object
#' #' @param cutoff returns parameters with an Rhat that exceeds the cutoff.
#' #'
#' #' @return a list with 2 elements.  The parameters with a Rhat exceeding the cutoff and the Rhat values forming the 90%+ quartiles.
#' #'
#' #' @examples
#' #' get_Rhat(post)
#' #'
#' #' @export
#' get_Rhat <- function(post, cutoff = 1.1){
#'   temp <- post$summary[, colnames(post$summary) == "Rhat"]
#'   list(data.frame("Rhat" = temp[temp > cutoff]),
#'        "R^ quantiles" = quantile(post$summary[colnames(post$summary) == "Rhat"], probs = seq(0.9, 1, by = .01)))
#' }
#' 
#' #' Print posterior summaries.
#' #'
#' #' Creates a matrix with posterior summary statistics.
#' #'
#' #' @param post Posterior data object.
#' #' @param param Character string.
#' #'
#' #' @return A matrix with rows of years and columns of summary statistics.
#' #'
#' #' @examples
#' #' get_table(post, "psi")
#' #'
#' #' @export
#' get_table <- function(post, param, format = NULL){
#'   stopifnot(exists("year_id", .GlobalEnv),
#'             exists("years_loc", .GlobalEnv),
#'             exists("psi_id", .GlobalEnv),
#'             format %in% c("list", "table", NULL))
#'   
#'   post_summary <- post[["summary"]][grep(paste0("^", param, "\\["), rownames(post$summary)), ] %>%
#'     as.data.frame() %>%
#'     tibble::rownames_to_column()
#'   class(post_summary) <- c(class(post_summary),
#'                            if(param %in% c("sigma", "trib", "main")) "trib" else(as.character(param)))
#'   
#'   if(is.null(format) & (("psi" %in% class(post_summary)) | ("trib" %in% class(post_summary)))) stop("Please specify and output format")
#'   if(!is.null(format) & !(("psi" %in% class(post_summary)) | ("trib" %in% class(post_summary)))) warning("Output format unnecessary... ignored")
#'   
#'   print_summary <- function(x) UseMethod("print_summary", x)
#'   
#'   print_summary.default <- function(x){
#'     x
#'   }
#'   
#'   print_summary.psi <- function(x){
#'     temp <- x %>%
#'       dplyr::mutate(year = names(year_id[as.numeric(gsub(".*\\[(\\d{1,2})\\,.*", "\\1", rowname))]),
#'                     loc = gsub(".*\\,(\\d{1})\\]", "\\1", rowname),
#'                     loc_print = ifelse(loc == "1", "rm 8", "rm 13"),
#'                     rt_parameter = names(psi_id[as.numeric(gsub(".*\\,(\\d{1})\\,.*", "\\1", rowname))])) %>%
#'       dplyr::filter(year >= years_loc[loc, 1] & year <= years_loc[loc, 2]) %>%
#'       dplyr::select(parameter = rowname, year, location = loc_print, rt_parameter, dplyr::everything())
#'     
#'     temp1 <- sapply(unique(temp$rt_parameter), function(x) temp[temp$rt_parameter == x, ], simplify = FALSE, USE.NAMES = TRUE)
#'     
#'     temp2 <-
#'       temp %>%
#'       dplyr::mutate(print = ifelse(grepl("median", rt_parameter),
#'                                    paste0(format(as.Date("2015/5/16") + mean, "%b-%d"), " (", format(sd, TRUE, digits = 1), ")"),
#'                                    paste0(format(mean, TRUE, digits = 3), " (", format(sd, TRUE, digits = 1), ")"))) %>%
#'       dplyr::select(year, location, rt_parameter, print) %>%
#'       tidyr::spread(rt_parameter, print)%>%
#'       dplyr::select(year, location, dplyr::contains("trib"), dplyr::contains("main"))
#'     
#'     switch(format,
#'            "list" = temp1,
#'            "table" = temp2,
#'            print("no format specified"))
#'   }
#'   
#'   print_summary.nu <- function(x){
#'     temp1 <- x %>%
#'       dplyr::mutate(covariate = c("Secchi", "Flow", "Bait", "Bait*secchi"),
#'                     percent_change = exp(mean) - 1) %>%
#'       dplyr::select(parameter = rowname, covariate, percent_change, dplyr::everything())
#'     
#'     q <- c(0.05, .25, .5, .75, .95)
#'     temp2 <-
#'       expand.grid(sec = quantile(scpue_covariates$secchi, probs = q, na.rm = TRUE),
#'                   flow = quantile(scpue_covariates$flow, probs = q, na.rm = TRUE)) %>%
#'       dplyr::mutate(scpue = exp(temp1[temp1$covariate == "Secchi", "mean"] * sec +
#'                                   temp1[temp1$covariate == "Flow", "mean"] * flow),
#'                     scpue_bait = exp(temp1[temp1$covariate == "Secchi", "mean"] * sec +
#'                                        temp1[temp1$covariate == "Bait*secchi", "mean"] * sec +
#'                                        temp1[temp1$covariate == "Bait", "mean"] +
#'                                        temp1[temp1$covariate == "Flow", "mean"] * flow),
#'                     print = paste0(digits(scpue), " (", digits(scpue_bait), ")")) %>%
#'       dplyr::select(-dplyr::starts_with("scpue")) %>%
#'       tidyr::spread(flow, print) %>%
#'       dplyr::select(-sec) %>%
#'       'row.names<-'(paste0("secchi@q", q)) %>%
#'       knitr::kable(digits = 2,
#'                    col.names = paste0("flow@q", q),
#'                    caption = "Change in SCPUE at various secchi and flow quartiles in unbaited (and baited) fisheries")
#'     
#'     # temp3 <-
#'     #   expand.grid(sec = quantile(scpue_covariates$secchi, probs = q, na.rm = TRUE),
#'     #               flow = quantile(scpue_covariates$flow, probs = q, na.rm = TRUE)) %>%
#'     #   dplyr::mutate(scpue = exp(temp1[temp1$covariate == "Secchi", "mean"] * sec +
#'     #                               temp1[temp1$covariate == "Bait*secchi", "mean"] * sec +
#'     #                               temp1[temp1$covariate == "Bait", "mean"] +
#'     #                               temp1[temp1$covariate == "Flow", "mean"] * flow)) %>%
#'     #   tidyr::spread(flow, scpue) %>%
#'     #   dplyr::select(-sec) %>%
#'     #   'row.names<-'(paste0("secchi@q", q)) %>%
#'     #   knitr::kable(digits = 2,
#'     #                col.names = paste0("flow@q", q),
#'     #                caption = "Change in SCPUE at various secchi and flow quartiles with bait")
#'     
#'     list("parameters" = temp1, "scpue w nobait(bait)" = temp2)
#'   }
#'   
#'   print_summary.mu_delta <- function(x){
#'     x %>%
#'       dplyr::mutate(stock = ifelse(as.numeric(gsub(".*(\\d{1})\\]", "\\1", rowname)) == 1, "Tributary", "Mainstem")) %>%
#'       dplyr::select(parameter = rowname, stock, dplyr::everything())
#'   }
#'   
#'   print_summary.delta <- function(x){
#'     x %>%
#'       dplyr::mutate(year = names(year_id[as.numeric(gsub(".*\\[(\\d{1,2})\\,.*", "\\1", rowname))]),
#'                     stock = ifelse(as.numeric(gsub(".*\\,(\\d{1})\\]", "\\1", rowname)) == 1, "Tributary", "Mainstem")) %>%
#'       dplyr::filter(year >= years_loc[2, 1] & year <= years_loc[2, 2]) %>%
#'       dplyr::select(parameter = rowname, year, stock, dplyr::everything())
#'   }
#'   
#'   print_summary.trib <- function(x){
#'     temp <- x %>%
#'       dplyr::mutate(year = names(year_id[as.numeric(gsub(".*\\[(\\d+),\\d\\]", "\\1", rowname))]),
#'                     index = names(index_id[as.numeric(gsub(".*\\[\\d+,(\\d)\\]", "\\1", rowname))])) %>%
#'       dplyr::select(parameter = rowname, year, index, dplyr::everything()) %>%
#'       dplyr::mutate(first = years_index[index, 1],
#'                     last = years_index[index, 2]) %>%
#'       dplyr::mutate_at(c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%"), dplyr::funs(ifelse(year < first | year > last, NA, .))) %>%
#'       dplyr::select(-first, - last)
#'                     
#'     temp1 <- sapply(unique(temp$index), function(x) temp[temp$index == x, ], simplify = FALSE, USE.NAMES = TRUE)
#'     
#'     temp2 <-
#'       temp %>%
#'       dplyr::mutate(print = ifelse(is.na(mean), "-", paste0(digits(mean), " (", trimws(digits(sd)), ")"))) %>%
#'       dplyr::select(year, index, print) %>%
#'       tidyr::spread(index, print)
#'     
#'     switch(format,
#'            "list" = temp1,
#'            "table" = temp2,
#'            print("no format specified"))
#'   }
#'   
#'   print_summary.W <- function(x){
#'     x %>%
#'       dplyr::mutate(year = min(weir$year) - 1 + as.numeric(gsub(".*\\[(\\d)\\]", "\\1", rowname))) %>%
#'       dplyr::select(parameter = rowname, year, dplyr::everything())
#'   }
#'   
#'   print_summary.theta_w <- function(x){
#'     x %>%
#'       dplyr::mutate(year = min(weir$year) - 1 + as.numeric(gsub(".*\\[(\\d)\\]", "\\1", rowname))) %>%
#'       dplyr::select(parameter = rowname, year, dplyr::everything())
#'   }
#'   
#'   print_summary(post_summary)
#' }
#' 
#' #' Calculate WAIC
#' #'
#' #' Calculate WAIC to assess model fit
#' #'
#' #' @param dat Jags input daily data (call to get_jagsdaily)
#' #' @param post Jags Posterior object with mu and sigma CODA saved
#' #' @param ... group_by variables to calculate waic for groups
#' #'
#' #' @return waic2 (BDA pg 173)
#' #'
#' #' @examples
#' #' get_waic(dat_daily, post)
#' #' get_waic(dat_daily, post, index)
#' #' get_waic(dat_daily, post, yr, index)  %>% tidyr::spread(index, llpd) %>% print(n = 100)
#' #'
#' #' @export
#' get_waic <- function(dat, post, ...){
#'   stopifnot("mu" %in% names(post[["sims.list"]]),
#'             "sigma" %in% names(post[["sims.list"]]),
#'             is.null(post[["data"]][["psi_row"]]) == FALSE)
#'   
#'   mu <- post[["sims.list"]][["mu"]]
#'   sigma <- post[["sims.list"]][["sigma"]]
#'   
#'   groups <- dplyr::quos(...)
#'   
#'   lpd <- function(y, id, year, index){
#'     pd_s <- dnorm(log(y), mu[, id], sigma[, post[["data"]][["psi_row"]][year, index]])
#'     pd <- log(mean(pd_s))
#'     vlpd <- var(log(pd_s))
#'     list(pd, vlpd)
#'   }
#'   
#'   pd <- function(dat){
#'     dat %>%
#'       tibble::rowid_to_column() %>%
#'       dplyr::rowwise() %>%
#'       dplyr::mutate(pd = lpd(obs, rowid, yr, index)[[1]],
#'                     vlpd = lpd(obs, rowid, yr, index)[[2]]) %>%
#'       dplyr::ungroup()
#'   }
#'   
#'   pd(dat) %>%
#'     dplyr::group_by(!!!groups) %>%
#'     dplyr::summarise(llpd = -2*(sum(pd) + sum(vlpd)))
#' }
