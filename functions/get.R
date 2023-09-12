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


# Create id variable to assist with labels/plotting/data subsetting -------


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


# Create a OYP/ORP/OF profile dataset -------------------------------------


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

# Identify poorly converging parameters -----------------------------------

#' Identify parameters with questionable convergence.
#'
#' Return parameters from a RJags posterior object with an Rhat that exceed a user specified cutoff.
#'
#' @param post JagsUI posterior object
#' @param cutoff returns parameters with an Rhat that exceeds the cutoff.
#'
#' @return a list with 2 elements.  The parameters with a Rhat exceeding the cutoff and the Rhat values forming the 90%+ quartiles.
#'
#' @examples
#' get_Rhat(post)
#'
#' @export
get_Rhat <- function(post, cutoff = 1.1){
  temp <- post$summary[, colnames(post$summary) == "Rhat"]
  list(data.frame("Rhat" = temp[temp > cutoff]),
       "R^ quantiles" = quantile(post$summary[colnames(post$summary) == "Rhat"], probs = seq(0.9, 1, by = .01)))
}
