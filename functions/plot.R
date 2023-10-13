
# Age composition plot ----------------------------------------------------

#' #' Plots of composition and abundance by age
#' #'
#' #' Faceted plot of age at maturity, age composition and total run by age.  Observed age composition is also plotted.
#' #'
#' #' @param input_dat The input dataset for the SRA model
#' #' @param post_dat The SRA model jagsUI ouput
#' #' @param run numeric. 1 for the first run, 2 for the second run
#' #'
#' #' @return A figure
#' #'
#' #' @examples
#' #' plot_age(dat_erinput, post, 1)
#' #'
#' #' @export
#' plot_age <- function(post_dat, run, firstyr = 1986){
#'   x=post_dat[["data"]][["x"]][, , run]
#'   n.a=rowSums(x)  #effective sample size
#'   Q.obs <- dplyr::as_tibble(x/n.a) %>%
#'     tibble::rownames_to_column(var = "year") %>%
#'     dplyr::rename(age1 = V1, age2 = V2, age3 = V3) %>%
#'     tidyr::pivot_longer(-year, names_to = "age", values_to = "prop") %>% 
#'     dplyr::group_by(year) %>%
#'     dplyr::arrange(year, desc(age)) %>%
#'     dplyr::mutate(prop = cumsum(prop), plot = "Age Composition") %>%
#'     dplyr::ungroup(year) %>%
#'     dplyr::mutate(year = firstyr - 1 + as.numeric(year))
#'   
#'   get_array <- function(post_dat, node, run, statistic = "mean"){
#'     pattern <- paste0("^", node, "\\[\\d+,\\d,", run, "\\]")
#'     df <- 
#'       post_dat[["summary"]] %>%
#'       as.data.frame() %>%
#'       tibble::rownames_to_column() %>%
#'       dplyr::filter(grepl(pattern, rowname)) %>%
#'       dplyr::mutate(yr = as.numeric(gsub(".*\\[(\\d+),\\d,\\d\\]", "\\1", rowname)),
#'                     age = paste0("age", as.numeric(gsub(".*\\[\\d+,(\\d),\\d\\]", "\\1", rowname)))) %>%
#'       dplyr::select_("yr", "age", prop = statistic)
#'     df
#'   }
#'   
#'   P.mn <- get_array(post_dat, "p", run) %>%
#'     dplyr::mutate(plot = "Age-at-Maturity")
#'   Q.mn <- get_array(post_dat, "q", run) %>%
#'     dplyr::mutate(plot = "Age Composition")
#'   N.mn <- get_array(post_dat, "N.tas", run) %>%
#'     dplyr::mutate(plot = "Total Run")
#'   
#'   dplyr::bind_rows(P.mn, Q.mn, N.mn) %>%
#'     dplyr::mutate(year = (plot != c("Age-at-Maturity")) * (firstyr - 1 + yr) + (plot == c("Age-at-Maturity")) * (firstyr - 1 - 7 + yr)) %>%
#'     ggplot2::ggplot(ggplot2::aes(x = year, y = prop, alpha = age)) +
#'     ggplot2::geom_area() +
#'     ggplot2::facet_grid(plot ~ ., scales = "free", switch = "y") +
#'     ggplot2::scale_x_continuous(breaks = seq(firstyr - 1 - 7, 2015, 3), minor_breaks = NULL) +
#'     ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
#'     ggplot2::geom_point(data = Q.obs, size = 3) +
#'     ggplot2::scale_alpha_discrete(name = NULL, labels = c("Age-5", "Age-6", "Age-7")) +
#'     ggplot2::labs(y = NULL, x = "Year") +
#'     ggplot2::theme_bw() +
#'     ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
#' }


# Comparison of Chinook BEGs ----------------------------------------------
 
#' #' Plot of Chinook BEG ranges relative to Smsy in Alaska.
#' #'
#' #' Function plots existing Chinook BEG ranges relative to Smsy for several Alaskan stocks.
#' #' Information about a proposed goal range will be plotted in a different color.
#' #'
#' #' @param existing dat_chinBEGs or another dataframe with 5 columns: Region(num), Stock(char), Smsy(num), lb (num), ub(num)
#' #' @param new a data frame with the columns specified above.
#' #'
#' #' @return A figure
#' #'
#' #' @examples
#' #' df <- data.frame("Region" = 3,
#' #'                  "Stock" = "Salcha",
#' #'                  "Smsy" = 4000,
#' #'                  "lb" = 2000,
#' #'                  "ub" = 6000,
#' #'                  stringsAsFactors = FALSE)
#' #' plot_chinBEGs(dat_chinBEGs, df)
#' #'
#' #' @export
#' plot_chinBEGs <- function(existing = dat_chinBEGs, new = NULL){
#'   
#'   temp1  <- if(!is.null(new)){dplyr::bind_rows(existing %>% dplyr::mutate(id = "Current goals"),
#'                                                new %>% dplyr::mutate(id = "Proposed goal(s)"))}
#'   else {existing %>% dplyr::mutate(id = "Current goals")}
#'   
#'   temp2 <- dplyr::mutate(temp1, lb_p = lb / Smsy,
#'                          ub_p = ub / Smsy)
#'   
#'   ggplot2::ggplot(temp2, ggplot2::aes(x = 1, xmin = lb_p, xmax = ub_p, y = Stock, color = id)) +
#'     ggplot2::geom_errorbarh(linetype = 1, size = 1) +
#'     ggplot2::geom_label(ggplot2::aes(x = lb_p + 0.15, label = Stock), show.legend = FALSE) +
#'     ggplot2::scale_x_continuous(breaks = seq(0, 3, 0.2)) +
#'     ggplot2::scale_y_discrete(breaks = NULL, limits = unique(temp2$Stock[rev(order(temp2$lb_p))])) +
#'     ggplot2::labs(x = expression(multiples~of~S[MSY]), y = "Chinook salmon stock") +
#'     ggplot2::theme(legend.position = "bottom", legend.title=ggplot2::element_blank())
#' }
#' 

# Model Fit Plot ----------------------------------------------------------
 
#' #' Early Run Model fit plots
#' #'
#' #' Produces a faceted plot of inriver, midriver and total run with the appropriately scaled indices of abundance that were used as inputs to the model.
#' #'
#' #' @param post_dat jagsUI posterior object
#' #'
#' #' @return A figure
#' #'
#' #' @examples
#' #' plot_ERfit(post)
#' #'
#' #' @export
#' plot_ERfit <- function(post_dat, firstyr = 1986){
#'   qhat <- 
#'     data.frame(index_name = c("ncpue", "nasb", "scpue"),
#'                q = post_dat[["q50"]][["q.ier"]],
#'                stringsAsFactors = FALSE)
#'   
#'   lut <- data.frame(index_name = c("ncpue", "nasb", "didson", "scpue", "IR9.hat", "aris", "ccpue"),
#'                     name = factor(c(rep("Midriver.Run", 3), "Inriver9", "Inriver9", "Inriver14", "N"),
#'                                   levels = c("N", "Inriver9", "Inriver9", "Midriver.Run", "Inriver14"),
#'                                   labels = c("Total", "Inriver(rm9)", "Inriver(rm9)", "Midriver(rm9)", "Inriver(rm14)")),
#'                     stringsAsFactors = FALSE)
#'   
#'   indicies1 <- 
#'     data.frame(post_dat[["data"]][["index1er"]], 
#'                post_dat[["data"]][["index2er"]], 
#'                post_dat[["data"]][["index3er"]]) %>%
#'     as.data.frame() %>%
#'     setNames(c("ncpue", "nasb", "scpue")) %>%
#'     tibble::rownames_to_column(var = "year") %>%
#'     dplyr::mutate(year = firstyr - 1 + as.numeric(year)) %>%
#'     tidyr::gather(index_name, raw, -year) %>%
#'     dplyr::left_join(qhat, by = "index_name") %>%
#'     dplyr::mutate(hooks = ifelse(index_name == "scpue", post_dat$data$hooks[, 1], 0),
#'                   bait = ifelse(index_name == "scpue", post_dat$data$bait[, 1], 0),
#'                   psi_hooks = ifelse(index_name == "scpue", post_dat$q50$psi[1], 0), 
#'                   psi_bait = ifelse(index_name == "scpue", post_dat$q50$psi[2], 0)) %>%
#'     dplyr::mutate(value = ifelse(!is.na(q), (raw)/(q + hooks*psi_hooks + bait*psi_bait), raw)) %>%
#'     dplyr::left_join(lut, by = "index_name")
#'   
#'   indicies2 <-
#'     data.frame(value = post_dat[["data"]][["IR9.hat"]][, 1],
#'                cv.IR = post_dat[["data"]][["cv.IR9"]][, 1]) %>%
#'     tibble::rownames_to_column(var = "year") %>%
#'     dplyr::mutate(year = firstyr - 1 + as.numeric(year),
#'                   index_name = "IR9.hat",
#'                   ub = exp(log(value) + 1.96 * sqrt(log(cv.IR * cv.IR + 1))),
#'                   lb = exp(log(value) - 1.96 * sqrt(log(cv.IR * cv.IR + 1)))) %>%
#'     dplyr::left_join(lut, by = "index_name")
#'   
#'   indicies3 <-
#'     data.frame(value = post_dat[["data"]][["ARIS"]][, 1],
#'                cv = post_dat[["data"]][["cv.AR"]][, 1]) %>%
#'     tibble::rownames_to_column(var = "year") %>%
#'     dplyr::mutate(year = firstyr - 1 + as.numeric(year),
#'                   index_name = "aris",
#'                   ub = exp(log(value) + 1.96 * sqrt(log(cv * cv + 1))),
#'                   lb = exp(log(value) - 1.96 * sqrt(log(cv * cv + 1)))) %>%
#'     dplyr::left_join(lut, by = "index_name")
#' 
#'   indicies4 <-
#'     data.frame(value = post_dat[["data"]][["DIDSON"]][, 1],
#'                cv = post_dat[["data"]][["cv.DS"]][, 1]) %>%
#'     tibble::rownames_to_column(var = "year") %>%
#'     dplyr::mutate(year = firstyr - 1 + as.numeric(year),
#'                   index_name = "didson",
#'                   ub = exp(log(value) + 1.96 * sqrt(log(cv * cv + 1))),
#'                   lb = exp(log(value) - 1.96 * sqrt(log(cv * cv + 1)))) %>%
#'     dplyr::left_join(lut, by = "index_name")
#'   
#'   post_dat[["summary"]] %>%
#'     as.data.frame() %>%
#'     dplyr::select_(value = as.name("50%"), lcb = as.name("2.5%"), ucb = as.name("97.5%")) %>%
#'     tibble::rownames_to_column() %>%
#'     dplyr::filter(grepl("^Midriver.Run\\[\\d+,1\\]|Inriver9\\[\\d+,1\\]|Inriver14\\[\\d+,1\\]", rowname)) %>%
#'     dplyr::mutate(name = factor(stringr::str_sub(rowname, 1, stringr::str_locate(rowname, "\\[")[, 1] - 1),
#'                                 levels = c("Inriver9", "Midriver.Run", "Inriver14"),
#'                                 labels = c("Inriver(rm9)", "Midriver(rm9)", "Inriver(rm14)")),
#'                   index = as.numeric(gsub(".*\\[(\\d+).*", "\\1", rowname)),
#'                   year = (firstyr - 1 + index)) %>%
#'     ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
#'     ggplot2::geom_line() +
#'     ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
#'     ggplot2::facet_grid(name ~ ., scales = "free_y", switch = "y") +
#'     ggplot2::labs(x = NULL, y = NULL) +
#'     ggplot2::coord_cartesian(xlim = c(firstyr - 1, 2021)) +
#'     ggplot2::geom_jitter(data = indicies1, ggplot2::aes(color = index_name, shape = index_name), size = 3, width = .3) +
#'     ggplot2::geom_pointrange(data = indicies2, ggplot2::aes(ymin = lb, ymax = ub, color = "IR.hat", shape = "IR.hat")) +
#'     ggplot2::geom_pointrange(data = indicies3, ggplot2::aes(ymin = lb, ymax = ub, color = "aris", shape = "aris")) +
#'     ggplot2::geom_pointrange(data = indicies4, ggplot2::aes(ymin = lb, ymax = ub, color = "didson", shape = "didson")) +
#'     ggplot2::scale_color_manual(name ="Index",
#'                                 breaks = c("aris", "IR.hat", "scpue", "didson", "nasb", "ncpue", "NhatLR"),
#'                                 values = c("aris" = "#e41a1c",
#'                                            "IR.hat" = "#377eb8",
#'                                            "scpue" = "#4daf4a",
#'                                            "didson" = "#e41a1c",
#'                                            "nasb" = "#377eb8",
#'                                            "ncpue" = "#4daf4a",
#'                                            "NhatLR" = "#e41a1c"),
#'                                 labels = c("ARIS", "CR", "SCPUE", "DIDSON", "NASB", "NCPUE", expression(paste(N[LR])))) +
#'     ggplot2::scale_shape_manual(name ="Index",
#'                                 breaks = c("aris", "IR.hat", "scpue", "didson", "nasb", "ncpue", "NhatLR"),
#'                                 values = c("aris" = 17,
#'                                            "IR.hat" = 17,
#'                                            "scpue" = 17,
#'                                            "didson" = 18,
#'                                            "nasb" = 18,
#'                                            "ncpue" = 18,
#'                                            "NhatLR" = 15),
#'                                 labels = c("ARIS", "CR", "SCPUE", "DIDSON", "NASB", "NCPUE", expression(paste(N[LR])))) +
#'     ggplot2::scale_x_continuous("Year", breaks = seq(firstyr - 1, 2021, 3), minor_breaks = NULL) +
#'     ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
#'     ggplot2::theme_bw() +
#'     ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")
#' }
#' 

# Expected Sustained Yeild Plot -------------------------------------------
 
#' #' Expected sustained yield plot
#' #'
#' #' Expected sustained yield plot with 50 percent confidence ribbon
#' #'
#' #' @param profile_dat Output of the profile data function
#' #' @param limit Upper bounds for plot c(xmax, ymax). Default (NULL) will pick bounds from the data.
#' #' @param rug Show scaled statewide goal ranges. Defaults to TRUE.
#' #' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#' #'
#' #' @return A figure
#' #'
#' #' @examples
#' #' plot_ey(get_profile(post, 1))
#' #' profiles <- lapply(1:2, get_profile, post_dat = post)
#' #' lapply(profiles, plot_ey)
#' #'
#' #' @export
#' plot_ey <- function(profile_dat, limit = NULL, rug = TRUE, goal_range = NA){
#'   rug_dat <- get_BEGbounds(median(profile_dat$S.msy))
#'   
#'   plot_dat <- profile_dat %>%
#'     dplyr::select(s, dplyr::starts_with("SY")) %>%
#'     dplyr::group_by(s) %>%
#'     dplyr::summarise(median.SY = median(SY, na.rm = TRUE),
#'                      p25.SY = quantile(SY, probs = 0.25, na.rm = TRUE),
#'                      p75.SY = quantile(SY, probs = 0.75, na.rm = TRUE)) %>%
#'     dplyr::mutate(p25.SY = ifelse(p25.SY < 0, ifelse(p75.SY >= 0, 0, NA), p25.SY),
#'                   p75.SY = ifelse(p75.SY < 0, NA, p75.SY)) %>%
#'     tidyr::gather(Productivity, SY, median.SY)
#'   
#'   if(is.null(limit)){
#'     ymax <- max(plot_dat$p75.SY) * 1.05
#'     xmax <- plot_dat$s[which(is.na(plot_dat$p75.SY))[1]]
#'     if(is.na(xmax)) 
#'       stop("Error: profile does not extend to escapements with zero yield, use a larger s_ub in get_profile()")
#'   }
#'   else {xmax <- limit[1]; ymax <- limit[2]}
#'   
#'   plot <-
#'     ggplot2::ggplot(plot_dat, ggplot2::aes(x = s, y = SY, color = Productivity)) +
#'     ggplot2::geom_line() +
#'     ggplot2::geom_ribbon(ggplot2::aes(x = s, ymin = p25.SY, ymax = p75.SY), inherit.aes = FALSE, alpha = 0.1) +
#'     ggplot2::scale_x_continuous("Spawners", labels = scales::comma) +
#'     ggplot2::scale_y_continuous("Expected Yield", labels = scales::comma) +
#'     ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax)) +
#'     ggplot2::scale_color_manual(name = "Productivity", labels = "1973-2013 broods", values = "black") +
#'     ggplot2::theme_bw()
#'   
#'   if(rug == TRUE) {
#'     plot2 <- plot +
#'       ggplot2::geom_rug(ggplot2::aes(x = lb_Kenai), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
#'       ggplot2::geom_rug(ggplot2::aes(x = ub_Kenai), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black")
#'   }
#'   else plot2 <- plot
#'   
#'   if(!anyNA(goal_range)) {
#'     plot2 + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#'                                data.frame(xmin = goal_range[1], xmax = goal_range[2], ymin = -Inf, ymax = Inf),
#'                                inherit.aes = FALSE, fill = "red", alpha = 0.2)
#'   }
#'   else plot2
#'   
#' }


# Horsetail plot ----------------------------------------------------------

#' Horsetail plot of plausible spawn-recruit relationships
#'
#' Produces a horsetail plot of the median Spawn-Recruit relationship.  Plot also shows 40 plausible Spawn-Recruit relationships in the background and Spawner and Recruit estimates with associated 90% CIs.
#'
#' @param post_dat SRA model jagsUI output
#' @param run numeric. 1 for the first run, 2 for the second run
#'
#' @return A figure
#'
#' @examples
#' plot_horse(post, 1)
#' lapply(1:2, plot_horse, post_dat = post)
#'
#' @export
plot_horse_red <- function(post_dat, firstyr = 1997){
  coeflines <-
    data.frame(beta = post_dat$sims.list[["beta"]], lnalpha = post_dat$sims.list[["lnalpha"]]) %>%
    dplyr::sample_n(40) %>%
    as.matrix() %>%
    plyr::alply(1, function(coef) {ggplot2::stat_function(fun=function(x){x * exp(coef[2] - coef[1] * x)}, colour="grey", alpha = 0.5)})
  
  param_50 <- 
    post_dat[["summary"]][c("beta", "lnalpha"), "50%", drop = FALSE] %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
  
  temp <- 
    post_dat[["summary"]][ , c("2.5%", "50%", "97.5%"), drop = FALSE] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(lb = "2.5%", median = "50%", ub = "97.5%") %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+\\]|^S\\[\\d+\\]|^Spre\\[\\d+\\]"), rowname)) %>%
    dplyr::mutate(name0 = gsub("(.*)\\[\\d+\\]", "\\1", rowname),
                  name = ifelse(name0 == "Spre", "S", name0),
                  index0 = as.numeric(gsub(".*\\[(\\d+)]", "\\1", rowname)),
                  index = ifelse(name0 == "S", index0 + 6, index0),
                  year = firstyr - 1 + index)
  
  v_dat <-  temp %>% dplyr::filter(name == "R") %>% dplyr::select(vlb = lb, vub = ub, year)
  h_dat <-  temp %>% dplyr::filter(name == "S") %>% dplyr::select(hlb = lb, hub = ub, year)

  text_dat <-  temp %>%
    dplyr::select(median, name, year) %>%
    tidyr::spread(name, median) %>%
    dplyr::filter(!is.na(R) & ! is.na(S)) %>%
    dplyr::inner_join(v_dat, by = "year") %>%
    dplyr::inner_join(h_dat, by = "year")
  
  upper <- max(quantile(c(text_dat$vub, text_dat$hub), 0.6), text_dat$R, text_dat$S)
  
  ggplot2::ggplot(text_dat, ggplot2::aes(x = S, y = R, label = year, ymin = vlb, ymax = vub, xmin = hlb, xmax = hub)) +
    ggplot2::geom_text() +
    ggplot2::geom_errorbar(linetype = 2) +
    ggplot2::geom_errorbarh(linetype = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50[2, 2] - param_50[1, 2] * x)}, size = 2, linetype = 2) +
    coeflines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, NA), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::geom_abline(slope = 1, size = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}

plot_horse_full <- function(post_dat, firstyr = 1977){
  coeflines <-
    data.frame(beta = post_dat$sims.list[["beta"]], lnalpha = post_dat$sims.list[["lnalpha"]]) %>%
    dplyr::sample_n(40) %>%
    as.matrix() %>%
    plyr::alply(1, function(coef) {ggplot2::stat_function(fun=function(x){x * exp(coef[2] - coef[1] * x)}, colour="grey", alpha = 0.5)})
  
  param_50 <- 
    post_dat[["summary"]][c("beta", "lnalpha"), "50%", drop = FALSE] %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
  
  temp <- 
    post_dat[["summary"]][ , c("2.5%", "50%", "97.5%"), drop = FALSE] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(lb = "2.5%", median = "50%", ub = "97.5%") %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+\\]|^S\\[\\d+\\]|^Spre\\[\\d+\\]"), rowname)) %>%
    dplyr::mutate(name0 = gsub("(.*)\\[\\d+\\]", "\\1", rowname),
                  name = ifelse(name0 == "Spre", "S", name0),
                  index = as.numeric(gsub(".*\\[(\\d+)]", "\\1", rowname)), #rename from index0 to index
                  #index = ifelse(name0 == "S", index0 + 6, index0),
                  year = (name != "R") * (firstyr - 1 + index) + (name == "R") * (firstyr - 1 - 6 + index)) #this line changes
  
  v_dat <-  temp %>% dplyr::filter(name == "R") %>% dplyr::select(vlb = lb, vub = ub, year)
  h_dat <-  temp %>% dplyr::filter(name == "S") %>% dplyr::select(hlb = lb, hub = ub, year)
  
  text_dat <-  temp %>%
    dplyr::select(median, name, year) %>%
    tidyr::spread(name, median) %>%
    dplyr::filter(!is.na(R) & ! is.na(S)) %>%
    dplyr::inner_join(v_dat, by = "year") %>%
    dplyr::inner_join(h_dat, by = "year")
  
  upper <- max(quantile(c(text_dat$vub, text_dat$hub), 0.6), text_dat$R, text_dat$S)
  
  ggplot2::ggplot(text_dat, ggplot2::aes(x = S, y = R, label = year, ymin = vlb, ymax = vub, xmin = hlb, xmax = hub)) +
    ggplot2::geom_text() +
    ggplot2::geom_errorbar(linetype = 2) +
    ggplot2::geom_errorbarh(linetype = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50[2, 2] - param_50[1, 2] * x)}, size = 2, linetype = 2) +
    coeflines +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, NA), minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous("Recruits", minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper), ylim = c(0, upper)) +
    ggplot2::geom_abline(slope = 1, size = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"))
}


# Profile plots -----------------------------------------------------------

#' OYP, ORP and/or OFP plots
#'
#' Produces a faceted plot of OYP, ORP or OFP with an overlay of the proposed goal range and a rug showing appropriately scaled upper and lower bounds of other statewide goals.
#'
#' @param profile_dat Output of the get_profile function
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#' @param rug Show scaled statewide goal ranges. Defaults to TRUE.
#' @param goal_range A vector with two element c(lower_bound, upper_bound). Defaults to NA.
#' @param profiles the profiles to plot as a character vector e.g. c("OYP", "OFP").  Defaults to c("OYP", "ORP", "OFP").
#' @param percent the percent of MSY to target as a management objective.  Defaults to c("70", "80", "90").
#'
#' @return A figure
#'
#' @examples
#' plot_profile(get_profile(post, 1), rug = FALSE, goal_range = NA, profiles = c("OYP", "OFP"))
#' profiles <- lapply(1:2, get_profile, post_dat = post)
#' lapply(profiles, plot_profile)
#'
#' @export
plot_profile <- function(profile_dat, limit = NULL, rug = TRUE, goal_range = NA, profiles = c("OYP", "ORP", "OFP"), percent = c("70","80","90")){
  temp <-unlist(lapply(profiles, function(x){paste0(x, percent)}))
  profile_label <- ggplot2::as_labeller(c('OYP' = "Optimum Yield Profile",
                                          'OFP' = "Overfishing Profile",
                                          'ORP' = "Optimum Recruitment Profile"))
  S.msy50 <- median(profile_dat$s[which.max(profile_dat$OYP90)]) #approximate
  rug_dat <- get_BEGbounds(S.msy50)
  
  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit
  
  plot <- profile_dat %>%
    dplyr::select_("s", .dots = temp) %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    dplyr::summarise(across(starts_with("O"), function(x) mean(x, na.rm = TRUE))) %>%
    tidyr::gather("key", "prob", -s, factor_key = TRUE) %>%
    dplyr::mutate(profile = factor(stringr::str_extract(key, "[A-Z]+"),
                                   levels = c("OYP", "OFP", "ORP")),
                  max_pct = stringr::str_extract(key, "[0-9]+")) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Spawners", limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous("Probability", breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_discrete(name = "Percent of Max.") +
    ggplot2::facet_grid(profile ~ ., labeller = profile_label) +
    ggplot2::theme_bw()
  
  if(rug == TRUE) {
    plot2 <- plot +     
      ggplot2::geom_rug(ggplot2::aes(x = lb_Kenai), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "darkgrey") +
      ggplot2::geom_rug(ggplot2::aes(x = ub_Kenai), data = rug_dat, inherit.aes = FALSE, sides = "b", color = "black")
  }
  else plot2 <- plot
  
  if(!anyNA(goal_range)) {
    plot2 + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               data.frame(xmin = goal_range[1], xmax = goal_range[2], ymin = -Inf, ymax = Inf),
                               inherit.aes = FALSE, fill = "grey", alpha = 0.2)
  }
  else plot2
}



# State variable plot -----------------------------------------------------

#' State Variable Plot
#'
#' Produces a faceted plot of escapement, recruitment, total run, Ricker residuals and harvest rate plotted with 95% confidence envelopes.
#'
#' @param post_dat The SRA model jagsUI output
#' @param run numeric. 1 for the first run, 2 for the second run
#' @param S_msr Logical (TRUE) indicating if S_msr shoud be included in the escapement panel.  Defaults to FALSE.
#'
#' @return A figure
#'
#' @examples
#' plot_state(post, 1, TRUE)
#'
#' @export
plot_state_full <- function(post_dat, S_msr = FALSE, firstyr = 1977){

  msy50 <-
    post_dat[["summary"]][, "50%"] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    setNames(c("rowname", "median")) %>%
    dplyr::filter(grepl(paste0("S.msy"), rowname)) %>% #"msy"
    dplyr::mutate(name = factor(stringr::str_sub(rowname, stringr::str_locate(rowname, ".")),
                                levels = c("S", "U"),
                                labels = c("Escapement", "Harvest Rate")))

  msr50 <-
    post_dat[["summary"]][, "50%"] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    setNames(c("rowname", "median")) %>%
    dplyr::filter(grepl(paste0("beta"), rowname)) %>% #|lnalpha.c
    dplyr::mutate(msr = ifelse(rowname == paste0("beta"), 1 / median, 1-1/exp(median)),
                  name = factor(c("S"), #, "U"
                                levels = c("S"), #, "U"
                                labels = c("Escapement"))) #, "Harvest Rate"

plot <-
  post_dat[["summary"]][, c("2.5%", "50%", "97.5%")] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(lcb = "2.5%", median = "50%", ucb = "97.5%") %>%
    dplyr::filter(grepl(paste0("^R\\[\\d+\\]|S\\[\\d+\\]|N\\[\\d+\\]|log.resid.vec\\[\\d+\\]"), rowname)) %>% #|mu.H\\[\\d+\\]
    dplyr::mutate(name = factor(gsub("(.*)\\[\\d+\\]", "\\1", rowname),
                                levels = c("S", "N", "R", "log.resid.vec"), #"mu.H", 
                                labels = c("Escapement", "Total Run", "Recruitment", "Ricker Residuals")), #"Harvest Rate", 
                  index = as.numeric(gsub(".*\\[(\\d+)]", "\\1", rowname)),
                  year = (name != c("Recruitment")) * (firstyr - 1 + index) +
                    (name == "Recruitment") * (firstyr - 1 - 6 + index)) %>%
    dplyr::filter(year >= firstyr - 1) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = median)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcb, ymax = ucb), inherit.aes = TRUE, alpha = 0.3) +
    ggplot2::facet_grid(name ~ ., scales = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_continuous("Year", breaks = seq(firstyr, 2022, 3), minor_breaks = NULL)  +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma)  +
    ggplot2::geom_hline(data = msy50, ggplot2::aes(yintercept = median), color = "red", linetype = 2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "black", linetype = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"), strip.placement = "outside")

if(S_msr == TRUE) {plot <- plot +     ggplot2::geom_hline(data = msr50, ggplot2::aes(yintercept = msr), color = "red", linetype = 5)}

plot
}

# Time series of escapement vrs. goal -------------------------------------

#' #' Plot of escapement vrs. proposed goals faceted by run
#' #'
#' #' Produces a faceted plot of model estimated escapement with 95% CI error bars overlain by proposed goal ranges for each run.
#' #'
#' #' @param post The SRA model jagsUI output
#' #' @param ergoal_range A vector with two elements c(lower_bound, upper_bound) providing upper and lower bounds for the proposed early run goal
#' #' @param lrgoal_range A vector with two elements c(lower_bound, upper_bound) providing upper and lower bounds for the proposed late run goal
#' #'
#' #' @return A figure
#' #'
#' #' @examples
#' #' plot_Swgoals(post, c(2800, 5600), c(13500, 27000))
#' #'
#' #' @export
#' plot_Swgoals <- function(post_dat, ergoal_range, lrgoal_range){
#'   dat_rect <- data.frame(run = c("Early Run", "Late Run"),
#'                          lb = c(ergoal_range[1], lrgoal_range[1]),
#'                          ub = c(ergoal_range[2], lrgoal_range[2]))
#'   
#'   dat_er <-
#'     post_dat[["summary"]] %>%
#'     as.data.frame() %>%
#'     tibble::rownames_to_column() %>%
#'     dplyr::filter(grepl("^S\\[\\d+,1\\]", rowname)) %>%
#'     dplyr::select_("rowname", Escapement = as.name("50%"), lb = as.name("2.5%"), ub = as.name("97.5%")) %>%
#'     dplyr::mutate(year = as.numeric(gsub("^S\\[(\\d+),1\\]", "\\1", rowname)) + 1985,
#'                   run = "Early Run")
#'   
#'   post_dat[["summary"]] %>%
#'     as.data.frame() %>%
#'     tibble::rownames_to_column() %>%
#'     dplyr::filter(grepl("^S\\[\\d+,2\\]", rowname)) %>%
#'     dplyr::select_("rowname", Escapement = as.name("50%"), lb = as.name("2.5%"), ub = as.name("97.5%")) %>%
#'     dplyr::mutate(year = as.numeric(gsub("^S\\[(\\d+),2\\]", "\\1", rowname)) + 1985,
#'                   run = "Late Run") %>%
#'     dplyr::bind_rows(dat_er) %>%
#'     ggplot2::ggplot(ggplot2::aes(x = year, y = Escapement)) +
#'     ggplot2::geom_line() +
#'     ggplot2::geom_pointrange(ggplot2::aes(ymin = lb, ymax = ub), linetype = 2) +
#'     ggplot2::geom_rect(data = dat_rect, ggplot2::aes(x = NULL, y = NULL, xmin = -Inf, xmax = Inf, ymin = lb, ymax = ub), fill = "red", alpha = 0.2) +
#'     ggplot2::scale_x_continuous("Year", breaks = seq(1985, 2015, 3), minor_breaks = NULL) +
#'     ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
#'     ggplot2::facet_grid(run ~ ., scales = "free_y") +
#'     ggplot2::theme_bw()
#' }
