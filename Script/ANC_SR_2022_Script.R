#2/3/2023 - Adam Reimer. Updated some of the priors to aid model fit. Ran full data model.
#12/8/2022 - Logan Wendling - Copied most of the previous 2019 code and updated best I could
# Uses data up to and including 2022.
# N.B. HUNT FOR "SPECIFIC" in the code when updating


#read in dat_chinBEG for profile comparisons
data_names <- list.files(path=".\\data")
lapply(data_names, function(x) load(paste0(".\\data\\", x), .GlobalEnv))

#source functions
function_files <- list.files(path=".\\functions\\")
lapply(function_files, function(x) source(paste0(".\\functions\\", x)))

library(tidyverse)
library(jagsUI)

# Reduced model analysis --------------------------------------------------
# CHOSEN MODEL =RED_21-46AR. SEE "Schem21-43R" sheet in ANC_CHIN_BASSR_JAGS_RED_21-43AR_RESULTS.xlsx workbook for schematics
# RED_21-43R = YEARS 27-46 with first 6 aerial surveys prior to 2003 used for the R[y] w/out direct spawning abund link PLUS AR(1) component in first 6 R[y]

upyear_red=22      # SPECIFIC
mod_red=c("_RED_21-46AR","OUT_YEARS_21-46AR")   # Chosen model; SPECIFIC model name (43)
projname_red=paste("ANC_CHIN_BASSR_JAGS_UPDATE_",upyear_red,mod_red[1],sep="") # Core name of BUGS model text and Excel results files.



#   * Data ---------------------------------------------------------------


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXX  DATA  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
if(T){
  # N.B. need byrow=TRUE for matrices
  # dat0-data that is relevant to the  RED_21-43AR model.  See previous versions (ANC_BASSR_JAGS_MOD_2_4_16.r) for data ever used in any of the models considered during model selection
  datanc_red=list(
    T=20,
    A=4,
    a.max=6,
    a.min=3,
    x = matrix(c(
      20,89,226,53,
      22,52,121,55,
      20,96,209,76,
      11,29,91,44,
      1,40,96,42,
      5,26,81,6,
      10,42,32,3,
      17,88,126,14,
      9,140,115,16,
      17,54,79,7,
      29,45,62,7,
      35,97,108,27,
      51,149,168,9,
      15,100,134,14,                      # 2016 age comp
      23, 44,107,12,                      # 2017 age comp
      8,109,107,15,                      # 2018 age comp
      10, 41,138,14,                      # 2019 age comp
      17, 48, 66,18,                      # 2020 age comp
      7, 19, 98,10,                      # 2021 age comp
      7, 51, 55, 4                        # 2022 age comp
    ),20, 4,byrow=TRUE),                 # 12/21/15   follows CHANGE 9/2/2014 to Buskin coho analysis
    
    n.tot= c(388 ,250, 401, 175, 179, 118,  87 ,245, 280 ,157, 143 ,267, 377, 263, 186,239,203,149,134,117),  # Sample sizes for age
    
    Air.Survey=c(647,834,651,899, 678, 528),  	# DGE SF counts from 2003-2008 (surveys discontinued after 2008); 528 associated with 2008 survey
    
    ASpre=c(477,789,685,752,414,748),       # ASpre are 1997-2002 air survey numbers(21-26); they inform first brood years
    
    #S.hat=c(13273,12016,11156,8945,9622, 5806, 3455,4449,3545,4509,4378,2497,10049,7149, 5796, 3162), # (OLD: before 2017-2018 SH-adjustment of 2/8/20-see emails with Holly). 2003-2018 escapement;
    S.hat=c(13273,12016,11156,8945,9622, 5806, 3455,4449,3545,4509,4378,2497,10049,7149, 5796, 3162, 5603,3624,4300,3123), # 2003-2019 escapement with new 2017-18 SH adj exc; 5603=2019 DIDSON plus weir escapement; see Anc_19_ALL.r
    
    S.cv=c(0.015,0.024,0.027,0.032,0.025,0.029,0.02,0.023,0.02,
           0.044,0.03,0.02,0.02,0.02,0.02, 0.02, 0.02,0.02,0.02,0.02),  # SJF: USE 0.02 for 2009,2011,2014 and 2015 when no sonar;Use 0.02 also for 2016-2019;(Note DIDSON numbers in 2018 assigned to SH and don't contribute to count)
    # NB DIDSON used in 2016-2019 but cv very low (0.01(2016),0.013(2017),0(2018..SH),0.014(2019) respectively). Be conservative and give them 0.02.
    # 2010-2022 I stayed with the 0.02 -LFW
    H.hat=c(1011,1561,1432,1349,  2081, 1612, 737,364,573,38,97,203,344,1384,845,40,151,364,1.0E-3,1.0E-3), # SJF: USE NA for 2019; see Area PF by report code; SWHS not availabale yet for 2019
    
    H.cv=c(0.16,0.13,0.16,0.14,0.16,0.15, 0.29,0.32,
           0.28,0.61,0.57,0.36,0.299,0.187,0.224, 1.0, 0.40,0.41,0.4,0.4),# DGE SWAG for 2019= mean(tail(H.cv,10)) (H.cv up to 2018); 2016=>0.187=259/1384 ; 2017=>0.224=189/845;2018=> 1.0=40/40 see Area PF by report code SE sheet
    E=c(12,15,15,15,15,20,8,12,15,9,12,12,13,20, 20,8,9,7,8,1.0E-3)     #  2/7/2023 Holly double checked
  )
  datanc_red$n.tot=rowSums(datanc_red$x) # Calculates sample size
} # DATA  ; SPECIFIC: numbers



#   * Model ---------------------------------------------------------------

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXX  MODEL DEFINITION --AND WRITE MODEL TO DIR XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
if(T){
  cat("
    model {

# Spawner Recruit analysis for Anchor River Chinook salmon.
# YEARS 21-43 with first 6 aerial surveys prior to 2003 used for the R[y] w/out direct spawning abund link PLUS AR(1) component in first 6 R[y]

#T=20 calendar years for which we have weir escapement and possibly age data-2003-2019
#A=4 return ages (3, 4, 5, 6) (Total age===> 1.1. 1.2,1.3 and 1.4)
#Data matrix is a T=17 By A=4 structure.
#BY Returns w/Ricker S-R Link with AR1 Errors
#R[y] = Total Return from BY y
#T+A-1 =20+4-1=23 BYs Represented in the age Data (=(T-a.min)+A+(a.min-1) ); BYS= 1997 (=2003-6) - 2016 (=2022-3)
#Oldest Age of Returning Fish     =6=a.max;
#Youngest Age of Returnung Fish=3=a.min.
#We DO NOT Have Spawning Abundances for the First A+a.min -1 = 4+3-1   = 6  => 1-6  BYs   (BYs 1997-2002)
#We DO        Have  Spawning Abundances for the Remaining T-a.min =17-3= 14 => 7-23 BYs   (BYs 2003-2019)
#                                              T      = 20
#                                              a.min  =  3
#                                              a.max  =  6
#                                              A      =  4

# Note: R[] corresponds to 1997 through 2019 (length=23)
# Note  S[] corresponds to 2003 through 2022 (length= 20)

#MODEL:

#  AR Set-Up
      for (y in (A+a.min):(T+A-1)) {
      log.R[y] ~ dt(log.R.mean2[y],tau.white,500)
      R[y] <- exp(log.R[y])
      log.R.mean1[y] <- log(S[y-a.max]) + lnalpha - beta * S[y-a.max]
      log.resid[y] <- log(R[y]) - log.R.mean1[y]
      }
      #log.resid.vec <- log.resid[(A+a.min):(T+A-1)]
      log.R.mean2[A+a.min] <- log.R.mean1[A+a.min] + phi * log.resid.0
      for (y in (A+a.min+1):(T+A-1)) {
      log.R.mean2[y] <- log.R.mean1[y] + phi * log.resid[y-1]
      }
      lnalpha ~ dnorm(0,1.0E-6)I(0,) #Truncation prevents negative log alpha. alpha <1 is unlikely in a sustainable population. Comm fish used the same trucation. 
      beta ~ dnorm(0,1.0E-1)I(6.6E-5,) #truncate at an Seq of 15,000
      tau.white ~ dgamma(0.01,0.01)
      alpha <- exp(lnalpha)
      sigma.white <- 1 / sqrt(tau.white)
      lnalpha.c <- lnalpha + (sigma.white * sigma.white / 2 / (1-phi*phi) )
      S.max <- 1 / beta
      S.eq <- lnalpha.c * S.max
      S.msy <- S.eq * (0.5 - 0.07*lnalpha.c)
      U.msy <- lnalpha.c * (0.5 - 0.07*lnalpha.c)

      #  BROOD YEAR RETURNS W/O DIRECT SR LINK:BASE ON AERIAL EXPANSION PLUS AR(1) COMPONENT- New for 2015 analysis; passed by Steve
      for(y in 1:a.max){
      Spre[y]~dunif(1,1.0E5)
      ASpre[y]~dlnorm(log.q.Spre[y],tau.log.AS)
      log.q.Spre[y]<-log.q.AS2+log.Spre[y]
      log.Spre[y]<-log(Spre[y])
      log.R[y]~dt(log.Rpre.mean2[y],tau.white,500)
      R[y]<-exp(log.R[y])
      log.Rpre.mean1[y]<-lnalpha+log(Spre[y])-beta*Spre[y]
      log.resid[y] <- log(R[y]) - log.Rpre.mean1[y]
      }
      log.Rpre.mean2[1] <- log.Rpre.mean1[1] + phi * log.resid.0
      for (y in (2:a.max)) {
      log.Rpre.mean2[y] <- log.Rpre.mean1[y] + phi * log.resid[y-1]
      }
      log.resid.0 ~ dnorm(0,tau.red)T(-3,3)
      tau.red <- tau.white * (1-phi*phi)
      sigma.red <- 1 / sqrt(tau.red)
      phi ~ dunif(-1, 0.95) #avoids 1 which  is instable and leads to unreasonably high lnalpha.c


      #  DIRICHLET GENERATION OF RETURNS AT AGE
      #  GENERATE ALL T+A-1 = 43 MATURITY SCHEDULES, USE ONLY THOSE NECESSARY
      # DGE: See notes in BUGS and BRUGS notes for explanaiton of this section
      #  D.scale ~ dbeta(1,9)
      #D.sum ~ dlnorm(4,6)  # from meta-analysis of 7 estimates of Dsum    #mod
      #D.scale <- 1 / sqrt(D.sum)
      D.scale ~ dunif(0,1)
      D.sum <- 1 / (D.scale * D.scale)


      pi[1] ~ dbeta(1,1)
      pi.2p ~ dbeta(1,1)
      pi.3p ~ dbeta(1,1)
      pi[2] <- pi.2p * (1 - pi[1])
      pi[3] <- pi.3p * (1 - pi[1] - pi[2])
      pi[4] <- 1 - pi[1] - pi[2] - pi[3]
      for (a in 1:A) {
      gamma[a] <- D.sum * pi[a]
      for (y in 1:(T+A-1)) {
      g[y,a] ~ dgamma(gamma[a],1)
      p[y,a] <- g[y,a]/sum(g[y,])
      }
      }
      for (a in 2:A) {
      sibratio[a] <- pi[a] / pi[a-1]
      }

      # ASSIGN PRODUCT OF P AND R TO ALL CELLS IN N MATRIX
      # y SUBSCRIPT INDEXES BROOD YEAR
      # y=.. IS THE BROOD YEAR OF THE OLDEST FISH IN YEAR 1 (upper right cell)
      # y=.. IS THE BROOD YEAR OF THE YOUNGEST FISH IN YEAR T (=..) (lower left cell) 													#DGE
      # FIRST DO INITIAL CELLS WITHOUT SR LINK (x's IN MATRIX BELOW)
      # THEN DO CELLS DESCENDING WITH SR LINK (y's IN MATRIX BELOW)

      # T      S       1 2 3 4 (Ages)
      # 2003   s       x x x x
      # 2004   s       y x x x
      # 20     s       y y x x
      # 20     s       y y y x
      # 20     s       y y y y
      # 20     s       y y y y
      # 20..   s       y y y y
      #        s       .
      # 2015   s       y y y y
      # 2016   s       y y y y
      # 2017   s       y y y y
      # 2018   s       y y y y
      # 2019   s       y y y y

      # ASSIGN PRODUCT OF P AND R TO ALL CELLS IN N MATRIX (Matt's code)
      for (a in 1:A) {
      for (y in a:(T + (a - 1))) {
      N.ta[y - (a - 1), (A + 1 - a)] <- p[y, (A + 1 - a)] * R[y]
      }
      }

      # MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N
      # INDEX t IS CALENDAR YEAR
      for (t in 1:T) {
      N[t] <- sum(N.ta[t,1:A])
      for (a in 1:A) {
      q[t,a] <- N.ta[t,a] / N[t]
      }
      #n[t] <- sum(x[t,1:A])
      #x[t,1:A] ~ dmulti(q[t,],n[t])
      x[t,1:A] ~ dmulti(q[t,],n.tot[t])
      }

      # APPLY MARINE HARVEST (not measured: based on 1997 Deep Creek mu = 4%, harvest was higher 1995 and before; i.e. for 19 years from 1977 through 1995)
      # mean of beta(50,950)=0.05
      #for (y in 1:19) {
      #mu.marine[y] ~ dbeta(50,950)
      #H.marine[y] <- mu.marine[y] * N[y]
      #IR[y] <- max(N[y] - H.marine[y],1)
      #}
      # mean of beta(40,960)=0.04
      for (y in 1:T) {                                                         																									#DGE
      mu.marine[y] ~ dbeta(40,960)
      H.marine[y] <- mu.marine[y] * N[y]
      IR[y] <- max(N[y] - H.marine[y],1)
      }

      # APPLY FRESHWATER HARVEST BELOW SONAR (=MU.T*N) TO GET SPAWNING ESCAPEMENT
      # DRAW EXPLOITATION RATES FROM COMMON BETA DISTN-hierachical
      B.scale ~ dunif(0,1)
      Q.H ~ dbeta(0.1,0.1)T(0.0000001,0.999999) #Truncate to avoid error of 'Slicer stuck at infinite density'
      B.sum <- 1 / B.scale / B.scale
      B[1] <- Q.H * B.sum
      B[2] <- B.sum - B[1]
      for (y in 1:T) {
      q.H[y] ~ dbeta(B[1],B[2])T(0.0000001,0.999999) #Truncate to avoid error of 'Slicer stuck at infinite density'
      F[y] <- q.H[y] * E[y]
      mu[y] <- 1 - exp(-F[y])
      H[y] <- mu[y] * IR[y]
      log.H[y] <- log(H[y])
      tau.log.H[y] <- 1 / H.cv[y] / H.cv[y]
      H.hat[y] ~ dlnorm(log.H[y],tau.log.H[y])
      S[y] <- max(IR[y] - H[y],1)
      }

      # OBSERVE AERIAL SURVEY COUNTS DURING first 6 YEARS- 2003-2008; USE TO LEVERGAE AERIAL COUNTS IN FIRST 6 BROOD YEARS W/OUT WEIR COUNTS
      log.q.AS2 ~ dnorm(0,1.0E-4)
      tau.log.AS ~ dgamma(0.1,0.1)
      sigma.log.AS <- sqrt(1/tau.log.AS)
      q.AS <- exp(log.q.AS2)
      q.AS.inv <- 1 / q.AS

      for(y in 1:6) {                             					#DGE Paired Aerial Survey and weir data for 2003-2008
      Air.Survey[y] ~ dlnorm(log.qS[y],tau.log.AS)
      log.qS[y] <- log.q.AS2 + log.S[y]
      }

      # WEIR DATA
      for(y in 1:T) {                                                         																								#DGE
      S.hat[y] ~ dlnorm(log.S[y],tau.log.S[y])
      log.S[y] <- log(S[y])
      tau.log.S[y] <- 1 / S.cv[y] / S.cv[y]
      }

    }
      ", file=paste(projname_red,"_MODEL.jag",sep=""))
  
} # MODEL SPECIFICATION : .jag; SPECIFIC updates in annotations here


# Chain params ------------------------------------------------------------
#specify chains here because of the way ewe are doing intis. Better to write a function which automatically generates inits
nchains <- 3
nb <- 50000
nt <- 200
ns <- 200000
 
#   * Inits ---------------------------------------------------------------
  
if(T){
  ini1_red=list(
    B.scale = 0.03,
    D.scale = 0.2,
    Q.H = 0.007218890595073482,
    beta = 1.124178669778089E-4,
    lnalpha = 1.338783290113077,
    log.q.AS2 = -2.87270171091565,
    log.resid.0 = 0.0437292250913513,
    phi = 0.4085737310890481,
    pi = c(0.08186550896067785,            NA,            NA,            NA),
    pi.2p = 0.2277409016251648,
    pi.3p = 0.7529542645626424,
    tau.log.AS = 3.754135323267579,
    tau.white = 18.54684104153244,
    #H.hat = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1),       # Done 2019
    
    g = matrix(c(
      5.350456983793456,12.14042256741815,55.62284745018294,20.88324991374627,
      19.69350620942554,62.78842244996497,17.16977381921095,8.212982613863549,
      52.51893926800602,13.0275893517386,10.87968292959776,22.85910914464972,
      13.97706025147766,12.62039608616902,20.13980254120176,47.25614711849315,
      7.266846845240096,24.5795681197197,75.44676094487109,17.66530040983674,
      24.80639022792742,53.79842320636524,20.76808406535992,8.998765541689092,
      52.81247982330103,17.66785052434414,4.378350731146027,21.66614027644304,
      17.33321056108252,11.45309471721048,26.2071970005356,58.08320827580205,
      7.219788685033764,15.32813295664652,51.11962651471244,14.60875562414948,
      26.73543368011418,46.66822203446996,19.83523930287884,7.480758058640787,
      44.80175953523263,19.30181407387566,5.271017668762489,16.22932248973823,
      20.09708117381976,11.0229101179567,23.82287141843975,49.63874225575773,
      5.541949022264385,21.75993916469551,58.36227266272564,15.77447261040662,
      20.00984447407456,57.82108211220505,18.09376665362165,5.596559454912147,
      61.83228571579335,14.1724222739327,12.34072223028771,23.05241300235147,
      20.9146243274962,6.208564892984356,21.36240412705023,60.29985981463911,
      13.68857530105607,22.21280797220706,49.14718846900355,24.19732831685385,
      44.80175953523263,20.30181407387566,4.271017668762489,17.22932248973823,
      5.741949022264385,22.75993916469551,58.36227266272564,15.77447261040662,
      10.68857530105607,20.21280797220706,35.14718846900355,21.19732831685385,
      44.80175953523263,20.30181407387566,4.271017668762489,17.22932248973823,
      5.741949022264385,22.75993916469551,58.36227266272564,15.77447261040662,
      10.68857530105607,20.21280797220706,35.14718846900355,21.19732831685385
    ),
    23,4,byrow=TRUE),    #Unsure where these values come from, just recopied the last 3 rows... - LW
    
    log.R = c(
      8.9,              9.1,              8.2,              8.5,                    8.1,
      8.751855086358638,9.197576585591872,9.175605587342712,9.18615804297054,       9.272992677723535,
      9.653296928071237,9.414705230777765,9.037436498534289,9.570211098803757,      9.440822216665074,
      9.244659492025479,9.067912485091426,9.6230777765, 9.4,9.2,9.2,9.2,9.2),# Unsure where these are derived from. Just repeated 9.2 for last three values
    
    q.H = c(
      0.007186511318970837,0.01125230325064814,0.009618202653655157,0.004237141075104568,0.009489240954800743,
      0.005651415421291692,0.01086176799161786,0.007913601076878949,0.005230254246562143,0.009189056426833219,
      0.005296051767506838,0.007003860693734387,0.0062179133442023, 0.007,0.0065, 0.008, 0.007,0.0065, 0.008, 0.007),#Repeated last three values. 
    
    mu.marine = c(
      0.04325263444661011,0.0409901645536648,0.04527632920542994,0.06028366111489206,0.04328725535589889,
      0.04045632398900765,0.05347097021719297,0.05233594891864612,0.05065178160394555,0.05774773898638204,
      0.0371248400847605,0.05467251075561203,0.04598240438848132,0.04730150559283569, 0.06,0.05, 0.045,0.06,0.05, 0.045)) #Repeated last three values
  
  # List different kinds of nodes to facilitate the creation of multiple inits for more than 1 chain (see below)
  poslinenode=c("B.scale","D.scale","Q.H","beta","lnalpha","log.q.AS2","log.resid.0", "tau.log.AS",
                "tau.white","g","log.R","q.H","mu.marine") #Removed ,"H.hat"
  propnode=c("pi","pi.2p","pi.3p")
  corrnode=c("phi")
  
  if(nchains==1){inianc_red=ini1_red}   # ini1anc is a list; stops here for nchains=1
  if(nchains %in% c(2, 3)){
    ini2_red=c(                     # Add some noise to original inits
      lapply(ini1_red[poslinenode],function(y){z=y+runif(1,-0.1,0.1)*y;return(z)}),
      lapply(ini1_red[propnode],function(y){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=0]=0.001;return(z)}),
      lapply(ini1_red[corrnode],function(y,corrnode){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=-0.999]=-0.99; return(z)}))
    inianc_red=list(ini1_red,ini2_red)
  }
  if(nchains==3){
    ini3_red=c(                   # Add some noise to original inits
      lapply(ini1_red[poslinenode],function(y){z=y+runif(1,-0.1,0.1)*y;return(z)}),
      lapply(ini1_red[propnode],function(y){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=0]=0.001;return(z)}),
      lapply(ini1_red[corrnode],function(y,corrnode){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=-0.999]=-0.99; return(z)}))
    inianc_red=list(ini1_red,ini2_red,ini3_red)}
  
}  # INITS: nchains chains ; SPECIFIC:numbers



#   * Run jags ------------------------------------------------------------

codavec_red=c("S.msy",
          "S.eq",
          "S.max",
          "lnalpha",
          "lnalpha.c",
          "beta",
          "alpha",
          "phi",
          "sigma.white",
          "sigma.red",
          "R.msy",
          "MSY",
          "U.msy",
          "pi",
          "S",
          "R",
          "R.fit",
          "R.fit2",
          "log.resid",
          "log.resid.0",
           "D.sum",
           "D.scale",
           "mu",
           "N",
           "N.ta",
            "IR",
            "q.AS",
            "p",   # Need p for plots
            "q",   # Need q for plots
            "Spre",
            "Q.H"
    )

post_red <- jags(data = datanc_red,
             parameters.to.save = codavec_red,
             inits = inianc_red,
             model.file = paste(projname_red,"_MODEL.jag",sep=""),
             n.chains = nchains,
             n.iter = ns,
             n.burnin = nb,
             n.thin = nt,
             parallel = TRUE,
             store.data = TRUE,
             codaOnly = c("mu")
)


post_red
#saveRDS(post_red, ".\\output\\post_2003on")
post_red <- readRDS(".\\output\\post_2003on")
#   * Plots ---------------------------------------------------------------

traceplot(post_red, c("S.msy", "S.eq", "lnalpha", "lnalpha.c", "beta", "sigma.white", "phi", "D"))
post_red$sims.list[c("S.msy", "S.eq", "lnalpha", "lnalpha.c", "beta", "sigma.white", "phi")] %>% 
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(stat ~ ., scales = "free_x")


plot_horse_red(post_red)
table_params_red(post_red)
profile_red <- get_profile(post_red)
plot_profile(profile_red, goal_range = c(3800, 7600))




# Full Data analysis ------------------------------------------------------

upyear_full=22
mod_full=c("FULL_1-39","OUT_YEARS_1-39");YRIND=1:46
projname_full=paste("ANC_CHIN_BASSR_JAGS_UPDATE_",upyear_full,mod_full[1],sep="") # Core name of BUGS model.


#   * Data ----------------------------------------------------------------

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXX  DATA  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  if(T){dat_full=list(
    T=46,
    Air.Stop=32,
    A=4,
    a.max=6,
    a.min=3,
    x = matrix(c(
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      0,0,0,0,
      20,89,226,53,
      22,52,121,55,
      20,96,209,76,
      11,29,91,44,
      1,40,96,42,
      5,26,81,6,
      10,42,32,3,
      17,88,126,14,
      9,140,115,16,
      17,54,79,7,
      29,45,62,7,
      35,97,108,27,
      51,149,168,9,
      15,100,134,14,                      # 2016 age comp
      23, 44,107,12,                      # 2017 age comp
      8,109,107,15,                      # 2018 age comp
      10, 41,138,14,                      # 2019 age comp
      17, 48, 66,18,                      # 2020 age comp
      7, 19, 98,10,                      # 2021 age comp
      7, 51, 55, 4                        # 2022 age comp
    ),46, 4,byrow=TRUE),                 # 12/21/15   follows CHANGE 9/2/2014 to Buskin coho analysis
    
    n.tot= c(0,   0 ,  0,   0 ,  0,   0 ,  0,   0 ,  0,   0 ,  0  , 0 ,  0,   0 ,  0  , 0 ,  0,   0 ,  0  , 0 ,  0,   0 ,  0  , 0 ,  0,   0 ,
             388 ,250, 401, 175, 179, 118,  87 ,245, 280 ,157, 143 ,267, 377, 263, 186,239,203,149,134,117),
    
    Air.Survey=c(3585,2209,1335,NA,1066,1493,1033,1087,1328,2287,2524,1458,940,967,589,99,1110,837, NA,277,477,
                 789,685,752,414,748,647,834,651,899, 678, 528),  	# DGE SF counts from 1977; Set NA after 2008 (surveys discontinued)
    
    S.hat=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
            NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,13273,12016,11156,8945,9622, 5806, 3455,4449,3545,4509,4378,2497,10049,7149, 5796, 3162, 5603,3624,4300,3123), 	    # DGE Last NA for 2015 pred
    
    S.cv=c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
           0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.015,0.024,0.027,0.032,0.025,0.029,0.02,0.023,0.02,
           0.044,0.03,0.02,0.05,0.02,0.02, 0.02, 0.02,0.02,0.02,0.02),  #SJF: USE 0.02 for 2009,2011 and 2014 when no sonar;  DGE SWAG of 0.05 used for Scv in 2015
    
    H.hat=c(1077,2109,1913,605,1069,718,1269,998,672,1098,761,976,578,1479,
            1047,1685,2787,2478,1475,1483,1563,783,1409,1730,889,1047,1011,1561,1432,1349,  2081, 1612, 737,364,573,38,97,203,344,1384,845,40,151,364,1.0E-3,1.0E-3), # SJF: USE NA for 2015
    
    H.cv=c(0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.21,0.22,0.20,0.14,
           0.14,0.15,0.12,0.14,0.13,0.14,0.12,0.15,0.14,0.11,0.18,0.18,0.16,0.13,0.16,0.14,0.16,0.15, 0.29,0.32,
           0.28,0.61,0.57,0.36,0.299,0.187,0.224, 1.0, 0.40,0.41,0.4,0.4), 
    E=c(8,12,12,12,12,12,12,12,12,12,12,14,15,15,
        15,15,15,15,15,15,15,15,15,15,15,12,12,15,15,15,15,20,8,12,15,9,12,12,13,20, 20,8,9,7,8,1.0E-3)    #  2/7/22: Holly double checked.
  )}  # dat0



#   * Model ---------------------------------------------------------------


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXX  MODEL DEFINITION --AND WRITE MODEL TO DIR XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

cat("
  model {

for (y in (A+a.min):(T+A-1)) {
    log.R[y] ~ dt(log.R.mean2[y],tau.white,500)
    R[y] <- exp(log.R[y])
    log.R.mean1[y] <- log(S[y-a.max]) + lnalpha - beta * S[y-a.max]
    log.resid[y] <- log(R[y]) - log.R.mean1[y]
}

log.resid.vec <- log.resid[(A+a.min):(T+A-1)]

log.R.mean2[A+a.min] <- log.R.mean1[A+a.min] + phi * log.resid.0

    for (y in (A+a.min+1):(T+A-1)) {
    log.R.mean2[y] <- log.R.mean1[y] + phi * log.resid[y-1]
    }

    lnalpha ~ dnorm(0,1.0E-6)I(0,)
    beta ~ dnorm(0,1.0E-1)I(0,)
    phi ~ dnorm(0,1.0E-4)I(-1,1)
    tau.white ~ dgamma(0.01,0.01)
    log.resid.0 ~ dnorm(0,tau.red)T(-3,3)
    alpha <- exp(lnalpha)
    tau.red <- tau.white * (1-phi*phi)
    sigma.white <- 1 / sqrt(tau.white)
    sigma.red <- 1 / sqrt(tau.red)
    lnalpha.c <- lnalpha + (sigma.white * sigma.white / 2 / (1-phi*phi) )
    S.max <- 1 / beta
    S.eq <- lnalpha.c * S.max
    S.msy <- S.eq * (0.5 - 0.07*lnalpha.c)
    U.msy <- lnalpha.c * (0.5 - 0.07*lnalpha.c)

    #  BROOD YEAR RETURNS W/O SR LINK DRAWN FROM COMMON LOGNORMAL DISTN
    #  Updated 12/23/15
    mean.log.R ~ dnorm(0,1.0E-4)I(0,)
    tau.R ~ dgamma(0.1,0.1)
    R.0 <- exp(mean.log.R)
    sigma.R0 <- 1 / sqrt(tau.R)
    for (y in 1:a.max) {
    log.R[y] ~ dt(mean.log.R,tau.R,500)
    R[y] <- exp(log.R[y])
    }

    #  DIRICHLET GENERATION OF RETURNS AT AGE
    #  GENERATE ALL T+A-1 = 42 MATURITY SCHEDULES, USE ONLY THOSE NECESSARY
    # DGE: See notes in BUGS and BRUGS notes for explanaiton of this section
    #  D.scale ~ dbeta(1,9)
    #D.sum ~ dlnorm(4,6)  # from meta-analysis of 7 estimates of Dsum    #mod
    #D.scale <- 1 / sqrt(D.sum)
    D.scale ~ dunif(0,1)
    D.sum <- 1 / (D.scale * D.scale)


    pi[1] ~ dbeta(1,1)
    pi.2p ~ dbeta(1,1)
    pi.3p ~ dbeta(1,1)
    pi[2] <- pi.2p * (1 - pi[1])
    pi[3] <- pi.3p * (1 - pi[1] - pi[2])
    pi[4] <- 1 - pi[1] - pi[2] - pi[3]
    for (a in 1:A) {
    gamma[a] <- D.sum * pi[a]
    for (y in 1:(T+A-1)) {
    g[y,a] ~ dgamma(gamma[a],1)
    p[y,a] <- g[y,a]/sum(g[y,])
    }
    }
    for (a in 2:A) {
    sibratio[a] <- pi[a] / pi[a-1]
    }

    # ASSIGN PRODUCT OF P AND R TO ALL CELLS IN N MATRIX
    # y SUBSCRIPT INDEXES BROOD YEAR
    # y=1 IS THE BROOD YEAR OF THE OLDEST FISH IN YEAR 1 (upper right cell)
    # y=42 IS THE BROOD YEAR OF THE YOUNGEST FISH IN YEAR T (=39) (lower left cell) 													#DGE
    # FIRST DO INITIAL CELLS WITHOUT SR LINK (x's IN MATRIX BELOW)
    # THEN DO CELLS DESCENDING WITH SR LINK (y's IN MATRIX BELOW)

    # T      S       1 2 3 4 (Ages)
    # 1977   s       x x x x
    # 1978   s       y x x x
    # 1979   s       y y x x
    # 1980   s       y y y x
    # 1981   s       y y y y
    # 1982   s       y y y y
    # 1983   s       y y y y
    #        s       .
    # 2015   s       y y y y


    # ASSIGN PRODUCT OF P AND R TO ALL CELLS IN N MATRIX (Matt's code)
        for (a in 1:A) {
        for (y in a:(T + (a - 1))) {
            N.ta[y - (a - 1), (A + 1 - a)] <- p[y, (A + 1 - a)] * R[y]
            }
        }

    # MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N
    # INDEX t IS CALENDAR YEAR
    for (t in 1:T) {
    N[t] <- sum(N.ta[t,1:A])
    for (a in 1:A) {
    q[t,a] <- N.ta[t,a] / N[t]
    }
    #n[t] <- sum(x[t,1:A])
    #x[t,1:A] ~ dmulti(q[t,],n[t])
    x[t,1:A] ~ dmulti(q[t,],n.tot[t])
       }

    # APPLY MARINE HARVEST (not measured: based on 1997 Deep Creek mu = 4%, harvest was higher 1995 and before; i.e. for 19 years from 1977 through 1995)
    # mean of beta(50,950)=0.05
    for (y in 1:19) {
    mu.marine[y] ~ dbeta(50,950)
    H.marine[y] <- mu.marine[y] * N[y]
    IR[y] <- max(N[y] - H.marine[y],1)
    }
    # mean of beta(40,960)=0.04
    for (y in 20:T) {                                                         																									#DGE
    mu.marine[y] ~ dbeta(40,960)
    H.marine[y] <- mu.marine[y] * N[y]
    IR[y] <- max(N[y] - H.marine[y],1)
    }

    # APPLY FRESHWATER HARVEST BELOW SONAR (=MU.T*N) TO GET SPAWNING ESCAPEMENT
    # DRAW EXPLOITATION RATES FROM COMMON BETA DISTN-hierachical
    B.scale ~ dunif(0,1)
    Q.H ~ dbeta(0.1,0.1)
    B.sum <- 1 / B.scale / B.scale
    B[1] <- Q.H * B.sum
    B[2] <- B.sum - B[1]
    for (y in 1:T) {
    q.H[y] ~ dbeta(B[1],B[2])
    F[y] <- q.H[y] * E[y]
    mu[y] <- 1 - exp(-F[y])
    H[y] <- mu[y] * IR[y]
    log.H[y] <- log(H[y])
    tau.log.H[y] <- 1 / H.cv[y] / H.cv[y]
    H.hat[y] ~ dlnorm(log.H[y],tau.log.H[y])
    S[y] <- max(IR[y] - H[y],1)
    }

    # OBSERVE AERIAL SURVEY COUNTS UNTIL 2008, WEIR/SONAR ESTIMATES DURING 2003-2015 (13 YEARS)
    log.q.AS1 ~ dnorm(0,1.0E-4)
    log.q.AS2 ~ dnorm(0,1.0E-4)
    tau.log.AS ~ dgamma(0.1,0.1)
    sigma.log.AS <- sqrt(1/tau.log.AS)
    q.ASearly <- exp(log.q.AS1) #renamed could not track when names q.A21 and q.AS2
    q.ASlate <- exp(log.q.AS2)
    q.AS1.inv <- 1 / q.ASearly
    q.AS2.inv <- 1 / q.ASlate

    for(y in 1:12) {
    Air.Survey[y] ~ dlnorm(log.qS1[y],tau.log.AS)
    log.qS1[y] <- log.q.AS1 + log.S[y]
    S.hat[y] ~ dlnorm(log.S[y],tau.log.S[y])
    log.S[y] <- log(S[y])
    tau.log.S[y] <- 1 / S.cv[y] / S.cv[y]
    }
   for(y in 13:Air.Stop) {                                                         																								#DGE
    Air.Survey[y] ~ dlnorm(log.qS2[y],tau.log.AS)
    log.qS2[y] <- log.q.AS2 + log.S[y]
    S.hat[y] ~ dlnorm(log.S[y],tau.log.S[y])
    log.S[y] <- log(S[y])
    tau.log.S[y] <- 1 / S.cv[y] / S.cv[y]
   }
   # WEIR DATA #I may have added this in 2022 (at least compared to when David di the multiple model test) which would be a significant error
   for(y in (Air.Stop + 1):T) { 
    S.hat[y] ~ dlnorm(log.S[y],tau.log.S[y])
    log.S[y] <- log(S[y])
    tau.log.S[y] <- 1 / S.cv[y] / S.cv[y]
  }
  }
", file=paste(projname_full,"_MODEL.jag",sep=""))





#   * Inits ---------------------------------------------------------------




# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXX  INITS - for nchains chains XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
if(T){
  
  if(T){ini0_full=list(
    #Air.Survey = ifelse(is.na(dat_full$Air.Survey), mean(dat_full$Air.Survey, na.rm = TRUE), NA),
    B.scale = 0.03,
    D.scale = 0.2,
    Q.H = 0.007218890595073482,
    beta = 1.124178669778089E-4,
    lnalpha = 1.338783290113077,
    log.q.AS1 = -1.785651448468361,
    log.q.AS2 = -2.87270171091565,
    log.resid.0 = 0.0437292250913513,
    mean.log.R = 9.847744027114981,
    phi = 0.4085737310890481,
    pi = c(0.08186550896067785,            NA,            NA,            NA),
    pi.2p = 0.2277409016251648,
    pi.3p = 0.7529542645626424,
    tau.R = 12.41755934253248,
    tau.log.AS = 3.754135323267579,
    tau.white = 18.54684104153244,
    
    S.hat = c(
      18238.44845704645,11546.20087017747,13313.70879511766,16536.19323183335,13268.33356923782,
      6729.188778678483,10138.74560308389,9432.738468613159,7485.493899702292,9286.446831566294,
      14856.75035027707,10934.68892709278,7197.249128506632,10258.31450918974,11835.998628875,
      14515.24663359293,13581.9639315281,10356.52440666883,14055.90390938503,11945.83815649061,
      12660.02841662113,9461.756301526106,11617.26508325859,15143.49711936063,6170.006290719739,
      9606.953621063036,  NA, NA,          NA,            NA,            NA,            NA, NA, NA, NA,NA,NA,NA,NA, NA, NA,NA,NA,NA,NA, NA),
    
    g = matrix(c(
      5.350456983793456,12.14042256741815,55.62284745018294,20.88324991374627,10.70187998562295,
      19.69350620942554,62.78842244996497,17.16977381921095,8.212982613863549,17.4154042833759,
      52.51893926800602,13.0275893517386,10.87968292959776,22.85910914464972,60.86053536685861,
      13.97706025147766,12.62039608616902,20.13980254120176,47.25614711849315,13.99850105047011,
      7.266846845240096,24.5795681197197,75.44676094487109,17.66530040983674,8.492182644531933,
      24.80639022792742,53.79842320636524,20.76808406535992,8.998765541689092,28.464663789383,
      52.81247982330103,17.66785052434414,4.378350731146027,21.66614027644304,58.61432236453248,
      17.33321056108252,11.45309471721048,26.2071970005356,58.08320827580205,15.80701749250942,
      7.219788685033764,15.32813295664652,51.11962651471244,14.60875562414948,9.183553002510909,
      26.73543368011418,46.66822203446996,19.83523930287884,7.480758058640787,22.3991914570843,
      44.80175953523263,19.30181407387566,5.271017668762489,16.22932248973823,63.08113918222397,
      20.09708117381976,11.0229101179567,23.82287141843975,49.63874225575773,8.725004078512418,
      5.541949022264385,21.75993916469551,58.36227266272564,15.77447261040662,8.384284638844415,
      20.00984447407456,57.82108211220505,18.09376665362165,5.596559454912147,19.40793802915,
      61.83228571579335,14.1724222739327,12.34072223028771,23.05241300235147,60.83332765970384,
      20.9146243274962,6.208564892984356,21.36240412705023,60.29985981463911,18.83735905030994,
      13.68857530105607,22.21280797220706,49.14718846900355,24.19732831685385,6.79888367512384,
      23.74572293565995,50.27871962584464,16.65552909648122,8.748145980140844,25.95722026445459,
      48.43931164406553,17.54377444796021,9.035218928398409,30.43862692858041,54.37145378539842,
      15.75462235151416,8.726966315985521,16.79914897437568,70.21744914605254,16.63339034588937,
      6.901606681634303,17.76073906098992,59.48080030963,14.51576391406237,6.773892321330562,
      18.82976110151854,45.39295512386794,18.01617603107455,4.386155507881982,25.66451413408785,
      59.6279905801976,20.05353186893558,14.62488532528542,20.19955915036056,44.57930568744207,
      17.86885389040289,5.496995449326843,22.92427853730302,53.71639775466916,17.71922328034215,
      11.72730654046007,21.93797331960099,53.43023379907113,23.35117950194843,7.017320095709441,
      13.91977168874004,47.90817343568695,16.30385499540698,7.574201757287311,23.80692979445955,
      49.89210701317273,26.98595884492361,
      8,24,50,17,
      8,24,50,18,
      6,24,50,19,
      8,24,55,15,
      7,24,53,17,
      8,24,51,14,
      8,24,55,12,
      8,30,45,17,
      9,32,44,13,
      8,24,55,15,
      7,24,53,17,
      8,24,51,14,
      8,24,55,12,
      8,30,45,17,
      9,32,44,13,
      9,32,44,13),
      49,4,byrow=TRUE),
    
    log.R = c(
      8,9,            9.1,            8.2,            8.5,
      8.1,8.751855086358638,9.197576585591872,9.175605587342712,9.18615804297054,
      9.272992677723535,9.653296928071237,9.414705230777765,9.037436498534289,9.570211098803757,
      9.440822216665074,9.634755253981242,9.708406979018052,9.815318747786613,9.506783680274577,
      9.400259447440821,9.244659492025479,9.067912485091426,9.468409119309003,9.294473447098209,
      9.411703795959893,9.20491261386243,9.651355264779388,9.551506894980447,9.372870213496611,
      9.53743123671844,9.243342978868499,9.012439141315847,8.9,9,9,9,9,9,9,9,9.5,9,9,9,9,9,9.5,9.5),
    
    q.H = c(
      0.007186511318970837,0.01125230325064814,0.009618202653655157,0.004237141075104568,0.009489240954800743,
      0.005651415421291692,0.01086176799161786,0.007913601076878949,0.005230254246562143,0.009189056426833219,
      0.005296051767506838,0.007003860693734387,0.0062179133442023,0.008242151138798729,0.008851120601600829,
      0.005837976263536464,0.01245373155245261,0.01209037761658881,0.007242509335759774,0.008831003077031128,
      0.009057574780301298,0.005276765182933607,0.008389883968200647,0.0108833003985714,0.005372243495597091,
      0.00613421257471205,0.004410316813741342,0.008664793191583147,0.007114618858816998,0.00931542,0.01,0.01,
      0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),
    
    mu.marine = c(
      0.04325263444661011,0.0409901645536648,0.04527632920542994,0.06028366111489206,0.04328725535589889,
      0.04045632398900765,0.05347097021719297,0.05233594891864612,0.05065178160394555,0.05774773898638204,
      0.0371248400847605,0.05467251075561203,0.04598240438848132,0.04730150559283569,0.05866483407757642,
      0.04563850925389768,0.0508023236308326,0.05292432034790853,0.05557981749629747,0.03560404434320997,
      0.03681220599310785,0.04033851984225476,0.02907586007189429,0.04112299881212985,0.03936780017903141,
      0.03615519331106785,0.04413502169966968,0.03246756822439872,0.03564969957682632,0.03413159871138456,
      0.04520616106459093,0.03291706628445844,0.04162286487006402,     0.05, 0.05, 0.05,0.05,0.047,0.047, 
      0.05, 0.05, 0.05,0.05,0.047,0.047, 0.047)
  )} # ini0-baseline inits file
  

ini1_full=ini0_full
poslinenode=c("B.scale","D.scale","Q.H","beta","lnalpha","log.q.AS1","log.q.AS2","log.resid.0","mean.log.R","tau.R", "tau.log.AS",
              "tau.white","S.hat","g","log.R","q.H","mu.marine")
propnode=c("pi","pi.2p","pi.3p")
corrnode=c("phi")
  
  if(nchains==1){ini_full=ini1_full}   # ini1mod is a list; stops here for nchains=1
  if(nchains %in% c(2, 3)){
    ini2_full=c(
      lapply(ini1_full[poslinenode],function(y){z=y+runif(1,-0.1,0.1)*y}),
      lapply(ini1_full[propnode],function(y){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=0]=0.001;return(z)}),
      lapply(ini1_full[corrnode],function(y,corrnode){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=-0.999]=-0.99; return(z)}))
    ini_full=list(ini1_full,ini2_full)
  }
  if(nchains==3){
    ini3_full=c(
      lapply(ini1_full[poslinenode],function(y){z=y+runif(1,-0.1,0.1)*y}),
      lapply(ini1_full[propnode],function(y){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=0]=0.001;return(z)}),
      lapply(ini1_full[corrnode],function(y,corrnode){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=-0.999]=-0.99; return(z)}))
    ini_full=list(ini1_full,ini2_full,ini3_full)}
  
}  # INITS: nchains chains

codavec_full=c("S.msy",
                "S.eq",
                "S.max",
                "lnalpha",
                "lnalpha.c",
                "beta",
                "alpha",
                "phi",
                "sigma.white",
                "sigma.red",
                "R.msy",
                "MSY",
                "U.msy",
                "pi",
                "S",
                "R",
                "log.resid",
                "log.resid.0",
                "log.resid.vec",
                "D.sum",
                "mu",
                "N",
                "N.ta",
                "IR",
                "p",
                "q",
                "q.ASearly",
                "q.ASlate",
                "Q.H",
                "H.marine")


#   * Run jags ------------------------------------------------------------


post_full <- jags(data = dat_full,
             parameters.to.save = codavec_full,
             inits = ini_full,
             model.file = paste(projname_full,"_MODEL.jag",sep=""),
             n.chains = nchains,
             n.iter = ns,
             n.burnin = nb,
             n.thin = nt,
             parallel = TRUE,
             store.data = TRUE,
             codaOnly = c("mu")
)

#saveRDS(post_full, ".\\output\\post_1977on")
post_full <- readRDS(".\\output\\post_1977on")

traceplot(post_full, c("S.msy", "S.eq", "lnalpha", "lnalpha.c", "beta", "sigma.white", "phi", "D"))
post_full$sims.list[c("S.msy", "S.eq", "lnalpha", "lnalpha.c", "beta", "sigma.white", "phi", "q.ASearly", "q.ASlate")] %>% 
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(stat ~ ., scales = "free_x")

#   *  Plots --------------------------------------------------------------


plot_state_full(post_full)
plot_horse_full(post_full)
table_params_full(post_full)
profile_full <- get_profile(post_full)
library(xlsx)
write.xlsx(profile_full, ".\\output\\profile_2022.xlsx")
plot_profile(profile_full, goal_range = c(3800, 7600))


