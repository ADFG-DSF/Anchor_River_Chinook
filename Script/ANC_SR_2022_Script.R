#12/8/2022 - Logan Wendling - Copied most of the previous 2019 code and updated best I could
#2/3/2023 - Adam Reimer. Updated some of the priors to aid model fit.
# CHOSEN MODEL =RED_21-43AR. SEE "Schem21-43R" sheet in ANC_CHIN_BASSR_JAGS_RED_21-43AR_RESULTS.xlsx workbook for schematics
# RED_21-43R = YEARS 27-43 with first 6 aerial surveys prior to 2003 used for the R[y] w/out direct spawning abund link PLUS AR(1) component in first 6 R[y]
# Uses data up to and including 2022.
# Previous models deleted from this code; See Previous versions of R code (ANC_BASSR_JAGS_MOD_2_4_16.r for other models entertained).
# N.B. HUNT FOR "SPECIFIC" in the code when updating

#read in dat_chinBEG for profile comparisons
data_names <- list.files(path=".\\data")
lapply(data_names, function(x) load(paste0(".\\data\\", x), .GlobalEnv))

#source functions
function_files <- list.files(path=".\\functions\\")
lapply(function_files, function(x) source(paste0(".\\functions\\", x)))

library(tidyverse)
library(jagsUI)
upyear=22      # SPECIFIC
mod=c("_RED_21-46AR","OUT_YEARS_21-46AR")   # Chosen model; SPECIFIC model name (43)
projname=paste("ANC_CHIN_BASSR_JAGS_UPDATE_",upyear,mod[1],sep="") # Core name of BUGS model text and Excel results files.



# Data --------------------------------------------------------------------


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXX  DATA  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
if(T){
  # N.B. need byrow=TRUE for matrices
  # dat0-data that is relevant to the  RED_21-43AR model.  See previous versions (ANC_BASSR_JAGS_MOD_2_4_16.r) for data ever used in any of the models considered during model selection
  datanc=list(
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
    E=c(12,15,15,15,15,20,12,12,15,9,12,12,13,20, 20,8,9,11,1.0E-3,1.0E-3 )     #  1/25/19:Updated effort for 2015-2016. 20 days fished in 2017; 8 days fished in 2018, 9 in 2019.  Carol wants two analyses:12 and 20 for 2019 forecast (different code); see email on 1/25/2019
    # S.ninc=S.ninc0,                                           # Used in plotting statements in the MODEL section
    # S.byinc=S.byinc0
  )
  datanc$n.tot=rowSums(datanc$x) # Calculates sample size
} # DATA  ; SPECIFIC: numbers


# Model -------------------------------------------------------------------
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
      lnalpha ~ dnorm(0,1.0E-6)I(0,) #Truncation prevents negative log alpha. alpha <1 is unlikely in a sustainable population. COm fish used the same trucation. 
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

      # # GENERATE FITTED VALUES OF R EVERY S.byinc SPAWNING FISH FOR GRAPHICS;
      # for (i in 1:S.ninc) {
      # S.star.1[i] <- S.byinc*i
      # R.fit[i] <- S.star.1[i] * exp(lnalpha - beta * S.star.1[i])
      # }
      # 
      # # CALCULATE SUSTAINED YIELD AT REGULAR INTERVALS OF S;
      # # FIND THE PROBABILITY THAT EACH VALUE OF S WILL RESULT IN YIELDS WITHIN 10, 20, & 30 OF MSY;
      # # FIND THE PROBABILITY OF NOT CONDUCTING RECRUITMENT OVERFISHING AT EACH S;
      # R.msy <- S.msy * exp(lnalpha - beta * S.msy)*exp(sigma.red*sigma.red/2)
      # MSY <- R.msy - S.msy
      # for (i in 1:S.ninc)  {
      # S.star.2[i] <- S.byinc*i
      # R.fit2[i] <- S.star.2[i] * exp(lnalpha - beta * S.star.2[i])*exp(sigma.red*sigma.red/2)
      # SY[i] <- R.fit2[i] - S.star.2[i]
      # I90[i] <- step(SY[i] - 0.9 * MSY)
      # I80[i] <- step(SY[i] - 0.8 * MSY)
      # I70[i] <- step(SY[i] - 0.7 * MSY)
      # #OF90[i] <- 1 - nOF90[i]
      # #OF80[i] <- 1 - nOF80[i]
      # #OF70[i] <- 1 - nOF70[i]
      # }
      # #nOF90[1] <- 0
      # #nOF80[1] <- 0
      # #nOF70[1] <- 0
      # #for (i in 2:120) {
      # #nOF90[i] <- max(I90[i],nOF90[i-1])
      # #nOF80[i] <- max(I80[i],nOF80[i-1])
      # #nOF70[i] <- max(I70[i],nOF70[i-1])
      #}
      }
      ", file=paste(projname,"_MODEL.jag",sep=""))
  
} # MODEL SPECIFICATION : .jag; SPECIFIC updates in annotations here


# Inits -------------------------------------------------------------------


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXX  INITS - for nchains chains XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
nchains <- 2
nb <- 50000
nt <- 200
ns <- 200000
   
if(T){
  ini1=list(
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
  
  if(nchains==1){inianc=ini1}   # ini1anc is a list; stops here for nchains=1
  if(nchains==2){
    ini2=c(                     # Add some noise to original inits
      lapply(ini1[poslinenode],function(y){z=y+runif(1,-0.1,0.1)*y;return(z)}),
      lapply(ini1[propnode],function(y){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=0]=0.001;return(z)}),
      lapply(ini1[corrnode],function(y,corrnode){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=-0.999]=-0.99; return(z)}))
    inianc=list(ini1,ini2)
  }
  if(nchains==3){
    ini3=c(                   # Add some noise to original inits
      lapply(ini1[poslinenode],function(y){z=y+runif(1,-0.1,0.1)*y;return(z)}),
      lapply(ini1[propnode],function(y){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=0]=0.001;return(z)}),
      lapply(ini1[corrnode],function(y,corrnode){z=y+runif(1,-0.1,0.1)*y;z[z>=0.999]=0.99;z[z<=-0.999]=-0.99; return(z)}))
    inianc=list(ini1,ini2,ini3)}
  
}  # INITS: nchains chains ; SPECIFIC:numbers


# Run Jags ----------------------------------------------------------------
codavec=c("S.msy",
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

post <- jags(data = datanc,
             parameters.to.save = codavec,
             inits = inianc,
             model.file = paste(projname,"_MODEL.jag",sep=""),
             n.chains = nchains,
             n.iter = ns,
             n.burnin = nb,
             n.thin = nt,
             parallel = TRUE,
             store.data = TRUE,
             codaOnly = c("mu")
)


post
#saveRDS(post, ".\\output\\post_2003on")
#   # Plots -------------------------------------------------------------------
traceplot(post, c("S.msy", "S.eq", "lnalpha", "lnalpha.c", "beta", "sigma.white", "phi", "D"))
post$sims.list[c("S.msy", "S.eq", "lnalpha", "lnalpha.c", "beta", "sigma.white", "phi")] %>% 
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(stat ~ ., scales = "free_x")


plot_horse(post)
table_params(post)
profile <- get_profile(post)
plot_profile(profile, goal_range = c(3800, 7600))
