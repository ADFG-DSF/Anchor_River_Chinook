# Update to Optimal Yeild Profile plot as requested 9/21/2023 - 
# Removed other panels, updated Esc goal range to 3200-6400 , remove 70 & 80% lines


#### Import data and Function ####
packs <- c("jagsUI", "tidyverse")
lapply(packs, require, character.only = TRUE)

#source functions
function_files <- list.files(path=".\\functions")
lapply(function_files, function(x) source(paste0(".\\functions\\", x)))

#load datasets
data_names <- list.files(path=".\\data")
lapply(data_names, function(x) load(paste0(".\\data\\", x), .GlobalEnv))

post <- readRDS(".\\output\\post_1977on") #OUTPUT is currently located in S:\RTS\Reimer\Anchor_River_Chinook

####



#### Create Plot ####

profile_full <- get_profile(post)
OYP_update = plot_profile(profile_full,rug=FALSE ,goal_range = c(3200, 6400),profiles = c("OYP"),percent = c("90"))+
  scale_x_continuous("Spawners", breaks = seq(0, 9000,800), labels = scales::comma)+theme(legend.position = "none")+
  labs(caption = "
  Probability of achieving sustained yield within 90 percent of maximum sustained yield for Anchor River Chinook salmon.
  The shaded area represents to escapement goal recommendation of 3,200-6,400 Chinook salmon.
       ")+
  theme(plot.caption = element_text(hjust=0))
OYP_update

####

#ggsave("OYP_update.png",height = 3,width = 9,units = "in")

