# Anchora River Chinook quality of escapement 

# Author: Adam Reimer
# Version: 2025-10-08

# Packages
packs <- c("tidyverse")
lapply(packs, require, character.only = TRUE)

# Used to investigate quality of escapement issues prior to the LCI BOF cycle. Area staff were 
# looking for a large fish goal. Used this script to look at the data and  
# think about whether it necessitated immediate action (Goal was revised last cycle).  
# Mike and I agreed (over email) that it could wait partially bc Anchor might not be 
# the best test case and partially bc we wanted to think about quality goals more generally 
# with possible recommendations next cycle on several stocks. I promised to work on that.
# Specifically I want to think about egg based goals as opposed to size based goals as a 
# way to avoid truncating the fish included in the escapement when size-at-age and 
# age-at-maturity seems are in decline.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Read/clean ASL data --------
# als dataset for Kenai Chinook (for tl ~ mef conversion)
load("S:\\RTS\\Reimer\\KenaiSRA\\data\\dat_chinmef.rda")
table(dat_chinmef$tl, dat_chinmef$sample)
dat_chinmef <- 
  dat_chinmef %>%
  mutate(tl_clean = ifelse(tl > 100, tl /10 / 2.54, tl))
table(dat_chinmef$tl_clean, dat_chinmef$sample)
convert <- lm(dat_chinmef$tl_clean ~ dat_chinmef$mef)

#asl data
asl <- 
  readxl::read_excel(".\\data\\data_raw.xlsx", sheet = "AnchorChinook_ASL") %>%
  mutate(sex = ifelse(sex == "U", NA, sex),
         tl = convert$coefficients[1] + convert$coefficients[2] * mef)

# look at missing data
table(asl$year, useNA = "ifany")

table(asl$sex, useNA = "ifany")
asl[is.na(asl$sex), ] %>% print(n = 100)

table(asl$mef, useNA = "ifany")
asl[is.na(asl$mef), ] %>% print(n = 100)

table(asl$age_ocean, useNA = "ifany")
asl[is.na(asl$age_ocean) & is.na(asl$sex), ] %>% print(n = 60)
asl[is.na(asl$age_ocean) & is.na(asl$mef), ]
asl[is.na(asl$age_ocean) & is.na(asl$sex) & is.na(asl$mef), ]

sum(complete.cases(asl))
nrow(asl)

# plot lg@age vrs. cutoffs ---------
#Notice some large outliers
asl %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x = tl, fill = sex)) +
  geom_vline(xintercept = 20) +
  geom_vline(xintercept = 28, linetype = 2) +
  geom_histogram(position = "dodge") +
  facet_grid(age_ocean ~ .)

#look at outliers
asl[!is.na(asl$tl) & asl$tl > 20 & !is.na(asl$age_ocean) & asl$age_ocean %in% 1, ] %>% print(n = 100)
asl[!is.na(asl$tl) & asl$tl < 20 & !is.na(asl$age_ocean) & asl$age_ocean %in% 2, ] %>% print(n = 100)
asl[!is.na(asl$tl) & asl$tl > 35 & !is.na(asl$age_ocean) & asl$age_ocean %in% 2, ] %>% print(n = 100)
asl[!is.na(asl$tl) & asl$tl < 28 & !is.na(asl$age_ocean) & asl$age_ocean %in% 4, ] %>% print(n = 100)

# plot age comps ------
# Jacks stable
# 2 oceans increase
# 3 oceans stable
# 4 oceans decrease
asl %>%
  filter(!is.na(age_ocean)) %>%
  group_by(year, age_ocean) %>%
  summarize(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = year, y = pct, color = as.character(age_ocean))) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE) +
  #facet_grid(paste0("Ocean Age:", age_ocean) ~ .) +
  scale_y_continuous(lim = c(0, 1)) +
  #theme(legend.position = "none") +
  labs(x = "Year", y = "Proportion of Escapement", color = "Ocean Age")

# plot sex comps -------
# slightly decreasing females
asl %>%
  filter(!is.na(sex)) %>%
  group_by(year, sex) %>%
  summarize(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = year, y = pct, color = sex)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE) +
  #facet_grid(paste0("Sex:", sex) ~ .) +
  scale_y_continuous(lim = c(0, 1)) +
  #theme(legend.position = "none") +
  labs(x = "Year", y = "Proportion of Escapement", color = "Sex")

# plot age / sex comps ------
# % of males increasing in the 2 and 4 ocean age classes
asl %>%
  filter(!is.na(sex), !is.na(age_ocean)) %>%
  group_by(year, age_ocean, sex) %>%
  summarize(n = n()) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = year, y = pct, color = sex)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE) +
  facet_grid(paste0("Ocean Age:", age_ocean) ~ .) +
  scale_y_continuous(lim = c(0, 1)) +
  #theme(legend.position = "none") +
  labs(x = "Year", y = "Proportion of Escapement", color = "Sex")

# plot fecundity-------
# prep data
age_comp <- 
  asl %>%
  filter(!is.na(age_ocean), !is.na(sex)) %>%
  group_by(year, age_ocean, sex) %>%
  summarize(n = n()) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n)) %>%
  select(year, age_ocean, sex, pct) %>%
  pivot_wider(id_cols = year, 
              names_from = c(age_ocean, sex),
              names_prefix = "age",
              values_from = pct,
              values_fill = 0)

mef_params <-
  asl %>%
  filter(complete.cases(.)) %>%
  group_by(year, age_ocean, sex) %>%
  summarize(mef_mean = mean(mef),
            mef_sd = ifelse(!is.na(sd(mef)), sd(mef), 0.2 * mef_mean))
  

# Escapement----------
S0 <- 
  readxl::read_excel(".\\data\\data_raw.xlsx", sheet = "S") %>%
  select(-notes) %>%
  filter(year >= 2003) %>%
  uncount(S) %>%
  left_join(age_comp, by = "year") %>%
  rowwise() %>%
  mutate(group = LaplacesDemon::rcat(1, p = c(age1_M, age2_F, age2_M, age3_F, age3_M, age4_F, age4_M)),
         age_ocean = ifelse(group == 1, 1,
                            ifelse(group %in% 2:3, 2,
                                   ifelse(group %in% 4:5, 3, 4))),
         sex = ifelse(group %in% c(1, 3, 5, 7), "M", "F")) %>%
  left_join(mef_params, by = c("year", "age_ocean", "sex")) %>%
  mutate(mef = rnorm(1, mean = mef_mean, sd = mef_sd),
         eggs = ifelse(sex == "F", 0.1633613 * mef^1.624, 0),
         tl = convert$coefficients[1] + convert$coefficients[2] * mef) #Kusko data from Boersma range 604-945

# * all sizes -------
S <- 
  S0 %>%
  group_by(year, age_ocean, sex) %>%
  summarize(S_salmon = n(),
            S_eggs = sum(eggs))

# ** Egg/Salmon Escapement -------
S %>%
  group_by(year, sex, age_ocean) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) %>%
  pivot_longer(starts_with("S_"), names_to = "S", names_prefix = "S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = sex)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE) +
  facet_grid(S ~ age_ocean, scales = "free_y") +
  labs(x = "Year", y = "Escaped Eggs", color = "Method", title = "all sizes")

# ** Relative Escapement ---------
Annual_S <-
  S %>%
  group_by(year) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) 

Annual_S %>%
  mutate(prop_S_salmon = S_salmon / Annual_S$S_salmon[Annual_S$year == 2003],
         prop_S_eggs = S_eggs / Annual_S$S_eggs[Annual_S$year == 2003]) %>%
  pivot_longer(starts_with("prop_S_"), names_to = "S", names_prefix = "prop_S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = S)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Escapement / 2003 Escapement", title = "all sizes")

#Relative Escapement by age
Annual_S_age <-
  S %>%
  group_by(year, age_ocean) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) 

Annual_S_age %>%
  left_join(Annual_S_age[Annual_S_age$year == 2003, c("age_ocean", "S_salmon", "S_eggs")], by = c("age_ocean")) %>%
  mutate(prop_S_salmon = S_salmon.x / S_salmon.y,
         prop_S_eggs = S_eggs.x / S_eggs.y) %>%
  pivot_longer(starts_with("prop_S_"), names_to = "S", names_prefix = "prop_S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = S)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(age_ocean ~ ., scale = "free_y") +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Escapement / 2003 Escapement", title = "all sizes")


# * > 28 inches ---------
S_28 <- 
  S0 %>%
  group_by(year, age_ocean, sex) %>%
  filter(tl >= 28) %>%
  summarize(S_salmon = n(),
            S_eggs = sum(eggs))

# ** Egg/Salmon Escapement -------
S_28 %>%
  group_by(year, sex, age_ocean) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) %>%
  pivot_longer(starts_with("S_"), names_to = "S", names_prefix = "S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = sex)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE) +
  facet_grid(S ~ age_ocean, scales = "free_y") +
  labs(x = "Year", y = "Escaped Eggs", color = "Method", title = "> 28 in. tl")

# ** Relative Escapement ---------
Annual_S28 <-
  S_28 %>%
  group_by(year) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) 

Annual_S28 %>%
  mutate(prop_S_salmon = S_salmon / Annual_S28$S_salmon[Annual_S$year == 2003],
         prop_S_eggs = S_eggs / Annual_S28$S_eggs[Annual_S$year == 2003]) %>%
  pivot_longer(starts_with("prop_S_"), names_to = "S", names_prefix = "prop_S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = S)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Escapement / 2003 Escapement", title = "ge 28 in. tl")

#Relative Escapement by age
Annual_S28_age <-
  S_28 %>%
  group_by(year, age_ocean) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) 

Annual_S28_age %>%
  left_join(Annual_S28_age[Annual_S28_age$year == 2003, c("age_ocean", "S_salmon", "S_eggs")], by = c("age_ocean")) %>%
  mutate(prop_S_salmon = S_salmon.x / S_salmon.y,
         prop_S_eggs = S_eggs.x / S_eggs.y) %>%
  pivot_longer(starts_with("prop_S_"), names_to = "S", names_prefix = "prop_S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = S)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(age_ocean ~ ., scale = "free_y") +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Escapement / 2003 Escapement", title = "ge 28 in. tl")


# * > 20 inches ---------
S_20 <- 
  S0 %>%
  group_by(year, age_ocean, sex) %>%
  filter(tl >= 20) %>%
  summarize(S_salmon = n(),
            S_eggs = sum(eggs))

# ** Egg/Salmon Escapement -------
S_20 %>%
  group_by(year, sex, age_ocean) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) %>%
  pivot_longer(starts_with("S_"), names_to = "S", names_prefix = "S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = sex)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE) +
  facet_grid(S ~ age_ocean, scales = "free_y") +
  labs(x = "Year", y = "Escaped Eggs", color = "Method", title = "> 20 in. tl")

# ** Relative Escapement ---------
Annual_S20 <-
  S_20 %>%
  group_by(year) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) 

Annual_S20 %>%
  mutate(prop_S_salmon = S_salmon / Annual_S20$S_salmon[Annual_S$year == 2003],
         prop_S_eggs = S_eggs / Annual_S20$S_eggs[Annual_S$year == 2003]) %>%
  pivot_longer(starts_with("prop_S_"), names_to = "S", names_prefix = "prop_S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = S)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Escapement / 2003 Escapement", title = "ge 20 in. tl")

#Relative Escapement by age
Annual_S20_age <-
  S_20 %>%
  group_by(year, age_ocean) %>%
  summarize(S_salmon = sum(S_salmon),
            S_eggs = sum(S_eggs)) 

Annual_S20_age %>%
  left_join(Annual_S20_age[Annual_S20_age$year == 2003, c("age_ocean", "S_salmon", "S_eggs")], by = c("age_ocean")) %>%
  mutate(prop_S_salmon = S_salmon.x / S_salmon.y,
         prop_S_eggs = S_eggs.x / S_eggs.y) %>%
  pivot_longer(starts_with("prop_S_"), names_to = "S", names_prefix = "prop_S_", values_to = "stat") %>%
  ggplot(aes(x = year, y = stat, color = S)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(age_ocean ~ ., scale = "free_y") +
  geom_hline(yintercept = 1) +
  labs(x = "Year", y = "Escapement / 2003 Escapement", title = "ge 20 in. tl")

