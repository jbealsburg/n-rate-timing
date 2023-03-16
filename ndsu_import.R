
# Nrate timing data from north dakota
# see https://docs.google.com/spreadsheets/d/1zI_Oh03m_a1ZtnyZgPBl_NHkdEgAA7YO7fivp1gUOZI/edit#gid=1591093391

# Data was collected from two 0.5m2 quadrats and combined

# N was broadcast on 11May2021 on dry soil and about 0.5" of rain fell 2 weeks
# later, but then it was very dry. Likely a lot of N was volatilized

# Unsure about when N was applied in 2020

library(tidyverse)


# Manually entering the treatment key to convert treatment code to the N rate applied
treatment <- seq(1,4,1)
lbsN <- c(
  0,25,50,75
)

# assuming it's lbs N per acre

lbsN*1.121 %>% 
  round(1) -> kgNha

tibble(
  treatment,
  lbsN,
  kgNha
) -> key1

# importing ndsu data
# adding in N rate
read.csv("ndsu_data.csv",
         skip = 1) %>% 
  filter(
    Experiment=="Nitrogen Rate"
  ) %>% 
  rename_all(tolower) %>% 
  dplyr::select(
    year,experiment,location,block,plot,treatment,
    threshed_grain_no_bag.g.
  ) %>% 
  left_join(key1) %>% 
  dplyr::select(-c(lbsN,
                   treatment)) %>% 
  rename(
    threshed_grain_gramsm2 = threshed_grain_no_bag.g.
  ) %>% 
  mutate(yield_kgha = threshed_grain_gramsm2*10) %>% 
  dplyr::select(-threshed_grain_gramsm2) %>% 
  mutate(year=as.factor(year))-> dat


# EDA ---------------------------------------------------------------------


dat %>% 
  # glimpse()
  ggplot(aes(kgNha,yield_kgha,
             group = year,
             col=year)) +
  stat_summary() +
  geom_smooth(
    data = subset(dat,year=="2020"),
    method = "lm",
    se=F
  ) +
  geom_smooth(
    data = subset(dat,year=="2021"),
    method = "lm",
    formula = y~poly(x,2),
    se=F
  ) 

dat %>% 
  lm(yield_kgha~kgNha*year*block,.) %>% 
  anova()

# We cannot reject the Ho that there were no differences in yield among N rates.

# We can reject Ho that yield did not differ between years
  