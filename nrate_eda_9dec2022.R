# Targeted hypothesis testing of the N-rate timing dataset
# Jesse
# 9Dec2022
# Due date: 12Dec2022


# Preliminary findings ----------------------------------------------------

# I read the report by Dominic and was taken aback by complexity and quantity of
# figures/tables. I think we need to simplify and find smaller datasets within
# the larger dataset where treatments are consistent and data isn't missing. I
# think this is a simple study that deserves a simple paper and then if there's
# time and space we can expand with supplementary material and the more complex
# aspects of the models dominic fitted and the limitations of the conclusions we
# can draw from them. 

# Since N rates were not consistent across sites, I may have luck rearrange data
# into the below format

# It seems dominic analyzed each site seperately, they have their own Rmarkdown
# files. Dataframe are not consistent

# dataframe we may need(
#   site,
#   year,
#   standage,
#   N_summer,
#   N_spring,
#   N_fall, 
#   N_prior_to_harvest = (spring+summer of year + fall of previous year),
#   split = yes or no,
#   yield
# ) 

# does rate of N matter?
lm = yield~N_prior_to_harvest*standage

lm = yield ~ N_spring*N_summer*N_fall*standage



# import data -------------------------------------------------------------
library(tidyverse)

read.csv("data_yield.csv") -> dat

dat %>% 
  # colnames()
  rename_all(tolower) %>% 
  # glimpse()
  dplyr::select(
    c(year,location,id,trt_code,treatment,threshed_grain_yield.kg.ha. )
  ) %>% 
  rename(yield = threshed_grain_yield.kg.ha. ) %>% 
  mutate(treatment = tolower(treatment)) %>% 
  filter(treatment != "")-> dat2 #dropping na for 2020 rsmt v17 plots that don't exist

dat2 %>% 
  group_by(treatment) %>% 
  tally() %>% 
  print(n=30)
# may need a treatment dataframe we join to the site, plot info due to errors
# uncovered so far with data. This may have been addressed in markdown however

# continue with rough exploratory data analysis

dat2 %>% 
  # colnames()
  # glimpse() %>% 
  str_split(dat2$treatment)

dat2 %>% 
  filter(location != "RSMT V17") -> dat_site1

# sites recieved summer applicaiton of 4 possible rates
# then rate of 60 in either spring or fall

dat_site1 %>% 
  distinct(treatment)
# ok so treatment is the total N
# where 60 is applied in spring or fall

str_split(dat_site1$treatment,
          pattern = " ",
          simplify = T) %>% 
  as_tibble() %>% 
  rename(rate_total = V1,
         timing = V2,
         method = V3) %>% 
  mutate(rate_total = if_else(rate_total=="control",
                        "0",
                        rate_total),
         timing = if_else(rate_total == "0",
                          "none",
                          timing)) %>% 
  mutate(
    rate_total = as.numeric(rate_total),
    rate_summer = rate_total - 60) %>% 
  View()
# I keep seeing things I don't expect
# need to review these methods more
# why do some say split and others do not?


str_split(dat2$treatment,
          pattern = " ",
          simplify = T) %>% 
  as_tibble() %>% 
  rename(rate = V1,
         timing = V2,
         method_fert = V3) %>% 
  # glimpse()
  mutate(rate = if_else(rate=="control",
                        "0",
                        rate),
         timing = if_else(rate == "0",
                          "none",
                          timing)) %>% 
  # distinct(timing)
  filter(timing=="split")
  # group_by(rate) %>% 
  # tally()

dat2 %>% 
  # filter(treatment == "80 split")
  filter(trt_code == "2")
#what was timing of rosemount V17?
# 80 split should have a timing of either fall or spring?

dat2 %>% 
  filter(location =="RSMT V17")

# Having trouble bringing datasets from each site together into a single
# dataframe. It appears the treatment code at one site is different than the
# treatment code at another site. The fertilizer treatments applied at the sites
# were different and it's confusing

# I am going to work through Dominic's markdown files to try to get clarity

# STOP at 3pm sunday

# START Just bringing in datasets from markdown files for hypothesis testing,
# each site remains separate

rsmtv17<- read.csv("Rosemount-V17.csv", header=T)
rsmtv17 %>% 
  rename_all(tolower) %>% 
  dplyr::select(year,experiment,location,id,treatment,
                napplied,ntiming,stand.age,block,
                yield.kgperha
  ) %>% 
  mutate_all(factor) %>% 
  mutate(yield.kgperha = as.numeric(yield.kgperha)) -> dat_v17

r100<- read.csv("Rosemount_R100.csv")

r100 %>% 
  rename_all(tolower) %>% 
  # colnames()
  dplyr::select(year,experiment,location,id,treatment,
                napplied,ntiming,stand.age,block,
                ntiming_newlabel,
                napplied_update,
                yield.kgperha
  ) %>% 
  mutate_all(factor) %>% 
  mutate(yield.kgperha = as.numeric(yield.kgperha)) -> dat_r100


# Does iwg grain yield differ by timing? ----------------------------------

# Ho: iwg grain yield does not differ when fertilizer is applied at different
# times

## v17
dat_v17 %>% 
  ggplot(aes(yield.kgperha)) +
  stat_bin(aes(fill=stand.age)) +
  labs(caption = "normally distributed within stand age")

lm1 <- lm(yield.kgperha~stand.age*ntiming,
          dat_v17)
anova(lm1)
# I ran mixed model, block didn't help and too many fixed effects made it rank
# deficient. This is the way. 

dat_v17 %>%
  distinct(ntiming)
  # distinct(stand.age)

# we reject Ho that grain yield is similar between 1yr 2yr and 3yr stands

# we reject Ho that grain yield is similar among timings (control, fall,
# spring, split) HOWEVER, let's see if we filter out control from dataset
  
dat_v17 %>% 
  filter(ntiming != "control") %>% 
  lm(yield.kgperha~stand.age*ntiming,
     .) %>% 
  anova()

# we cannot reject Ho that grain yield is similar among timings (fall, spring,
# split)

  
## R100

dat_r100 %>% 
  ggplot(aes(yield.kgperha)) +
  stat_bin(aes(fill=stand.age)) +
  labs(caption = "normally distributed by stand age")

dat_r100 %>% 
  # colnames()
  # lmer(
  #   yield.kgperha~napplied_update*
  #     ntiming+(1|block), 
  #   data=.) # rank defficient, block useless
  lm(
    yield.kgperha~stand.age*ntiming,
    .
  ) %>% 
  # summary()
  anova()
  # emmeans(~ntiming)

# reject Ho that grain yield is similar among stand.age

# cannot reject Ho that grain yield is similar among different timings


## staples 

# need to get excel document downloaded on work computer


## Compiled
dat_v17 %>% 
  filter(ntiming != "control") %>% 
  ggplot(aes(ntiming,yield.kgperha)) +
  geom_jitter(width=.2,
              aes(col=stand.age)) +
  geom_boxplot(fill=NA,
               width=.2) +
  labs(caption = "v17")

dat_r100 %>% 
  filter(ntiming != "control") %>% 
  ggplot(aes(ntiming,yield.kgperha)) +
  geom_jitter(width=.2,
              aes(col=stand.age)) +
  geom_boxplot(fill=NA,
               width=.2) +
  labs(caption = "r100")


# yld~timing*age*site

# Applied research question addressed: "I'd prefer to fertilize in fall vs.
# spring, can I apply fertilize in the fall and get high yields?


# Does iwg grain yield differ by rate? ------------------------------------

# Ho: Grain yield does not differ among different fertilizer rates


# yld~rate*age*site subsetted within timing?
# yld~timing*rate*age*site

# Applied research question addressed: "I'd prefer to fertilize in fall vs.
# spring, can I apply fertilize in the fall and get high yields? Do I need to
# apply more fertilizer in the fall to achieve the same yield response as a
# spring application?



# IF iwg grain yields differ by N rate, how do we model? ------------------


# 4 models: no effect, linear, quadratic, quadratic planar


# What is the optimum N rate for grain yield? -----------------------------



# Which treatment does best? ----------------------------------------------

# Assuming you have to pick a fertilizer program to do every year, what is the
# best program?

# yld~treatment*site*year


# Do grain yields differ when fertilizer is split applied? ----------------




