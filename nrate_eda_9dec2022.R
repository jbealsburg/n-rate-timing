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
# lm = yield~N_prior_to_harvest*standage

# lm = yield ~ N_spring*N_summer*N_fall*standage


# comments ----------------------------------------------------------------

# grain yields are ranging from 10-100 kg ha at each site, is this a conversion
# error?

# staples dataset makes the most sense, feels like strongest dataset 

# r100 seems weird, unfertilized and low fertilized did best--was this due to
# lodging?

# V17 seems good, but maybe a little lighter on data

# key conclusions ---------------------------------------------------------

# yields differ among stand age
# yields differ in plots that were not fertilized vs plots that were fertilized




# import data -------------------------------------------------------------
library(tidyverse)

# importing master dataset
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
    rate_summer = rate_total - 60) 
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

# STOP at 3pm 11Dec

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

# there are like 3 staples datasets...
staples <- read.csv("Staples.csv")
staples_split <- read.csv("Staples.Nsplit.analysis.only.csv")
staples.fallspringonly<- subset(staples.cumulative.dataset, 
                                Ntiming %in% c("Unfertilized","Fall","Spring"))

staples %>% 
  rename_all(tolower) %>% 
  # colnames()
  dplyr::select(year,experiment,location,id,treatment,
                napplied,ntiming,stand.age,block,
                ntiming_actual, #is ntiming actual same as ntiming_newlabel?
                updatednrate, #same as napplied_update
                updatedncumsum,
                yield.kgperha,
                cumulative.grain.yield
  ) %>% 
  mutate_all(factor) %>% 
  mutate(yield.kgperha = as.numeric(yield.kgperha),
         cumulative.grain.yield = as.numeric(cumulative.grain.yield)) -> dat_staples

dat_staples %>% 
  filter(ntiming == "Unfertilized" |
           ntiming == "Fall" |
           ntiming == "Spring") -> dat_staples_fallspring

staples_split  %>% 
  rename_all(tolower) %>% 
  # colnames()
  dplyr::select(year,experiment,location,id,treatment,
                napplied,ntiming,stand.age,block,
                summernrate, #only thing different
                yield.kgperha
  ) %>% 
  mutate_all(factor) %>% 
  mutate(yield.kgperha = as.numeric(yield.kgperha)) -> dat_staples_split

# seems like dat_staples is most robust, but lacks summernrate factor


  
# Does iwg grain yield differ by timing? ----------------------------------

# Ho: iwg grain yield does not differ when fertilizer is applied at different
# times

## v17
dat_v17 %>% 
  ggplot(aes(yield.kgperha)) +
  stat_bin(aes(fill=stand.age)) +
  labs(caption = "normally distributed within stand age")

dat_v17 %>% 
  # distinct(ntiming)
  filter(ntiming != "control") %>% 
  lm(yield.kgperha~stand.age*ntiming,
          .) -> lm1
anova(lm1)
# I ran mixed model, block didn't help and too many fixed effects made it rank
# deficient. This is the way. 

# we reject Ho that grain yield is similar between 1yr 2yr and 3yr stands

# we fail to reject Ho that grain yield is similar among timings (fall,
# spring, split) 

  
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
  # distinct(ntiming)
  filter(ntiming != "control") %>% 
  lm(
    yield.kgperha~stand.age*ntiming,
    .
  ) %>% 
  # summary()
  anova()
  # emmeans(~ntiming)

# reject Ho that grain yield is similar among stand.age

# fail to reject Ho that grain yield is similar among different timings


## staples 

dat_staples %>% 
  # glimpse()
  # lmer(
    # yield.kgperha~ntiming_actual*
      # stand.age+(1|block),
    # data=.) # rank defficient, block useless
  # distinct(ntiming)
  filter(ntiming != "Unfertilized") %>% 
  lm(
    # yield.kgperha~stand.age*ntiming_actual,
    yield.kgperha~stand.age*ntiming,
    .
  ) %>% 
  # summary()
  anova()
  # emmeans(~ntiming) 

# reject Ho that grain yield is the same among timing HOWEVER we may not be able
# to reject this at all stand ages

dat_staples %>% 
  filter(ntiming != "Unfertilized") %>% 
  ggplot(aes(
    ntiming,
    # ntiming_actual,
    yield.kgperha)) +
  stat_summary() +
  # geom_jitter(width = .2) +
  # geom_boxplot(fill=NA,width=.2) +
  facet_wrap(~stand.age)

# looks like trend is consistent except for year 1 which makes sense because
# ntiming was weird because in fall year 1 don't see that fertilizer until year
# 2. I say we deal with that data another way that adding in a bunch of new
# treatment names as in ntiming_actual



## Compiled

# grain yields differ among stand age and between fertilizer (yes vs no), but
# among fertilized plots we fail to reject Ho that grain yields are similar

dat_v17 %>% 
  filter(ntiming != "control") %>% 
  ggplot(aes(ntiming,yield.kgperha)) +
  geom_jitter(width=.2,
              aes(col=stand.age)) +
  geom_boxplot(fill=NA,
               width=.2) +
  labs(caption = "v17\n fail to reject Ho that grain yield is similar among different timings")

dat_r100 %>% 
  filter(ntiming != "control") %>% 
  ggplot(aes(ntiming,yield.kgperha)) +
  geom_jitter(width=.2,
              aes(col=stand.age)) +
  geom_boxplot(fill=NA,
               width=.2) +
  labs(caption = "r100\nfail to reject ho that grain yield is similar among timings")



dat_staples %>% 
  filter(ntiming != "Unfertilized") %>% 
  ggplot(aes(ntiming,yield.kgperha)) +
  geom_jitter(width=.2,
              aes(col=stand.age)) +
  geom_boxplot(fill=NA,
               width=.2) +
  facet_wrap(~stand.age) +
  labs(caption = "staples
  reject ho that yield is same among timing but with standage interaction
       facet wrapping by stand.age due to interaction
       I understand ntiming is not accurate for stand.age==1
       likely adjusting ntiming will remove interaction, but I don't like the ntiming_update ")


# Applied research question addressed: "I'd prefer to fertilize in fall vs.
# spring, can I apply fertilize in the fall and get high yields?

# ANSWER: if fertilizer timing (fall vs. spring) impacts yield potential, we
# failed to detect any differences in V17 and R100. In staples, grain yields
# were higher in the second and third years where fertilizer was applied in the
# fall.


# Does iwg grain yield differ by rate? ------------------------------------

# Ho: Grain yield does not differ among different fertilizer rates

# yld~rate*age*site subsetted within timing?
# yld~timing*rate*age*site

# running all code in "Staples.Rmd"

gg1
gg2 + labs(caption = "staples")
gg3 + labs(caption = "staples")
gg4 + labs(caption = "staples")


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

# mixed effect models didn't really add much

# V17
dat_v17 %>% 
  # glimpse()
  lmer(yield.kgperha~treatment*stand.age +
         (1|block),.) %>% 
  # lm(yield.kgperha~treatment*stand.age,
     # .) %>% 
  # anova()
  emmeans(~treatment)
# 80 fall split for V17

#R100
dat_r100 %>% 
  # glimpse()
  lmer(yield.kgperha~treatment*stand.age +
         (1|block),.) %>% 
  # lm(yield.kgperha~treatment*stand.age,
     # .) %>% 
  # anova()
  emmeans(~treatment) %>% 
  as_tibble() %>% 
  arrange(emmean)
# 60 spring for R100, control is also high. Maybe lodging explains?

# staples
dat_staples %>% 
  # glimpse()
  lmer(yield.kgperha~treatment*stand.age +
         (1|block),.) %>%
  # car::Anova()
  # lm(yield.kgperha~treatment*stand.age,
     # .) %>%
  # anova()
  emmeans(~treatment) %>% 
  as_tibble() %>% 
  arrange(emmean)
# 100 fall split

# we would conclude the fall split applications were associated with the highest
# yielding plots in V17 and staples. R100 yields were highest in spring applied
# and control


# Do grain yields differ when fertilizer is split applied? ----------------

dat_v17 %>% 
  # glimpse()
  mutate(split = str_detect(treatment,"split")) %>% 
  # glimpse()
  distinct(treatment)
# no split treatments in V17


dat_r100 %>% 
  # glimpse()
  filter(treatment != "control") %>% 
  filter(treatment != "140 fall split" &
           treatment != "60 fall" &
           treatment != "60 spring") %>% # making split and not split even
  mutate(split = str_detect(treatment,"split")) %>% 
  # glimpse()
  group_by(split) %>%
  # distinct(treatment)  
  # arrange(treatment)
  # tally()
  # lm(yield.kgperha~split,.) %>%
  # anova() #lol, even less of an effect after balancing split and not split
  ggplot(aes(split,yield.kgperha)) +
  geom_jitter(width=.2,set.seed(314)) +
  geom_boxplot(fill=NA,
               width = .2) +
  labs(caption = "r100
       fail to reject Ho that yield is the same when fertilizer is split or not")


dat_staples %>% 
  # glimpse()
  mutate(split = str_detect(treatment,"split")) %>% 
  filter(treatment != "control") %>% 
  distinct(treatment)
  # glimpse()
  group_by(split) %>%
  tally()
  # lm(yield.kgperha~split,.) %>% 
  # anova()
  ggplot(aes(split,yield.kgperha)) +
  geom_jitter(width=.2,set.seed(314)) +
  geom_boxplot(fill=NA,
               width = .2) +
  labs(caption = "staples
       fail to reject Ho that yield is the same when fertilizer is split or not")



