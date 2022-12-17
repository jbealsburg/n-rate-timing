# Targeted hypothesis testing of the N-rate timing dataset
# Jesse
# 9Dec2022
# Due date: 12Dec2022


# Preliminary findings ----------------------------------------------------

# I read the report by Dominic and I felt we need to simplify the analysis and story. 

# if there's time and space we can expand with supplementary material and the
# more complex aspects of the models dominic fitted and the limitations of the
# conclusions we can draw from them.

# It seems dominic analyzed each site seperately and each have their own
# Rmarkdown files. Dataframe naming is not consistent. Sometimes 0N plots are
# "Unfertilized", sometimes "control", sometimes "ntiming_update", sometimes
# "nupdate_timing".

# comments ----------------------------------------------------------------

# grain yields are ranging from 10-100 kg ha at each site, is this a conversion
# error?

# staples dataset makes the most sense and feels like the strongest dataset 

# r100 seems weird, unfertilized and low fertilized did best--was this due to
# lodging?

# V17 seems good, but less responsive to treatments

# key conclusions ---------------------------------------------------------

# yields differ among stand age
# yields differ in plots that were not fertilized vs plots that were fertilized


# import data -------------------------------------------------------------
library(tidyverse)
library(lme4)
library(emmeans)
library(multcomp)
library(multcompView)

# see "import_data_allsites.R"

# import data separately from sites ---------------------------------------

# Bringing in datasets from Dominics markdown files for hypothesis testing, each
# site remains separate

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

# there are multiple staples datasets
staples <- read.csv("Staples.csv")
staples_split <- read.csv("Staples.Nsplit.analysis.only.csv")

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



# Combining across sites --------------------------------------------------

# Since N rates and timing were not consistent across sites, analyzing sites
# together is difficult. 

dat_v17 %>% 
  bind_rows(dat_r100) %>% 
  bind_rows(dat_staples) %>% 
  ggplot(aes(yield.kgperha)) +
  geom_density(aes(col=location)) +
  labs(caption = "sites yielded similarly enough to combine
       V17 definately lower than the other 2")


# Option 1: 3 sites, filtered  dataset

# 3 timings: 0N, spring, fall

# [0N, spring(staples+r100:60 spring, v17:80 spring), fall(staples+r100:60 fall,
# v17:80 fall]

dat_v17 %>% 
  # distinct(treatment)
  filter(treatment != "80 Split") %>% 
  # colnames()
  mutate(timing = if_else(
    treatment == "80 Fall",
    "fall",
    if_else(
      treatment == "80 Spring",
      "spring",
      "control"
    )
  )) -> dat_v17_tiny

dat_r100 %>% 
  # distinct(treatment)
  filter(treatment == "60 spring" |
           treatment == "60 fall" |
           treatment == "control") %>% 
  # distinct(treatment)
  # colnames()
  mutate(timing = if_else(
    treatment == "60 fall",
    "fall",
    if_else(
      treatment == "60 spring",
      "spring",
      "control"
    )
  )) -> dat_r100_tiny


dat_staples %>% 
  # distinct(treatment)
  filter(treatment == "60 spring" |
           treatment == "60 fall" |
           treatment == "control") %>% 
  # distinct(treatment)
  # colnames()
  mutate(timing = if_else(
    treatment == "60 fall",
    "fall",
    if_else(
      treatment == "60 spring",
      "spring",
      "control"
    )
  )) -> dat_staples_tiny


# n=27 for each timing

dat_v17_tiny %>% 
  bind_rows(dat_r100_tiny) %>% 
  bind_rows(dat_staples_tiny) %>% 
  # group_by(timing) %>% 
  # tally()
  # filter(timing != "control") %>% 
  # lm(yield.kgperha~timing*stand.age*location,.) %>%
  # lmer(yield.kgperha ~ timing*stand.age +
  #        (1|location),.) %>% 
  # car::Anova()
  # anova()
  # emmeans(~timing*stand.age*location) %>% 
  # multcomp::cld(Letters=letters,reverse=T)
  # as_tibble()
mutate(location = fct_relevel(location,
                              "RSMT V17",
                              .before = "Staples")) %>%
  ggplot(aes(timing,yield.kgperha)) +
  stat_summary() +
  facet_wrap(~location*stand.age,
             scales = "free") +
  labs(caption = "staples shows what I expect...spring does best in yr 1, then fall for year 2 and 3
  r100 continues to be weird, v17 doesn't show much of a timing difference")

# CONCLUSION: Combining across sites shows no effect of fertilizer timing on
# yield except that it's better than not fertilzing. Sites are different,
# staples shows what I would expect (fertilize in spring for year 1 and then
# fall for year 2 and 3), R100 makes no sense and V17 shows little difference in
# timing. Could be explained by sandy staples soil being more responsive to
# fertilizer and V17 just being less responsive. It's a solid conclusion.

# BUT WE NEED TO CHANGE HOW WE CATEGORIZE TIMING
# year1 yields for fall fertilizer is no fertilizer

dat_v17_tiny %>% 
  bind_rows(dat_r100_tiny) %>% 
  bind_rows(dat_staples_tiny) %>%
  mutate(
    timing_improved = if_else(
      timing=="fall" & stand.age == "1",
      "control",
      timing
    ),
    .after=timing
  ) 


# Option 2: 2 sites, 18 treatments

# R100 does not have data for stand.age == 1, so to keep it balanced we need to
# remove the stand.age == 1 data from Staples. n=6 per treatment per site

dat_r100 %>% 
  bind_rows(dat_staples) %>% 
  # drop_na(yield.kgperha) %>%
  # distinct(stand.age)
  filter(stand.age != "1") %>% #R100 does not have 1st year stand age, so remove from stapes
  lm(
    yield.kgperha~treatment*stand.age*location,
    .) %>%
  # anova()
  emmeans(
    # ~treatment:stand.age
    ~treatment*location
  ) %>%
  multcomp::cld()
# ggplot(aes(location,yield.kgperha,
#            col=treatment,
#            fill = treatment)) +
# stat_summary(geom = "bar",
#              position = position_dodge(.6),
#              width=.6,
#              col=1)

# fall applied fertilizer is better in Staples (because we removed year 1 data)
# and the control and low fertiliy 60N treatments are best in R100 because R100
# data is weird. 

# CONCLUSION: Not a good option combining R100 with Staples. We need to cut out
# stand.age==1 data from staples to make comparison and the R100 data just adds
# more noise than insight...unless the insight is that kernza yield response to
# N is noisy--which will need to be discussed, but missing that first stand age
# data is devastating to the story and the control yielding near highest in R100
# makes me think either N was not limiting in R100, fertility treatments were
# incorrectly applied, N caused a penalty due to drought. The impact of N on
# kernza stands in year 2 and 3 is an angle, but we don't want to analyze with
# treatment as a factor with 18 levels but by grouping by timing or rate or split


# START - TIMING AND RATE ON 2-3 YR OLD STANDS FOR GRAIN YIELD
# may need to add in columns to R100
# nappliedupdate
# ntiming_Actual

# dat_r100 %>% 
#   bind_rows(dat_staples) %>% 
#   # colnames()
#   group_by(location,updatednrate) %>% 
#   tally()
#   filter(stand.age != "1") %>% #R100 does not have 1st year stand age, so remove from stapes
#   lm(
#     yield.kgperha~updatednrate,
#     # yield.kgperha~ntiming_actual,
#     .) %>%
#   anova()
#   emmeans(
#     # ~treatment:stand.age
#     ~ntiming_actual
#   ) %>%
#   multcomp::cld()
# END - TIMING AND RATE ON 2-3 YR OLD STANDS FOR GRAIN YIELD



# Option 3: Analyze all sites seperately

# This is what Dominic did. It seems R100 data showed no response to N, whereas
# Staples and V17 did.


# Option 4: Generate new dataset 

# The first solution is making a vector that is N applied prior to grain
# harvest, which dominic did for staples "updatednrate" and r100
# "napplied_update". I added an "updatednrate" column to V17 and made a column
# in r100 that's updatednrate.

dat_r100 %>% 
  mutate(updatednrate = napplied_update) -> dat_r100

dat_v17 %>% 
  # View()
  # colnames()
  # distinct(treatment)
  mutate(napplied = as.character(napplied)) %>% 
  mutate(updatednrate = if_else(
    treatment == "80 Fall" & stand.age =="1",
    "0",
    napplied
  )) %>% 
  mutate(updatednrate = if_else(
    treatment == "80 Split" & stand.age == "1",
    "40",
    napplied
  )) -> dat_v17

# you can fit a nice curve to the data

dat_r100 %>% 
  bind_rows(dat_v17) %>% 
  bind_rows(dat_staples) %>% 
  # lm(yield.kgperha~updatednrate*stand.age,.) %>%
  # anova()
  ggplot(aes(
    as.numeric(updatednrate),
    yield.kgperha,
    col=stand.age,
    group=stand.age,
    shape = location
  )) +
  geom_point() +
  geom_smooth() +
  labs(caption = "V17 messes up standage=1 curve")

# Let's take out V17

dat_r100 %>% 
  # bind_rows(dat_v17) %>% 
  bind_rows(dat_staples) %>% 
  # lm(yield.kgperha~updatednrate*stand.age,.) %>%
  # anova()
  ggplot(aes(
    as.numeric(updatednrate),
    yield.kgperha,
    col=stand.age,
    group=stand.age,
    shape = location
  )) +
  geom_point() +
  geom_smooth() +
  labs(caption = "Pretty nice curve without V17")

# seperate by site

dat_r100 %>% 
  bind_rows(dat_v17) %>% 
  bind_rows(dat_staples) %>% 
  # lm(yield.kgperha~updatednrate*stand.age,.) %>%
  # anova()
  # filter(location == "RSMT V17") %>% 
  ggplot(aes(
    as.numeric(updatednrate),
    yield.kgperha,
    col=stand.age,
    group=stand.age,
    shape = location
  )) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~location) +
  labs(caption = "yield~nrate is different among sites, can't combine
       note V17 only has 3 n rates so loess nor quadratic cannot be fit")


# but this fails to take into account timing. We null hypothesis test for the
# effect of timing on yield (failed to reject Ho that timing didn't matter), but
# we could just build it into the dataframe



# I may have luck rearrange data into the below format

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

# first compare cumulative yield as a function of cumulative fertilizer

# updatedncumsum and cumulative.grain.yield were only used in figures and no
# metadata exists for how they were calculated

## staples
dat_staples %>% 
  # View()
  # colnames()
  group_by(id,updatednrate) %>% 
  summarise(cumyield = cumsum(yield.kgperha)) %>% 
  # lm(cumyield~updatednrate,.) %>% 
  # anova()
  # emmeans::emmeans(~updatednrate)
  ggplot(aes(
    as.numeric(updatednrate),
    cumyield
  )) +
  geom_point() +
  # geom_smooth() # soooo quadratic omg
  geom_smooth(
    method = "lm",
    formula = y~poly(x,2),
    se=F
  ) +
  labs(caption = "staples
       reject Ho that yields are similar among N applied rates")

dat_v17 %>% 
  # colnames()
  # distinct(napplied) %>%
  group_by(id,napplied) %>% 
  summarise(cumyield = cumsum(yield.kgperha)) %>% 
  lm(cumyield~napplied,.) %>%
  anova()
# not a range of fertility levels, but we reject Ho that yields are similar
# among the 2 fertility levels (0N and 80N)


dat_r100 %>% 
  # colnames()
  group_by(id,napplied_update) %>% 
  summarise(cumyield = cumsum(yield.kgperha)) %>% 
  # lm(cumyield~napplied_update,.) %>%
  # anova()
  ggplot(aes(
    as.numeric(napplied_update),
    cumyield
  )) +
  geom_point() +
  geom_smooth() +
  labs(caption = "R100
       fail to reject Ho that grain yield is similar among fertilizer rates")

# Conclusion: Cumulative yields can be calculated for a given plot over the
# course of the experiment. Plots differ in the amount of N applied. Our Ho is
# that yields do not differ among plots with different N rates. At V17, we had
# two N rates (0 and 80N), we reject Ho that yields were similar among these two
# rates. At staples, we reject the Ho that yields are similar among multiple
# rates and find a very strong quadratic relationship between N rate and yield.
# At R100, we fail to reject Ho


# Compare yield response of n applied prior to harvest (we can assume
# timing does not matter from previous analysis)

dat_staples %>% 
  # colnames()
  ggplot(aes(
    as.numeric(updatednrate),
    yield.kgperha
  )) +
  geom_point(aes(col=stand.age)) +
  geom_smooth(aes(group = stand.age,
                  col=stand.age),
              # method = "lm",
              # formula = y~poly(x,2)
              )
dat_staples %>% 
  lm(
  yield.kgperha~poly(as.numeric(updatednrate),2)*stand.age,
    # yield.kgperha~as.numeric(updatednrate)*stand.age,
  .) %>%
  anova()
  # emmeans::emmeans(~poly(as.numeric(updatednrate),2)*stand.age)
# we reject Ho that grain yields are similar among nrate and stand age, but
# there is an interaction and this effect is not consistent. It seems
# approximately quadratic in all years, but in year 2 there is more of a decline
# at higher N rates which is not true in the other years

# V17
# only 2 rates, cannot make this comparison

# R100
# no difference among rates, let alone will fitting a curve be possible.


# running all code in "Staples.Rmd" to show figures dominic produced

# gg1
# gg2 + labs(caption = "staples")
# gg3 + labs(caption = "staples")
# gg4 + labs(caption = "staples")


# Applied research question addressed: "I'd prefer to fertilize in fall vs.
# spring, can I apply fertilize in the fall and get high yields? Do I need to
# apply more fertilizer in the fall to achieve the same yield response as a
# spring application?

# ANSWER: Previously we were unable to reject the Ho that timing of fertilizer
# does not impact yield. With timing being equal, yields seem to be highest
# around 5-7 nrate. This is most important in the first and second year, but
# overfertilizing in the second year can result in lower yields


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
  # group_by(split) %>%
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
  # distinct(treatment)
  # glimpse()
  # group_by(split) %>%
  # tally()
  # lm(yield.kgperha~split,.) %>% 
  # anova()
  ggplot(aes(split,yield.kgperha)) +
  geom_jitter(width=.2,set.seed(314)) +
  geom_boxplot(fill=NA,
               width = .2) +
  labs(caption = "staples
       fail to reject Ho that yield is the same when fertilizer is split or not")



