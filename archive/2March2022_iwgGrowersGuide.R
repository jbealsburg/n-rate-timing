
# Go to report.Rmd and run all the code to populate your environment with
# correct dataframes and packages

# Now we are ready to make figures

# Jake wants a figure for kernza growers guide using nrate timing data

# Criteria
# x=timing
# y= yield
# data = subset of fertilizer rate
# facet = stand age


# for fertilizer rates, I am using "cumn" in dat_new. This has the cumulative
# amounts N applied between grain harvests.
# cumn = 80, 100 and 120, these are the 3 rates he wants

## Deciding to just use data from staples site. This dataset is the most
## complete and appropiate for using a timing on the x axis that is a factor


# 80N Staples
dat_new %>% 
  filter(location == "Staples") %>% 
  filter(cumn=="80") %>%
  ggplot(aes(
    x=ntiming,
    y=yield.kgperha
  )) +
  stat_summary(
    geom = "bar"
  ) +
  facet_wrap(~stand.age) +
  labs(
    x="Timing of 80N application prior to grain harvest where split application were 20N in summer, 60N in spring/fall",
    title = "Staples 80N"
  )

# 100N staples
dat_new %>% 
  filter(location == "Staples") %>% 
  filter(cumn=="100") %>%
  ggplot(aes(
    x=ntiming,
    y=yield.kgperha
  )) +
  stat_summary(
    geom = "bar"
  ) +
  facet_wrap(~stand.age) +
  labs(
    x="Timing of 100N application prior to grain harvest where split application were 40N in summer, 60N in spring/fall",
    title = "Staples 100N"
  )


# 120N staples
dat_new %>% 
  filter(location == "Staples") %>% 
  filter(cumn=="120") %>%
  ggplot(aes(
    x=ntiming,
    y=yield.kgperha
  )) +
  stat_summary(
    geom = "bar"
  ) +
  facet_wrap(~stand.age) +
  labs(
    x="Timing of 120N application prior to grain harvest where split application were 60N in summer, 60N in spring/fall",
    title = "Staples 120N"
  )

## Looking at figures, let's make a figure with 3 rates facet wrapped

dat_new %>% 
  filter(location == "Staples") %>% 
  mutate(ntiming = factor(ntiming,
                          levels = c(
                            "Fall",
                            "Fall split",
                            "Spring",
                            "Spring split"
                          ))) %>% 
  filter(cumn=="120" | 
           cumn=="100" | 
           cumn=="80") %>%
  ggplot(aes(
    x=ntiming,
    y=yield.kgperha,
    fill=factor(cumn)
  )) +
  stat_summary(
    geom = "bar",
    position = position_dodge(.5),
    width = .5,
    col=1
  ) +
  facet_wrap(~stand.age) +
  labs(
  )

# Hmmmm, let's just use the 80N rate

# final figure ------------------------------------------------------------


dat_new %>% 
  filter(location == "Staples") %>% 
  mutate(ntiming = factor(ntiming,
                          levels = c(
                            "Fall",
                            "Fall split",
                            "Spring",
                            "Spring split"
                          ))) %>% 
  filter(cumn=="80") %>%
  filter(ntiming != "Fall split" | 
           stand.age != 1) %>% 
  mutate(
    stand.age = fct_recode(stand.age,
                           "1st year " = "1",
                           "2nd year" = "2",
                           "3rd year" = "3"
    )
  ) %>% 
  mutate(yield.kgperha = yield.kgperha/1.121) %>% 
  ggplot(aes(
    x=ntiming,
    y=yield.kgperha )) +
  stat_summary(
    geom = "bar",
    col=1
  ) +
  facet_wrap(~stand.age) +
  labs(
    y= "Kernza grain yield \n(lbs per acre)",
    x= "Timing of applying 80 lbs of N per acre",
    caption= "Split application were 20 lbs N in summer, 60 lbs N in spring/fall
  Fall timings are 17Oct and 31Oct for 2nd and 3rd year
  Spring timings are 4May, 22Apr and 12May for 1st 2nd and 3rd year
  Summer timings are 1Jun 23May and 23Jun for 1st 2nd and 3rd year
  Data collected in Staples MN from 2018-2020
  "
  )


# Improving figure aestetics ----------------------------------------------

dat_new %>% 
  filter(location == "Staples") %>% 
  mutate(ntiming = factor(ntiming,
                          levels = c(
                            "Fall",
                            "Fall split",
                            "Spring",
                            "Spring split"
                          ))) %>% 
  filter(cumn=="80") %>%
  filter(ntiming != "Fall split" | 
           stand.age != 1) %>% 
  mutate(
    stand.age = fct_recode(stand.age,
                           "1st year " = "1",
                           "2nd year" = "2",
                           "3rd year" = "3"
    )
  ) %>% 
  mutate(yield.kgperha = yield.kgperha/1.121) %>% 
  ggplot(aes(
    x=ntiming,
    y=yield.kgperha )) +
  stat_summary(
    geom = "bar",
    col=1
  ) +
  facet_wrap(~stand.age) +
  labs(
    y= "Kernza grain yield \n(lbs per acre)",
    x= "Timing of applying 80 lbs of N per acre",
    caption= "
  
  Split application were 20 lbs N in summer, 60 lbs N in spring/fall
  Fall timings are 17Oct and 31Oct for 2nd and 3rd year
  Spring timings are 4May, 22Apr and 12May for 1st 2nd and 3rd year
  Summer timings are 1Jun 23May and 23Jun for 1st 2nd and 3rd year
  Data collected in Staples MN from 2018 - 2020
  "
  ) -> plot1

plot1 %>% 
  ggsave(
    "KernzaGrowerGuide_nrate.png",
    .,
    device = "png",
    width = 8,
    height = 4,
    units = "in",
    dpi = 500
  )
