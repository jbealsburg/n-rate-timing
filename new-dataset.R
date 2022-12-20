
# creating a full dataset of all possible options

# c("staples","v17","r100") -> site_opts
# 
# c(2018:2021) -> year_opts
# 
# c("fall","spring","summer") -> timing_opts
# 
# seq(0,140,20) -> rate_opts
# 
# seq(1:54) -> plot_opts
# 
# expand.grid(
#   site_opts,
#   year_opts,
#   plot_opts,
#   timing_opts,
#   rate_opts
# ) %>% 
#   rename(site = Var1,
#          year = Var2,
#          plot = Var3,
#          timing = Var4,
#          rate = Var5)-> dat_full

# dat_full is all possible data we could have

# Now we filter to what we do have


# V17 ---------------------------------------------------------------------

# plot = only 16
# timing = fall spring 
# rate = 40 for split, 80 for others
# year = 2019:2021

# 
# 
# dat_full %>% 
#   # glimpse()
#   filter(plot <= 16) %>% 
#   filter(site == "v17") %>% 
#   filter(year != "2018") %>% 
#   filter(timing != "summer") %>% 
#   filter(rate == "40" |
#            rate == "80" | 
#            rate == "0") 
#   
# fuck it, let's try this in excel
# nevermind!


dat_v17 %>% 
  # dplyr::select(stand.age,treatment) %>%
  mutate(gross_rate = readr::parse_number(as.character(treatment),
                                          na = "control")) %>% 
  mutate(gross_rate = replace_na(gross_rate,0)) %>%
  mutate(fall2 = if_else(
    treatment=="80 Fall",
    80,
    if_else(
      treatment == "80 Split",
      40,
      0
    )
  )) %>%
  mutate(fall = if_else(
    stand.age==1,
    0,
    fall2
  )) %>% 
  mutate(spring = if_else(
    treatment=="80 Spring",
    80,
    if_else(
      treatment == "80 Split",
      40,
      0
    )
  )) %>% 
  mutate(summer = 0) %>% 
  dplyr::select(-fall2)  -> dat_v172


# staples -----------------------------------------------------------------

# 60N in fall or spring
# varying rates in summer

dat_staples %>% 
  # dplyr::select(stand.age,treatment) %>%
  mutate(gross_rate = readr::parse_number(as.character(treatment),
                                          na = "control")) %>% 
  mutate(gross_rate = replace_na(gross_rate,0)) %>% 
  mutate(detect_fall = str_detect(treatment,"fall"),
         detect_spring = str_detect(treatment,"spring")) %>% 
  mutate(fall2 = if_else(
    detect_fall == T,
    60,
    0
  ),
  spring = if_else(
    detect_spring == T,
    60,
    0
  )
  ) %>%  
  mutate(summer = gross_rate-fall2-spring) %>% 
  mutate(fall = if_else(
    stand.age == "1",
    0,
    fall2
  ),
  .before=spring
  ) %>% 
  dplyr::select(-c(detect_fall,detect_spring,fall2)) -> dat_staples2


# R100 --------------------------------------------------------------------

# should be same as staples
# except no N rate in year 2

dat_r100 %>% 
  # dplyr::select(stand.age,treatment) %>%
  mutate(gross_rate = readr::parse_number(as.character(treatment),
                                          na = "control")) %>% 
  mutate(gross_rate = replace_na(gross_rate,0)) %>% 
  mutate(detect_fall = str_detect(treatment,"fall"),
         detect_spring = str_detect(treatment,"spring")) %>% 
  mutate(fall2 = if_else(
    detect_fall == T,
    60,
    0
  ),
  spring = if_else(
    detect_spring == T,
    60,
    0
  )
  ) %>%  
  mutate(summer = gross_rate-fall2-spring) %>% 
  mutate(fall = if_else(
    stand.age == "2",
    0,
    fall2
  ),
  .before=spring
  ) %>% 
  dplyr::select(-c(detect_fall,detect_spring,fall2)) -> dat_r1002


# bind together -----------------------------------------------------------

# what is the difference between a spring split and a fall split in a site where
# there is already a summer application?

dat_v172 %>% 
  bind_rows(dat_r1002) %>% 
  bind_rows(dat_staples2) %>% 
  dplyr::select(-gross_rate) %>% 
  mutate(
    across(
      starts_with("height"),
      as.numeric
    )) %>% 
  mutate(lodging=as.numeric(lodging)) %>% 
  mutate(height = (height_1+height_2+height_3+height_4+height_5)/5) %>% 
  dplyr::select(-c(height_1,height_2,height_3,height_4,height_5)) %>% 
  dplyr::select(location,stand.age, id, treatment,fall,spring,summer,yield.kgperha,lodging,height) %>% 
  mutate(cum_n_harvestreset = fall+spring+summer)-> dat_new

rm(dat_v172,dat_r1002,dat_staples2)
