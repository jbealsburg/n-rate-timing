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