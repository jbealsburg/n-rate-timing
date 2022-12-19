
# creating a full dataset of all possible options

c("staples","v17","r100") -> site_opts

c(2018:2021) -> year_opts

c("none","fall","spring","summer") -> timing_opts

seq(0,140,20) -> rate_opts

expand.grid(
  site_opts,
  year_opts,
  timing_opts,
  rate_opts,
  stringsAsFactors = T
) %>% 
  rename(site = Var1,
         year = Var2,
         timing = Var3,
         rate = Var4)-> dat_full

# dat_full is all possible data we could have

# Now we filter to what we do have


# V17 ---------------------------------------------------------------------

# timing = fall spring 
# rate = 40 for split, 80 for others
# year = 2019:2021


dat_full %>% 
  filter(site == "v17") %>% 
  filter(year != "2018") %>% 
  filter(timing != "summer") %>% 
  filter(rate == "40" |
           rate == "80" | 
           rate == "0") 
  
  
