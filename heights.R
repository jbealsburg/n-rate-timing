# heights

source("new-dataset.R")

# see if there are any issues with height data. Ideally I want to put this as a
# single averaged value into dat_new

# There are only 3 site-years of data on heights. 
# Variation is too large to just summarize the center. 

dat_new %>% 
  dplyr::select(
    c(year,location,id) |
      starts_with("height") 
  ) %>% 
  # glimpse()
  pivot_longer(
    cols = starts_with("height")
  ) %>% 
  ggplot(aes(value)) +
  stat_bin() +
  facet_wrap(~interaction(location,year)) +
  labs(caption = "only have 3 site-years of height data")

dat_new %>% 
  dplyr::select(
    c(year,location,id) |
      starts_with("height") 
  ) %>% 
  # glimpse()
  pivot_longer(
    cols = starts_with("height")
  ) %>% 
  group_by(location,year,id) %>%
  # group_by(location,year) %>%
  summarise(max = max(value),
            min = min(value),
            range = max-min,
            mean = mean(value)) %>%
  arrange(desc(range)) %>% 
  ggplot(aes(range)) +
  stat_bin() +
  facet_wrap(~interaction(location,year)) +
  labs(caption = "huge ranges of height values from R100-2019
       a little more reasonable for V17, but plants generally much shorter",
       x="range of plant height values, i.e. max-min. n=5 per plot")

# huge amounts of variation in height data in most plots.

# Probably can't just summarise with a single mean value due to large ranges,
# median maybe, but that's uncommon. Need to show the variation. Maybe CV?
