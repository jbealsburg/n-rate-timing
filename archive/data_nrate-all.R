source("nrate_eda_9dec2022.R")
source("new-dataset.R")

dat_new %>% 
  dplyr::select(
    year,location,id, block, stand.age, 
    treatment, 
    fall,spring,summer,
    yield.kgperha, lodging,
    height_1,height_2,height_3,height_4,height_5,
    cumn,cumulative.grain.yield
  ) %>% 
  write.csv(
    "data_nrate-all.csv",
    row.names = F
  )
glimpse()