library(tidyverse)
library(tidycensus)

data = get_pums(
  state = 'CA',
  variables = c(
    'PUMA', # public use microdata area (first three digits county)
    'RMSP', # rooms
    'NP', # people
    'AGEP', # age of householder
    'TEN', # tenure (owner/renter)
    'FES', # household type
    'CIT', # citizenship (immigrant or not)
    'YOEP', # year of entry into county
    'RAC1P', # race
    'RAC2P', # race detailed
    'HISP', # hispanic origin
    'FINCP', # income
    'MULTG', # mutligenerational household
    'YBL', # year structure first built
    'MV' # when moved into this house/apt
  ),
  rep_weights = 'housing',
  # show_call = TRUE
)

beepr::beep()

data

data %>% write_rds('data/raw/california-pums.rds', compress = 'bz')

beepr::beep()
