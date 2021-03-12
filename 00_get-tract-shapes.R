library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

state.fips = fips_codes %>% 
  as_tibble() %>% 
  select(contains('state')) %>% 
  distinct() %>% 
  # take out these territories, the get_acs function call breaks on them
  filter(!(state %in% c('AS','GU','MP','UM','VI')))

state.fips

get.shapes.for.state = function(s) {
  get_acs(
    geography = 'tract',
    state = s,
    variables = 'B01001_001E',
    geometry = TRUE,
    year = 2019,
    cache_table = TRUE
  )
}

shapes = state.fips %>% 
  mutate(
    state.data = map(
      state_code,
      get.shapes.for.state
    )
  )

beepr::beep()

shapes

all.shapes = shapes %>% 
  unnest(c(state.data)) %>%
  ungroup() %>%
  st_as_sf() %>% 
  select(-variable, -estimate, -moe)

beepr::beep()

all.shapes

all.shapes %>% write_rds('data/processed/tracts-2019.rds', compress = 'gz')
