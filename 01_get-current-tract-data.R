library(tidyverse)
library(tidycensus)

state.fips = fips_codes %>% 
  as_tibble() %>% 
  select(contains('state')) %>% 
  distinct() %>% 
  # take out these territories, the get_acs function call breaks on them
  filter(!(state %in% c('AS','GU','MP','UM','VI')))

state.fips

get.table.for.state = function(s) {
  get_acs(
    geography = 'tract',
    state = s,
    variables = c(
      # https://www.socialexplorer.com/data/ACS2018_5yr/metadata/?ds=ACS18_5yr&table=B25014
      "total" = 'B25014_001',
      "owner_occ" = 'B25014_002',
      "owner_occ_0.5_or_less_per_room" = 'B25014_003',
      "owner_occ_0.51_to_1.0_per_room" = 'B25014_004',
      "owner_occ_1.01_to_1.5_per_room" = 'B25014_005',
      "owner_occ_1.51_to_2.0_per_room" = 'B25014_006',
      "owner_occ_2.01_or_more_per_room" = 'B25014_007',
      "renter_occ" = 'B25014_008',
      "renter_occ_0.5_or_less_per_room" = 'B25014_009',
      "renter_occ_0.51_to_1.0_per_room" = 'B25014_010',
      "renter_occ_1.01_to_1.5_per_room" = 'B25014_011',
      "renter_occ_1.51_to_2.0_per_room" = 'B25014_012',
      "renter_occ_2.01_or_more_per_room" = 'B25014_013'
    ),
    year = 2019,
    cache_table = TRUE
  )
}

# get.table.for.state('AL')

occ.tables = state.fips %>% 
  mutate(
    state.data = map(
      state_code,
      get.table.for.state
    )
  )

beepr::beep()

occ.tables

occ.tables$state.data[[10]]

occ.table = occ.tables %>% 
  unnest(c(state.data)) %>% 
  separate(NAME, into = c('tractid', 'county', 'statename'), sep = ', ', remove = FALSE)

occ.table

total = occ.table %>% 
  filter(variable == 'total') %>% 
  select(GEOID, state, state_name, tractid, county, total = estimate)

total

crowded = occ.table %>% 
  filter(str_detect(variable, '1.01|1.51|2.01')) %>% 
  group_by(GEOID, state, state_name, tractid, county) %>% 
  summarise(crowded = sum(estimate)) %>% 
  ungroup()

crowded

not.crowded = occ.table %>% 
  filter(str_detect(variable, '0.5_or_less|0.51')) %>% 
  group_by(GEOID, state, state_name, tractid, county) %>% 
  summarise(not.crowded = sum(estimate)) %>% 
  ungroup()

not.crowded

calcs = total %>% 
  left_join(crowded) %>% 
  left_join(not.crowded)

calcs

# make sure the sums line up
calcs %>% 
  mutate(mismatch = !((crowded + not.crowded) == total)) %>% 
  summarise(mismatches = sum(mismatch))

# z-value calculation

overall.crowding = sum(calcs$crowded) / sum(calcs$total)

calcs = calcs %>% 
  mutate(
    pct.crowded = crowded / total,
    zval = (pct.crowded - overall.crowding) / sqrt(overall.crowding * (1 - overall.crowding) / total)
  )

calcs %>% arrange(-pct.crowded)

calcs %>% arrange(-zval)

calcs %>% arrange(-zval) %>% write_csv('data/processed/crowding-tracts-2019.csv')

# occ.table %>% write_csv('occupancy-data-tracts.csv')
occ.table %>% write_rds('data/processed/occupancy-tracts-2019.rds', compress = 'gz')
