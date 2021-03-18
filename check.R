library(tidyverse)

mapla.history = read_csv('data/processed/mapla-crowding-history.csv')

mapla.history

mapla.history %>% 
  drop_na(county) %>%
  group_by(year, county) %>% 
  summarise(
    total = sum(total),
    crowded = sum(crowded),
  )

check = read_csv('/Users/ryan/Downloads/nhgis0018_csv/nhgis0018_ts_nominal_county.csv')

check

check %>% 
  select(
    STATEFP, COUNTYFP,
    STATE, COUNTY,
    A43AA1970, A43AA1980, A43AA1990, A43AA2000, A43AA2010
  ) %>% 
  pivot_longer(-STATEFP:-COUNTY, names_to = 'year', values_to = 'occupied') %>% 
  mutate(year = str_sub(year, start = -4)) %>% 
  group_by(year) %>% 
  summarise(sum(occupied, na.rm = TRUE))
  filter(STATEFP == '06' & (COUNTYFP %in% c('037', '059')))
