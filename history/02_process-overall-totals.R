library(tidyverse)
library(tidycensus)

history = read_csv('data/nhgis-raw/tables/nhgis0022_csv/nhgis0022_ts_nominal_county.csv')

history

latest = get_acs(
  geography = 'county',
  year = 2019,
  variables = c(
    'total' = 'B25002_001',
    'occupied' = 'B25002_002',
    'vacant' = 'B25002_003'
  )
)

latest

counties.totals = history %>% 
  transmute(
    YEAR,
    GISJOIN, 
    NAME,
    total = A43AA + A43AB,
    occupied = A43AA,
    vacant = A43AB
  ) %>% 
  bind_rows(
    latest %>% 
      separate(NAME, sep = ', ', into = c('NAME', 'state')) %>% 
      select(GEOID, NAME, variable, estimate) %>% 
      pivot_wider(names_from = 'variable', values_from = 'estimate') %>% 
      transmute(
        YEAR = 2019,
        GISJOIN = glue::glue('G{str_sub(GEOID, end = 2)}0{str_sub(GEOID, start = 3)}0'),
        NAME,
        total, occupied, vacant
      )
  ) %>% 
  mutate(pct.vacant = vacant / total)

counties.totals

counties.totals %>% write_csv('data/processed/units-counties-history.csv')