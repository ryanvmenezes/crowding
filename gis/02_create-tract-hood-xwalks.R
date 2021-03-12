library(tidyverse)
library(sf)
library(glue)

mapla = read_sf('data/processed/mapping-la-neighborhoods.geojson') %>% 
  st_transform(3311)

mapla

years = c(1960, 1970, 1980, 1990, 2000, 2010, 2019)

tracts = map(
  years,
  ~read_sf(glue('data/processed/la-oc-tracts-{.x}.geojson')) %>% 
    st_transform(3311) %>% 
    transmute(year = .x, GISJOIN)
) %>% 
  bind_rows()

tracts

intersected = st_intersection(
  st_buffer(tracts, 0),
  st_buffer(mapla %>% mutate(mapla.area = as.double(st_area(.))), 0),
) %>% 
  mutate(
    piece.area = as.double(st_area(.)),
    pct.mapla = piece.area / mapla.area
  ) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

intersected

intersected %>% write_csv('data/processed/tract-mapla-xwalk.csv')
