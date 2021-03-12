library(tidyverse)
library(sf)

mapla = read_sf('data/processed/mapping-la-neighborhoods.geojson') %>% 
  st_transform(3311)

mapla

s60 = read_sf('data/processed/la-oc-tracts-1960.geojson') %>% 
  st_transform(3311)

s60

s70 = read_sf('data/processed/la-oc-tracts-1970.geojson') %>% 
  st_transform(3311)

s70

s80 = read_sf('data/processed/la-oc-tracts-1980.geojson') %>% 
  st_transform(3311)

s80

s90 = read_sf('data/processed/la-oc-tracts-1990.geojson') %>% 
  st_transform(3311)

s90

s00 = read_sf('data/processed/la-oc-tracts-2000.geojson') %>% 
  st_transform(3311)

s00

s10 = read_sf('data/processed/la-oc-tracts-2010.geojson') %>% 
  st_transform(3311)

s10

s19 = read_sf('data/processed/la-oc-tracts-2019.geojson') %>% 
  st_transform(3311)

s19

tracts = bind_rows(
  s60 %>% mutate(year = 1960),
  s70 %>% mutate(year = 1970),
  s80 %>% mutate(year = 1980),
  s90 %>% mutate(year = 1990),
  s00 %>% mutate(year = 2000),
  s10 %>% mutate(year = 2010),
  s19 %>% mutate(year = 2019)
)

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
