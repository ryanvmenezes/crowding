library(tidyverse)
library(tidycensus)
library(sf)

# 1960 --------------------------------------------------------------------

shp60 = read_sf('data/nhgis-raw/shapes/nhgis0017_shapefile_tl2008_us_tract_1960/')

shp60

laoc60 = shp60 %>% 
  filter(NHGISST == '060', COUNTY %in% c('037','059')) %>% 
  transmute(
    tract = GISJOIN,
    tract = str_replace(tract, 'G0600370', '06037'),
    tract = str_replace(tract, 'G0600590\\w', '06059'),
    tract = str_sub(tract,end = 9)
  ) %>% 
  group_by(tract) %>%
  summarise() %>%
  st_transform(4269) %>% 
  filter(!str_detect(tract, '599'))

laoc60 %>%
  ggplot() +
  geom_sf()

laoc60 %>% write_sf('data/processed/la-oc-tracts-1960.geojson', delete_dsn = TRUE)

# 1970 --------------------------------------------------------------------

shp70 = read_sf('data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_1970/')

shp70

laoc70 = shp70 %>% 
  filter(NHGISST == '060', COUNTY %in% c('037','059')) %>% 
  transmute(
    tract = GISJOIN,
    tract = str_replace(tract, 'G0600370', '06037'),
    tract = str_replace(tract, 'G0600590', '06059'),
    tract = str_sub(tract,end = 9)
  ) %>% 
  group_by(tract) %>%
  summarise() %>%
  st_transform(4269) %>% 
  filter(!str_detect(tract, '599'))
  
laoc70 %>%
  ggplot() +
  geom_sf()

laoc70 %>% write_sf('data/processed/la-oc-tracts-1970.geojson', delete_dsn = TRUE)

# 1980 --------------------------------------------------------------------

shp80 = read_sf('data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_1980/')

shp80

laoc80 = shp80 %>% 
  filter(NHGISST == '060', COUNTY %in% c('037','059')) %>% 
  transmute(
    tract = GISJOIN,
    tract = str_replace(tract, 'G0600370', '06037'),
    tract = str_replace(tract, 'G0600590', '06059'),
    tract = str_sub(tract,end = 9)
  ) %>% 
  group_by(tract) %>%
  summarise() %>%
  st_transform(4269) %>% 
  filter(!str_detect(tract, '599'))

laoc80 %>%
  ggplot() +
  geom_sf()

laoc80 %>% write_sf('data/processed/la-oc-tracts-1980.geojson', delete_dsn = TRUE)

# 1990 --------------------------------------------------------------------

shp90 = read_sf('data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_1990/')

shp90

laoc90 = shp90 %>% 
  filter(NHGISST == '060', COUNTY %in% c('037','059')) %>% 
  transmute(
    tract = GISJOIN,
    tract = str_replace(tract, 'G0600370', '06037'),
    tract = str_replace(tract, 'G0600590', '06059'),
    tract = str_sub(tract,end = 9)
  ) %>% 
  group_by(tract) %>%
  summarise() %>%
  st_transform(4269) %>% 
  filter(!str_detect(tract, '599'))

laoc90 %>%
  ggplot() +
  geom_sf()

laoc90 %>% write_sf('data/processed/la-oc-tracts-1990.geojson', delete_dsn = TRUE)

# 2000 --------------------------------------------------------------------

shp00 = read_sf('data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_2000/')

shp00

laoc00 = shp00 %>% 
  filter(NHGISST == '060', COUNTY %in% c('037','059')) %>% 
  transmute(
    tract = GISJOIN,
    tract = str_replace(tract, 'G0600370', '06037'),
    tract = str_replace(tract, 'G0600590', '06059'),
    tract = str_sub(tract,end = 9)
  ) %>% 
  group_by(tract) %>%
  summarise() %>%
  st_transform(4269) %>% 
  filter(!str_detect(tract, '599'))

laoc00

laoc00 %>% 
  ggplot() + 
  geom_sf()

laoc00 %>% write_sf('data/processed/la-oc-tracts-2000.geojson', delete_dsn = TRUE)

# 2010 --------------------------------------------------------------------

shp10 = read_sf('data/nhgis-raw/shapes/nhgis0005_shapefile_tl2010_us_tract_2010/')

shp10

laoc10 = shp10 %>% 
  filter(STATEFP10 == '06', COUNTYFP10 %in% c('037','059')) %>% 
  transmute(
    tract = GISJOIN,
    tract = str_replace(tract, 'G0600370', '06037'),
    tract = str_replace(tract, 'G0600590', '06059'),
    tract = str_sub(tract,end = 9)
  ) %>% 
  group_by(tract) %>%
  summarise() %>%
  st_transform(4269) %>% 
  filter(!str_detect(tract, '599'))

laoc10

laoc10 %>% 
  ggplot() + 
  geom_sf()

laoc10 %>% write_sf('data/processed/la-oc-tracts-2010.geojson', delete_dsn = TRUE)

# 2019 --------------------------------------------------------------------

shp19 = read_sf('data/nhgis-raw/shapes/nhgis0005_shapefile_tl2019_us_tract_2019/')

shp19

laoc19 = shp19 %>% 
  filter(STATEFP == '06', COUNTYFP %in% c('037','059')) %>% 
  transmute(
    tract = GISJOIN,
    tract = str_replace(tract, 'G0600370', '06037'),
    tract = str_replace(tract, 'G0600590', '06059'),
    tract = str_sub(tract,end = 9)
  ) %>% 
  group_by(tract) %>%
  summarise() %>%
  st_transform(4269) %>% 
  filter(!str_detect(tract, '599'))

laoc19

laoc19 %>% 
  ggplot() + 
  geom_sf()

laoc19 %>% write_sf('data/processed/la-oc-tracts-2019.geojson', delete_dsn = TRUE)

# mapping la --------------------------------------------------------------

mapla = read_sf('http://boundaries.latimes.com/1.0/boundary-set/la-county-neighborhoods-v6/?format=geojson')

mapla

mapla.clean = mapla %>% 
  transmute(
    hood.name = name,
    county = map_chr(metadata, ~jsonlite::fromJSON(.)$county),
    region = map_chr(metadata, ~jsonlite::fromJSON(.)$region),
  ) %>% 
  st_transform(4269)

mapla.clean

mapla.clean %>% write_sf('data/processed/mapping-la-neighborhoods.geojson', delete_dsn = TRUE)

mapla.clean %>% 
  ggplot() +
  geom_sf()
