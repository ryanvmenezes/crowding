library(tidyverse)
library(tidycensus)
library(sf)
library(glue)

process.shapefile = function(year, path) {
  shp = read_sf(path)
  
  this.filter = function(df) {
    if (year < 2010) {
      final = shp %>% 
        filter(NHGISST == '060', COUNTY %in% c('037','059'))
    } else if (year == 2010) {
      final = shp %>% 
        filter(STATEFP10 == '06', COUNTYFP10 %in% c('037','059'))
    } else if (year == 2019) {
      final = shp %>% 
        filter(STATEFP == '06', COUNTYFP %in% c('037','059'))
    }
    return(final)
  }
  
  laoc = shp %>% 
    this.filter() %>%
    select(GISJOIN) %>% 
    st_transform(4269) %>% 
    filter(!str_detect(GISJOIN, 'G0600370599'))
  
  # print(year)
  # print(laoc %>% st_set_geometry(NULL) %>% count(str_sub(GISJOIN, end = 8)))
  
  laoc %>% write_sf(glue('data/processed/la-oc-tracts-{year}.geojson'), delete_dsn = TRUE)
}


paths = tribble(
  ~year, ~path,
  1960, 'data/nhgis-raw/shapes/nhgis0017_shapefile_tl2008_us_tract_1960/',
  1970, 'data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_1970/',
  1980, 'data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_1980/',
  1990, 'data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_1990/',
  2000, 'data/nhgis-raw/shapes/nhgis0005_shapefile_tl2008_us_tract_2000/',
  2010, 'data/nhgis-raw/shapes/nhgis0005_shapefile_tl2010_us_tract_2010/',
  2019, 'data/nhgis-raw/shapes/nhgis0005_shapefile_tl2019_us_tract_2019/',
)

paths %>% pwalk(process.shapefile)
