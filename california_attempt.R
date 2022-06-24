library(readr)
library(sf)
library(dplyr)

safegraph <- read_csv("https://www.dropbox.com/s/6o4hwvlm8w7d5hu/cali_nhood_safegraph.csv?dl=1")
la_census_blocks <- st_read("https://www.dropbox.com/s/miu428w0otabiy8/LA_2010_Census_Block_Groups.geojson?dl=1")
la_safegraph <- left_join(la_census_blocks, safegraph, by = c("GEOID10" = "area"))
la_crim <- read_csv("https://www.dropbox.com/s/y8ys52hwjhuho9a/la_crime_june.csv?dl=1")
