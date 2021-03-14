library("dplyr") 
library("tidyverse")
# install.packages("sf") # make sure you have latest version
library("sf")
library("tigris")

chicagovaccinelocations <- read_sf("https://data.cityofchicago.org/api/geospatial/6q3z-9maq?method=export&format=GeoJSON")
st_write(chicagovaccinelocations,"C:/Users/scott/Desktop/delete/ally/chicagovaccinelocations.shp")

IL_Tracts_geom <- tracts("IL", cb=TRUE, class="sf")
CookCounty_Tracts_geom <- IL_Tracts_geom %>% filter(COUNTYFP == "031")
st_write(CookCounty_Tracts_geom,"C:/Users/scott/Desktop/delete/ally/CookCounty_Tracts_geom.shp")
st_write(st_centroid(CookCounty_Tracts_geom),"C:/Users/scott/Desktop/delete/ally/CookCounty_Tracts_geom_XY.shp")
