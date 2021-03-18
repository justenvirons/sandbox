## ---------------------------
##
## Script name: Initial geocode of provider addresses
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-03-16
## Date Last Updated: 2021-03-16
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(arcgisbinding)
library(dplyr)
library(tidyverse)
library(ggmap)
library(sf)

masterlist_address <- masterlist %>% 
  mutate(FullAddress = paste(toupper(street), " ", toupper(city),", IL ",zip, sep=""))

register_google(key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
getOption("ggmap")
addresses = masterlist_address$FullAddress
length(addresses) # check number of addresses
locations <- geocode(addresses, key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
masterlist_address_geo <- masterlist_address %>% bind_cols(locations)
masterlist_address_geo <- masterlist_address_geo %>% 
  sf::st_as_sf(coords = c("lon","lat")) %>% 
  sf::st_set_crs(4326)

# Check to ensure that connection is open
arc.check_product()
arc.check_portal()
arc.portal_connect("https://cookcountyil.maps.arcgis.com/")

CC_CCVI_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CCVI_By_Muni/FeatureServer/0")
CC_CCVI_df <- arc.select(object = CC_CCVI_arc)
CC_CCVI_geom <- arc.data2sf(CC_CCVI_df)

CC_ZipCodes_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/addressZipCode/MapServer/1")
CC_ZipCodes_df <- arc.select(object = CC_ZipCodes_arc)
CC_ZipCodes_geom <- arc.data2sf(CC_ZipCodes_df)

CC_CensusTracts_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/ME_COVID_Tracts/FeatureServer/0")
CC_CensusTracts_df <- arc.select(object = CC_CensusTracts_arc)
CC_CensusTracts_geom <- arc.data2sf(CC_CensusTracts_df)

CC_Municipalities_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CCDPH_Muni_by_District/FeatureServer/0")
CC_Municipalities_df <- arc.select(object = CC_Municipalities_arc)
CC_Municipalities_geom <- arc.data2sf(CC_Municipalities_df)

CC_CommissionerDistricts_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/politicalBoundary/MapServer/1")
CC_CommissionerDistricts_df <- arc.select(object = CC_CommissionerDistricts_arc)
CC_CommissionerDistricts_geom <- arc.data2sf(CC_CommissionerDistricts_df)

CC_Border_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/plss/MapServer/1")
CC_Border_df <- arc.select(object = CC_Border_arc)
CC_Border_geom <- arc.data2sf(CC_Border_df)

# Transform datasets to same coordinate system, UTM Z16

# CC_ZipCodes_geom <- st_transform(CC_ZipCodes_geom, crs = 26916)

CC_Border_geom <- st_transform(CC_Border_geom, crs = 26916)
CC_Municipalities_geom <- st_transform(CC_Municipalities_geom, crs = 26916)
CC_CensusTracts_geom <- st_transform(CC_CensusTracts_geom, crs = 26916)
CC_CommissionerDistricts_geom <- st_transform(CC_CommissionerDistricts_geom, crs = 26916)
masterlist_address_geom <- st_transform(masterlist_address_geo,crs = 26916)

CC_CensusTracts_geom_sub <- CC_CensusTracts_geom %>%
  select(censustract = GEOID,
         cc_com_district = Comm_Dist)

CC_Municipalities_geom_sub <- CC_Municipalities_geom %>%
  select(cdph_district = DIST)

masterlist_address_geom <- masterlist_address_geom %>% st_join(CC_CensusTracts_geom_sub)
masterlist_address_geom <- masterlist_address_geom %>% st_join(CC_Municipalities_geom_sub)
masterlist_address_geom <- masterlist_address_geom %>% bind_cols(locations)
masterlist_address_geom <- masterlist_address_geom %>% mutate(street = toupper(street), city=toupper(city))
write_clip(masterlist_address_geom)

write_clip(vfinder)

ggplot() + geom_polygon(CC_Border_geom) + geom_point(masterlist_address_geom)
plot(CC_CensusTracts_geom['GEOID10'])

st_write(CC_Municipalities_geom,"CC_Municipalities_geom.shp")
st_write(masterlist_address_geom,"masterlist_address_geom.shp")
st_write(CC_Border_geom,"CC_Border_geom.shp")
st_write(CC_CensusTracts_geom,"CC_CensusTracts_geom.shp")
