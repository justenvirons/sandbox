## ---------------------------
##
## Script name: example06_spatialjoins.R
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-03-16
## Date Last Updated: 2021-03-25
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# install arcgisbinding
install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

library(arcgisbinding)
library(dplyr)
library(tidyverse)
library(ggmap)
library(sf)

# Check to ensure that connection is open
arc.check_product()
arc.check_portal()
arc.portal_connect("https://cookcountyil.maps.arcgis.com/")

# CCDPH Target Community
# https://cookcountyil.maps.arcgis.com/home/item.html?id=782950628c6c42a490f4b0989b07d1e0
CC_targetcommunities_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CCDPH_Target_Community/FeatureServer/0")
CC_targetcommunities_df <- arc.select(object = CC_targetcommunities_arc)
CC_targetcommunities_geom <- arc.data2sf(CC_targetcommunities_df)

# COVID-19 Community Vulnerability Index (CCVI) By Tract SURGO 
# https://cookcountyil.maps.arcgis.com/home/item.html?id=8a885b47a1c7426c8cd1ac2e0fee3cbc
CC_CCVI_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/ccvi_cook_tracts_2020_fc/FeatureServer/0")
CC_CCVI_df <- arc.select(object = CC_CCVI_arc)
CC_CCVI_geom <- arc.data2sf(CC_CCVI_df)

# Social Vulnerability Index (SVI) By Census Tract CDC 
# https://cookcountyil.maps.arcgis.com/home/item.html?id=f4cbc636c9c44b5f87183411de3157e0
CC_SVI_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/sviTractsIL/FeatureServer/0")
CC_SVI_df <- arc.select(object = CC_SVI_arc)
CC_SVI_geom <- arc.data2sf(CC_SVI_df)

# Zip Code Boundary USPS 
# https://cookcountyil.maps.arcgis.com/home/item.html?id=66657c0939db491eb258ccc38110f3b7
CC_ZipCodes_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/addressZipCode/MapServer/1")
CC_ZipCodes_df <- arc.select(object = CC_ZipCodes_arc)
CC_ZipCodes_geom <- arc.data2sf(CC_ZipCodes_df)

# Cook County Commissioner District 
# https://cookcountyil.maps.arcgis.com/home/item.html?id=9be915367e77412cafcef725beb32a5a
CC_CommissionerDistricts_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/politicalBoundary/MapServer/1")
CC_CommissionerDistricts_df <- arc.select(object = CC_CommissionerDistricts_arc)
CC_CommissionerDistricts_geom <- arc.data2sf(CC_CommissionerDistricts_df)

CC_CensusTracts_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/ME_COVID_Tracts/FeatureServer/0")
CC_CensusTracts_df <- arc.select(object = CC_CensusTracts_arc)
CC_CensusTracts_geom <- arc.data2sf(CC_CensusTracts_df)

CC_Municipalities_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CCDPH_Muni_by_District/FeatureServer/0")
CC_Municipalities_df <- arc.select(object = CC_Municipalities_arc)
CC_Municipalities_geom <- arc.data2sf(CC_Municipalities_df)

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

ggplot() + geom_polygon(CC_Border_geom) + geom_point(masterlist_address_geom)
plot(CC_CensusTracts_geom['GEOID10'])

st_write(CC_Municipalities_geom,"CC_Municipalities_geom.shp")
st_write(masterlist_address_geom,"masterlist_address_geom.shp")
st_write(CC_Border_geom,"CC_Border_geom.shp")
st_write(CC_CensusTracts_geom,"CC_CensusTracts_geom.shp")
