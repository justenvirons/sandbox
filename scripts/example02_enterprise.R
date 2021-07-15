## ---------------------------
##
## Script name: Using ArcGIS R Bridge (arcgisbinding package)
## Purpose of script: Access CCDPH feature services via R
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-03-05
## Date Last Updated: 
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------

install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

library(arcgisbinding)
library(dplyr)
library(ineq)
library(lctools)
library(fuzzyjoin)
library(sqldf)

library(arcgisbinding)
arc.check_product()
arc.check_portal()

# Zip Code Boundaries 
CC_ZipCodes_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/addressZipCode/MapServer/1")
CC_ZipCodes_df <- arc.select(object = CC_ZipCodes_arc)
CC_ZipCodes_sf <- arc.data2sf(CC_ZipCodes_df)


arc.portal_connect("https://cookcountyil.maps.arcgis.com")
arc.portal_connect("https://depaul-edu.maps.arcgis.com")
example_data <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CCDPH_Target_Community/FeatureServer/0")

example_data # data summary
example_data@shapeinfo # geometry, projection info only 
example_data@extent # bounding box
example_data.dataframe <- arc.select(object = example_data)
example_data.dataframe_sf <- arc.data2sf(example_data.dataframe)

# Transformed datasets

# Zip Code Boundaries 
CC_ZipCodes_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/addressZipCode/MapServer/1")
CC_ZipCodes_df <- arc.select(object = CC_ZipCodes_arc)
CC_ZipCodes_sf <- arc.data2sf(CC_ZipCodes_df)



zip_codes 
CC_CommissionerDistricts_geom <- st_transform(example_data.dataframe_sf, crs = 26916)

masterlist_address_geom <- st_transform(masterlist_address_geo,crs = 26916)


# Load income data for dissemination areas, and join by the csd geo_code the boundary data frame:


