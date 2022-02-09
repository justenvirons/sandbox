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

library(tigris)
library(sf)
library(arcgisbinding)
library(dplyr)
library(ineq)
library(lctools)
library(fuzzyjoin)
library(sqldf)

library(arcgisbinding)
arc.check_product()
arc.check_portal()

# vaccine providers
SCC_providers_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/vaccineproviders091721/FeatureServer/0")
SCC_providers_df <- arc.select(object = SCC_providers_arc)
SCC_providers_sf <- arc.data2sf(SCC_providers_df)

SCC_providers_sf_sub <- SCC_providers_sf %>%
  select(OBJECTID, 
         out_add = Match_addr,
         in_street = USER_street,
         city=USER_city,
         zip = USER_zip,
         name = USER_site_name,
         type = USER_site_type,
         pharm = USER_Pharmacy_type) %>%
  st_transform(crs=3435)

st_write(SCC_providers_sf_sub,"E:/OneDrive - Cook County Health/git_repos/justenvirons/CCDPH-vaccine-mgmt/layers/SCC_covid_vaccine_providers_20210930.shp", append = FALSE)
st_crs(SCC_providers_sf_sub)

ccdph_districts <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/CCDPH-vaccine-mgmt/layers/ccdph_districts_boundaries.shp")
st_crs(ccdph_districts)

ccdph_districts_by_muni <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/CCDPH-vaccine-mgmt/layers/ccdph_districts_boundaries_muni_dissolved.shp")
st_crs(ccdph_districts_by_muni)

ccdph_districts_by_muni_sub_4326 <- ccdph_districts_by_muni %>%
  select(district=location) %>%
  mutate(sqmi=set_units(st_area(geometry),mi^2)) %>%
  st_transform(crs=4326)

st_write(ccdph_districts_by_muni_sub_3435, "E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/epsg-3435-illinois-stateplane-east/ccdph_districts_boundaries_muni_epsg3435.shp")

ccdph_districts_by_muni_sub_3435 <- ccdph_districts_by_muni %>%
  select(district=location) %>%
  mutate(sqmi=set_units(st_area(geometry),mi^2))

st_write(ccdph_districts_by_muni_sub_4326, "E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/epsg-4326-wgs84-for-leaflet/ccdph_districts_boundaries_muni_epsg4326.shp")

library(units)

SCC_providers_sf_sub_district <- SCC_providers_sf_sub %>% st_join(ccdph_districts)

SCC_providers_sf_sub_district %>%
  st_drop_geometry() %>%
  group_by(district) %>%
  summarise(n())


# Zip Code Boundaries 
CC_ZipCodes_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/addressZipCode/MapServer/1")
CC_ZipCodes_df <- arc.select(object = CC_ZipCodes_arc)
CC_ZipCodes_sf <- arc.data2sf(CC_ZipCodes_df)

st_write(CC_ZipCodes_sf,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/zips_usps.shp")

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

IL_ZCTAs_geom <- zctas(cb=TRUE, class="sf")
st_write(IL_ZCTAs_geom,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/zctas.shp", append=FALSE)

zip_codes 
CC_CommissionerDistricts_geom <- st_transform(example_data.dataframe_sf, crs = 26916)

masterlist_address_geom <- st_transform(masterlist_address_geo,crs = 26916)


# Load income data for dissemination areas, and join by the csd geo_code the boundary data frame:


