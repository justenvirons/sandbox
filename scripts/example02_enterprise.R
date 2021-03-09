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




arc.check_product()
arc.check_portal()
arc.portal_connect("https://cookcountyil.maps.arcgis.com")
example_data <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/ccdph_counties/FeatureServer/0")
example_data # data summary
example_data@shapeinfo # geometry, projection info only 
example_data@extent # bounding box
example_data.dataframe <- arc.select(object = example_data, fields = c("FID", "COUNTYFP", "SHAPE", "ALAND"))
class(example_data.dataframe) 

# Load income data for dissemination areas, and join by the csd geo_code the boundary data frame:


