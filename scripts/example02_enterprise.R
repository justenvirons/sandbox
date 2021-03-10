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

arc.check_product()
arc.check_portal()
arc.portal_connect("https://depaul-edu.maps.arcgis.com")
example_data <- arc.open("https://services7.arcgis.com/8kZv9DESIQ1hYuyJ/arcgis/rest/services/COVID_19_cases_deaths_and_hospitalizations_and_related_demographic_data/FeatureServer/0")
example_data # data summary
example_data@shapeinfo # geometry, projection info only 
example_data@extent # bounding box
example_data.dataframe <- arc.select(object = example_data)
class(example_data.dataframe) 

# Load income data for dissemination areas, and join by the csd geo_code the boundary data frame:


