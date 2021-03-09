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

setwd("C:/Users/scott/OneDrive - CCDPH/OneDrive - Cook County Health/Projects")  

library(arcgisbinding)
library(dplyr)
arc.check_product()
arc.portal_connect("https://cookcountyil.maps.arcgis.com")
arc.check_portal()
test <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CensusACS2019Age/FeatureServer/0")
test@extent
test@fields
test.dataframe <- arc.select(object = test, fields = c("FID", "COUNTYFP10", "SHAPE", "TOTALPOP"))
arc.portal_connect("https://depaul-edu.maps.arcgis.com")
test2 <- arc.open("https://services7.arcgis.com/8kZv9DESIQ1hYuyJ/arcgis/rest/services/20201117_RushPartnerChurches/FeatureServer/0")
test2@fields

test3 <- test2 %>% 
