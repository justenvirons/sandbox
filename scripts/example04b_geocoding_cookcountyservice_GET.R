## ---------------------------
##
## Script name: Geocoding in R using Cook County ArcGIS Rest API (GET request Method)
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-09-17
## Date Last Updated: 2021-09-23
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# required packages 
library(dplyr)
library(tidyverse)
library(jsonlite)
library(urltools)

# Background information concerning ArcGIS REST API geocoding services
# https://developers.arcgis.com/rest/geocode/api-reference/geocoding-geocode-addresses.htm

# Information concerning the Cook County ArcGIS REST API geocoding service
# https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer?f=pjson

# Download data with addresses
# Cook County Medical Examiner data
cc_medicalexaminercases <- 
  read.csv(file="https://datacatalog.cookcountyil.gov/api/views/cjeq-bs86/rows.csv?accessType=DOWNLOAD")

# single line address geocoding ---------------------------------------------------
# format medical examiner data
# limited to 14 or so addresses with GET request

addresses_full <- cc_medicalexaminercases %>% 
  select(Incident.Address,
         Incident.City,
         Incident.Zip.Code,
         OBJECTID) %>%
  drop_na(c(Incident.Address,
            Incident.City,
            Incident.Zip.Code)) %>%
  filter(Incident.Address!="UNK",
         Incident.Address!="unknown",
         Incident.Address!="", 
         Incident.Zip.Code!="") %>%
  mutate(Incident.Address = toupper(gsub("[^[:alnum:]]", " ",Incident.Address)),
         Incident.Address = gsub("\\s+"," ",Incident.Address),
         SingleLine = paste0(Incident.Address," ",Incident.City,", IL ",Incident.Zip.Code)) %>%
  sample_n(10) %>%
  select(OBJECTID, SingleLine)

# create input json
addresses_json <- jsonlite::toJSON(list(records=addresses_full),flatten = TRUE) # use pretty=TRUE to view structured json format
addresses_text <- addresses_json %>% 
  str_replace_all('\\{\\"OBJECTID\\"', '\\{\\"attributes\\":\\{\"OBJECTID\\"') %>% 
  str_replace_all('\\},\\{\\"attributes\\"','\\}\\},\\{\\"attributes\\"') %>%
  str_replace_all('\\}\\]\\}','\\}\\}\\]\\}')

# GET request, subject to URL length restrictions
addresses_urlencoded <- url_encode(addresses_text)
geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses?addresses="
geocoder_options <- "&SingleLine=&f=pjson" #pjson, kmz or html
geocoder_call <- paste0(geocoder_service, addresses_urlencoded, geocoder_options)

returned_result <- fromJSON(file(geocoder_call), flatten = TRUE) %>% 
  .$locations %>% 
  select(address_out = address,
         score,
         OBJECTID = attributes.ResultID,
         X = attributes.X,
         Y = attributes.Y) %>%
  left_join(addresses_full, by="OBJECTID") %>%
  rename(address_in = SingleLine)

