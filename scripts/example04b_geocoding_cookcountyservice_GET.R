## ---------------------------
##
## Script name: Geocoding in R vs ArcGIS
## Purpose of script:
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

library(dplyr)
library(tidyverse)
library(jsonlite)

# Background information concerning ArcGIS REST API geocoding services
# https://developers.arcgis.com/rest/geocode/api-reference/geocoding-geocode-addresses.htm

# Information concerning the Cook County ArcGIS REST API geocoding service
# https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer?f=pjson

# Download data with addresses
# Cook County Medical Examiner data
cc_medicalexaminercases <- 
  read.csv(file="https://datacatalog.cookcountyil.gov/api/views/cjeq-bs86/rows.csv?accessType=DOWNLOAD")

# street-zone geocoding ---------------------------------------------------

# format medical examiner data
# street, zone format
addresses_full <- cc_medicalexaminercases %>% 
  select(Incident.Address, 
         Incident.Zip.Code,
         OBJECTID) %>%
  drop_na(c(Incident.Address,
            Incident.Zip.Code)) %>%
  filter(Incident.Address!="UNK",
         Incident.Address!="unknown",
         Incident.Address!="", 
         Incident.Zip.Code!="") %>%
  mutate(Incident.Address = toupper(gsub("[^[:alnum:]]", " ",Incident.Address)),
         Incident.Address = gsub("\\s+"," ",Incident.Address)) %>%
  sample_n(25) %>%
  rename(STREET=Incident.Address, 
         ZONE=Incident.Zip.Code) 

# create input json
addresses_json <- jsonlite::toJSON(list(records=addresses_full),flatten = T)
addresses_text <- addresses_json %>% 
  str_replace_all('\\{\\"STREET\\"', '\\{\\"attributes\\":\\{\"STREET\\"') %>% 
  str_replace_all('\\},\\{\\"attributes\\"','\\}\\},\\{\\"attributes\\"') %>%
  str_replace_all('\\}\\]\\}','\\}\\}\\]\\}')

addresses_urlencoded <- url_encode(addresses_text)
geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses?addresses="
geocoder_options <- "&f=pjson"
geocoder_call <- paste0(geocoder_service, addresses_urlencoded, geocoder_options)

returned_result <- fromJSON(file(geocoder_call)) %>% .$locations

# single line address geocoding ---------------------------------------------------
# format medical examiner data
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
addresses_json <- jsonlite::toJSON(list(records=addresses_full),flatten = T)
addresses_text <- addresses_json %>% 
  str_replace_all('\\{\\"OBJECTID\\"', '\\{\\"attributes\\":\\{\"OBJECTID\\"') %>% 
  str_replace_all('\\},\\{\\"attributes\\"','\\}\\},\\{\\"attributes\\"') %>%
  str_replace_all('\\}\\]\\}','\\}\\}\\]\\}')

# GET request, subject to URL length restrictions
addresses_urlencoded <- url_encode(addresses_text)
geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses?addresses="
geocoder_options <- "&SingleLine=&f=pjson" #pjson, kmz or html
geocoder_call <- paste0(geocoder_service, addresses_urlencoded, geocoder_options)

# POST request
# https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses

geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses?&SingleLine=&f=pjson"

addresses_json_rev <- rjson::fromJSON(addresses_text)
addresses_json_rev <- jsonlite::toJSON(addresses_json_rev, flatten=TRUE, auto_unbox = TRUE)
test_result <- httr::POST(url = geocoder_service,
           body =  jsonlite::toJSON(addresses_json_rev, flatten=TRUE, auto_unbox = TRUE),
           httr::content_type_json(),
           verbose())

http_type(test_result)
test_content <- fromJSON(test_result)

returned_result <- fromJSON(file(geocoder_call), flatten = TRUE) %>% 
  .$locations %>% 
  select(address_out = address,
         score,
         OBJECTID = attributes.ResultID,
         X = attributes.X,
         Y = attributes.Y) %>%
  left_join(addresses_full, by="OBJECTID") %>%
  rename(address_in = SingleLine)

