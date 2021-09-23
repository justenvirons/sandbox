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


# Download data with addresses
# Cook County Medical Examiner data
cc_medicalexaminercases <- 
  read.csv(file="https://datacatalog.cookcountyil.gov/api/views/cjeq-bs86/rows.csv?accessType=DOWNLOAD")

# format medical examiner data
addresses_full <- cc_medicalexaminercases %>% 
  tibble() %>%
  select(Incident.Address, 
         Incident.Zip.Code,
         OBJECTID) %>%
  drop_na(c(Incident.Address,
            Incident.Zip.Code)) %>%
  filter(Incident.Address!="UNK",
         Incident.Address!="unknown",
         Incident.Address!="", 
         Incident.Zip.Code!="") %>%
  mutate(Incident.Address = toupper(str_replace_all(Incident.Address,"\\,",""))) %>%
  sample_n(10) %>%
  rename(STREET=Incident.Address, 
         ZONE=Incident.Zip.Code) 

# create input json
addresses_json <- jsonlite::toJSON(list(records=addresses_full),flatten = T)
addresses_text <- addresses_json %>% 
  str_replace_all('\\{\\"STREET\\"', '\\{\\"attributes\\":\\{\"STREET\\"') %>% 
  str_replace_all('\\},\\{\\"attributes\\"','\\}\\},\\{\\"attributes\\"') %>%
  str_replace_all('\\}\\]\\}','\\}\\}\\]\\}')

#   
geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses?addresses="
geocoder_options <- "&category=&sourceCountry=&matchOutOfRange=true&langCode=&locationType=&searchExtent=&outSR=&f=pjson"

geocoder_call <- paste0(geocoder_service, addresses_text, geocoder_options)

returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates


geocodeurl <- URLencode(paste0("https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses?addresses=",
                               addresses_full_text,
                               "&category=&sourceCountry=&matchOutOfRange=true&langCode=&locationType=&searchExtent=&outSR=&f=pjson"))

write_clip(URLencode(addresses_full_text))





cat(addresses_full_json)
addresses_full_text <- jsonlite::fromJSON(addresses_full_json, simplifyDataFrame = TRUE)
