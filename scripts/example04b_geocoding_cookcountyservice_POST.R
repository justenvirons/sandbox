## ---------------------------
##
## Script name: Geocoding in R using Cook County ArcGIS Rest API (POST request Method)
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
  sample_n(1000) %>%
  select(OBJECTID, SingleLine)

# create input json
addresses_json <- jsonlite::toJSON(list(records=addresses_full),flatten = T)
addresses_text <- addresses_json %>% 
  str_replace_all('\\{\\"OBJECTID\\"', '\\{\\"attributes\\":\\{\"OBJECTID\\"') %>% 
  str_replace_all('\\},\\{\\"attributes\\"','\\}\\},\\{\\"attributes\\"') %>%
  str_replace_all('\\}\\]\\}','\\}\\}\\]\\}')

# POST request
start_time <- Sys.time()
geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses"
addresses_json_rev <- rjson::fromJSON(addresses_text)
addresses_json_rev <- jsonlite::toJSON(addresses_json_rev, flatten=TRUE, auto_unbox = TRUE)
request_geo <- POST(url = geocoder_service,
           body = list(addresses=addresses_json_rev,f="json"),
           encode="form")
result_json <- content(request_geo,"parsed","application/json")

result_df <- data.frame()
for (i in seq_len(length(result_json$locations))){
  d <- with(result_json$locations[[i]], {data.frame(OBJECTID = attributes$ResultID,
                                            X = as.numeric(location$x),
                                            Y = as.numeric(location$y),
                                            score = score, 
                                            status = attributes$Status,
                                            address_match = attributes$Match_addr,
                                            side = attributes$Side,
                                            addressType = attributes$Addr_type)})
  result_df <- rbind(result_df, d)
}

end_time <- Sys.time()
print(end_time - start_time)

result_df <- result_df %>%
  left_join(addresses_full, by="OBJECTID") %>%
  rename(address_orig = SingleLine)

# results summary
result_df %>% group_by(status) %>% summarise(count=n(),
                                             mean_score = mean(score),
                                             min_score = min(score),
                                             max_score = max(score),
                                             sd_score = sd(score))

# clean up
rm(list=c('request_geo','result_json','addresses_full','addresses_json','addresses_json_rev','addresses_text','geocoder_service','i'))

