## ---------------------------
##
## Script name: Geocoding in R using Cook County ArcGIS Rest API (POST request Method, using street, zone as address input)
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-09-17
## Date Last Updated: 2021-10-01
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
library(httr)

# Background information concerning ArcGIS REST API geocoding services
# https://developers.arcgis.com/rest/geocode/api-reference/geocoding-geocode-addresses.htm
# https://github.com/cengel/ArcGIS_geocoding/blob/master/SUL_gcFunctions.R #great example

# Information concerning the Cook County ArcGIS REST API geocoding service
# https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer?f=pjson

# Download data with addresses
# Cook County Medical Examiner data
cc_medicalexaminercases <- 
  read.csv(file="https://datacatalog.cookcountyil.gov/api/views/cjeq-bs86/rows.csv?accessType=DOWNLOAD")

cc_provider_addresses <- read_csv("C:/Users/christopher.smith/OneDrive - Cook County Health/git_repos/justenvirons/vac_by_facility_shiny/data/icare_unique_providers.csv")
addresses_full <- cc_provider_addresses %>%
  drop_na(c(icare_clean_street,
            icare_zip)) %>%
  mutate(OBJECTID = row_number()) %>%
  select(STREET = icare_clean_street,
         ZONE = icare_zip,
         OBJECTID)

# create input json
addresses_json <- jsonlite::toJSON(list(records=addresses_full),flatten = T)
addresses_text <- addresses_json %>% 
  str_replace_all('\\{\\"STREET\\"', '\\{\\"attributes\\":\\{\"STREET\\"') %>% 
  str_replace_all('\\},\\{\\"attributes\\"','\\}\\},\\{\\"attributes\\"') %>%
  str_replace_all('\\}\\]\\}','\\}\\}\\]\\}')

# POST request
start_time <- Sys.time()
geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/geocodeAddresses"
addresses_json_rev <- rjson::fromJSON(addresses_text)
addresses_json_rev <- jsonlite::toJSON(addresses_json_rev, flatten=TRUE, auto_unbox = TRUE)
request_geo <- POST(url = geocoder_service,
                    body = list(addresses=addresses_json_rev,f="json", outSR=4326),
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

# format result dataframe, join with original addresses
result_df <- result_df %>%
  left_join(addresses_full, by="OBJECTID") %>%
  rename(street_orig = STREET,
         zone_orig = ZONE)

# results summary
result_df %>% group_by(status) %>% summarise(count=n(),
                                             mean_score = mean(score),
                                             min_score = min(score),
                                             max_score = max(score),
                                             sd_score = sd(score)) %>%
  mutate(pct = round(count/sum(count),3)*100) %>%
  arrange(desc(pct))

# clean up
rm(list=c('d','request_geo','result_json','addresses_full','addresses_json','addresses_json_rev','addresses_text','geocoder_service','i'))

