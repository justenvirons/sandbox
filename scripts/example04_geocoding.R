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
library(ggmap)

# This method is good when 
# https://www2.census.gov/geo/tiger/TIGER2020/ADDRFEAT/

# Download data with addresses
# Here are two examples of very large datasets with addresses that are, by the
# way, already geocoded
# Cook County Medical Examiner data and Chicago 311 requests
cc_medicalexaminercases <- 
  read.csv(file="https://datacatalog.cookcountyil.gov/api/views/cjeq-bs86/rows.csv?accessType=DOWNLOAD")
requestsChicago311 <- 
  read.csv(file="https://data.cityofchicago.org/api/views/v6vf-nfxy/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

# example geocoding medical examiner data 
addresses_full <- cc_medicalexaminercases %>% 
  drop_na(c(Incident.Address, Incident.City, Incident.Zip.Code, longitude, latitude)) %>%
  filter(Incident.Address!="UNK", Incident.Address!="", Incident.City!="", Incident.Zip.Code!="") %>%
  select(case=Case.Number, street=Incident.Address, city=Incident.City, zip=Incident.Zip.Code, state="IL", cc_x = longitude, cc_y=latitude)

addresses_full_formatted <- key %>%
  mutate(zip5 = case_when(nchar(trimws(zip))==5 ~ trimws(zip),
                          nchar(trimws(zip))==6 ~ trimws(str_replace(zip,"-","")),
                          nchar(trimws(zip))==10 ~ substr(trimws(zip),1,5)),
         FullAddress = paste0(street," ",city,", ",state," ",zip5),
         FullAddressFormatted = trimws(FullAddress) ,
         FullAddressFormatted = str_to_upper(FullAddressFormatted) ,
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "\\.",""),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "/"," at "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "#",""),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " BLDG "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " RM "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " ATTN "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " APT "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " FL "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " STE "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " UNIT "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " TRLR "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, " LOT "," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "\\s\\d+\\s"," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "\\s[[:alpha:]]+[[:digit:]]+\\s", " "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "\\s[[:digit:]]+[[:alpha:]]+\\s", " "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "\\s\\d+\\s"," "),
         FullAddressFormatted = str_replace_all(FullAddressFormatted, "\\s+"," "),
         FullAddressFormatted = trimws(FullAddressFormatted) )

# request a Google API key via the following URL
# https://developers.google.com/maps/documentation/embed/get-api-key
register_google(key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
getOption("ggmap")
addresses_full <- masterlist %>%
  mutate(FullAddressFormatted = toupper(paste0(street," ",city,", IL ",zip)))
addresses = addresses_full$FullAddressFormatted
length(addresses) # check number of addresses
locations <- geocode(addresses, key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
locations <- cbind(addresses_full_formatted,locations)
masterlist <- cbind(masterlist,locations)

write_csv(masterlist,paste0(aPathName, "provider_masterlist_geo.csv"))
