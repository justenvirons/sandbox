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

## set working directory for Mac and PC

library(dplyr)
library(tidyverse)
library(ggmap)

Districts_raw <- read_csv("https://raw.githubusercontent.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/main/Districts.csv")
Addresses_raw <- read_csv("https://raw.githubusercontent.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/main/Healthcare%20Facility%20Addresses.csv")

Addresses <- Addresses_raw %>% 
  select(Facility=facility, Address=street, City=city, Zip=zip) %>% 
  mutate(FullAddress = paste(Address, " ", City,", IL ",Zip, sep=""))

Addresses$FullAddressFormatted <- str_to_lower(Addresses$FullAddress)
Addresses <- Addresses %>% mutate(FullAddressFormatted = trimws(FullAddressFormatted))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "#"," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " apt* "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " ste* "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " unit* "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " trlr "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " lot "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s\\d*\\s"," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s[:alpha:][:digit:]\\s", " "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s\\d\\s", " "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s[:digit:][:alpha:]\\s", " "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = trimws(FullAddressFormatted))

register_google(key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
getOption("ggmap")
addresses = Addresses$FullAddressFormatted
length(addresses) # check number of addresses
locations <- geocode(addresses, key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
locations <- cbind(Addresses,locations)

# CC MedicalExaminer data
cc_medicalexaminercases <- read.csv("https://datacatalog.cookcountyil.gov/api/views/cjeq-bs86/rows.csv?accessType=DOWNLOAD")

Addresses <- cc_medicalexaminercases %>% 
  drop_na(c(Incident.Address, Incident.City, Incident.Zip.Code, longitude, latitude)) %>%
  filter(Incident.Address!="UNK", Incident.Address!="", Incident.City!="", Incident.Zip.Code!="") %>%
  select(case=Case.Number, Address=Incident.Address, City=Incident.City, Zip=Incident.Zip.Code, cc_x = longitude, cc_y=latitude) %>% 
  mutate(FullAddress = paste(Address, " ", City,", IL ",Zip, sep=""))

Addresses$FullAddressFormatted <- str_to_lower(Addresses$FullAddress)
Addresses <- Addresses %>% mutate(FullAddressFormatted = trimws(FullAddressFormatted))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "#"," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " apt* "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " ste* "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " unit* "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " trlr "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, " lot "," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s\\d*\\s"," "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s[:alpha:][:digit:]\\s", " "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s\\d\\s", " "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = str_replace(FullAddressFormatted, "\\s[:digit:][:alpha:]\\s", " "))
Addresses <- Addresses %>% mutate(FullAddressFormatted = trimws(FullAddressFormatted))

write.csv(Addresses, "C:/Users/scott/OneDrive - DePaul University/PROJECTS/COVID/GIS/Layers/Geocode/cc_medicalexamineraddresses.csv")
