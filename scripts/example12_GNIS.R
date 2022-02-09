## ---------------------------
##
## Script name: 
## Purpose of script: create routes file
## Author: C. Scott Smith, PhD AICP
## Date Created: 2022-02-09
## Date Last Updated: 2022-02-09
## Email: c.scott.smith@depaul.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# activate packages
# may need to install first using commented line below
# install.packages(c("tidyverse","dplyr","sf","readxl"))

library(tidyverse)
library(dplyr) 
library(sf)
library(readxl)

# download gnis file
# https://www.usgs.gov/u.s.-board-on-geographic-names/download-gnis-data
# https://geonames.usgs.gov/docs/stategaz/NationalFile.zip

# import comprehensive dataset into R (change file path to where you saved file)
gnis_2021 <- read_delim("data/NationalFile_20210825.txt", delim="|")

# format gnis dataset to include places only, needed fields
gnis_places_2021 <- gnis_2021 %>% 
  filter(FEATURE_CLASS == "Populated Place") %>%
  select(place_name = FEATURE_NAME,
         state_name = STATE_ALPHA,
         latitude = PRIM_LAT_DEC,
         longitude = PRIM_LONG_DEC) %>%
  mutate(place_state = paste0(place_name,", ",state_name))

# import dataset provided by chaddick
routes <- read_excel("data/routes.xlsx", sheet="routes")

# join route origin and destination coordinates with gnis dataset
# (note that you will need to remove NAs from dataset before using XY to Line tool)
routes_xy <- routes %>% 
  left_join(gnis_places_2021 %>% 
              select(Origin_Lat = latitude,
                     Origin_Lon = longitude, 
                     place_state),
            by=c("Origin"="place_state")) %>%
  left_join(gnis_places_2021 %>% 
              select(Dest_Lat = latitude,
                     Dest_Lon = longitude, 
                     place_state),
            by=c("Destination"="place_state")) 

# write output as csv to data directory
write_csv(routes_xy,"data/routes_xy.csv")  

