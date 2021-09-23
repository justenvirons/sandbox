
#### Loading packages ####

library(tidyverse)
library(jsonlite)



#### Sample calls to show results ####

#Sample strings
sample_string_lite <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates?Street=7556 W Jackson&City=Forest Park&ZIP=60130&outSR=3435&SingleLine=&outFields=User_fld&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
sample_string_full <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates?Street=7556 W Jackson&City=Forest Park&ZIP=60130&outSR=3435&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
sample_string_null <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates?Street=7000 W Jackson&City=Timbuktu&ZIP=60103&outSR=3435&SingleLine=&outFields=outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"

#Sample calls
fromJSON(file(sample_string_lite))  #only return most essential fields
fromJSON(file(sample_string_full))  #return all available fields
fromJSON(file(sample_string_null))  #see what return looks like for a junk address



#### Draft multiple field functions -- THESE ASSUME ADDRESS DATA IS STORED IN SEPARATE COLUMNS ####

return_mailing_address <- function(street, city, zip, wkid = 3435) {
  
  geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates"
  geocoder_options <- "&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, 
                          "?Street=", street, "&City=", city, "&ZIP=", zip, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(returned_result$address)
  }
  
}

return_x <- function(street, city, zip, wkid = 3435) {

  geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates"
  geocoder_options <- "&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, 
                          "?Street=", street, "&City=", city, "&ZIP=", zip, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(returned_result$location[["x"]])
  }
  
}

return_y <- function(street, city, zip, wkid = 3435) {
  
  geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates"
  geocoder_options <- "&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, 
                          "?Street=", street, "&City=", city, "&ZIP=", zip, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(returned_result$location[["y"]])
  }
  
}

return_score <- function(street, city, zip, wkid = 3435) {

  geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates"
  geocoder_options <- "&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, 
                          "?Street=", street, "&City=", city, "&ZIP=", zip, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(returned_result$score)
  }
  
}

return_residence_city <- function(street, city, zip, wkid = 3435) {
  
  geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates"
  geocoder_options <- "&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, 
                          "?Street=", street, "&City=", city, "&ZIP=", zip, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(returned_result$attributes[["User_fld"]])
  }
  
}

#NOTE: This function is unlikely to be needed long term but I wanted it for initial quality checks
return_address_type <- function(street, city, zip, wkid = 3435) {
  
  geocoder_service <- "https://gisinternal.cookcountyil.gov/secure/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates"
  geocoder_options <- "&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, 
                          "?Street=", street, "&City=", city, "&ZIP=", zip, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(returned_result$attributes[["Addr_type"]])
  }
  
}



#### Draft single field functions -- THESE ASSUME ADDRESS DATA IS STORED IN ONE COLUMN ####
#Note only have one of these written so far

return_mailing_address_sl <- function(address, wkid = 3435) {
  
  geocoder_service <- "https://gis7.cookcountyil.gov/arcgis/rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates?Street=&City=&SingleLine="
  geocoder_options <- "&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, address, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call)) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(returned_result$address)
  }
  
}



#### CODE EXAMPLES ####

#Note: functions above are not vectorized so if using them in the tidyverse, they must be used with either rowwise() or map() functions
# using purrr map functions seems faster in speed tests and is cleaner code

#test dataset: first address is junk, second is mis-classified third and fourth are unincorporated 
test_address <- data.frame(street = c("7000 W Jackson", "2434 Westover Ave", "8667 Josephine Lane", "10959 WELLINGTON AVENUE"), 
                           city = c("Timbuktu", "Riverside", "Des Plaines", "Melrose Park"), 
                           zip = c(60067, 60546, 60016, 60164))

#multiple field example where columns are the same name as function arguments 
test_call_pmap <- test_address %>%
  mutate(geocoded_address = pmap_chr(., .f = return_mailing_address))

#single field example
test_call_map <- test_address %>%
  mutate(address = paste(street, city, zip, sep = ", "),
         geocoded_address = map_chr(address, return_mailing_address_sl)) 

#multiple field example where columns are NOT the same name as function arguments 
test_address_pmap <- data.frame(address1 = c("7000 W Jackson", "2434 Westover Ave", "8667 Josephine Lane", "10959 WELLINGTON AVENUE"), 
                                city = c("Timbuktu", "Riverside", "Des Plaines", "Melrose Park"), 
                                zip_code = c(60067, 60546, 60016, 60164))

test_call_pmap_2 <- test_address_pmap %>%
  mutate(geocoded_address = pmap_chr(list(street = address1, city = city, zip = zip_code), .f = return_mailing_address))


#rowwise example but map/pmap preferred
# test_call <- test_address %>%
#   #slice(2) %>%
#   rowwise() %>%
#   mutate(geocoded_address = return_mailing_address(street, city, zip),
#          geocoded_x = return_x(street, city, zip),
#          geocoded_y = return_y(street, city, zip),
#          geocoded_score = return_score(street, city, zip),
#          geocoded_city = return_residence_city(street, city, zip),
#          address_type = return_address_type(street,city,zip)) %>%
#   ungroup()




