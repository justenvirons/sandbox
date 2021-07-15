

afilename <- paste0("C:/Users/scott/Desktop/delete/all providers list_3.31.21.xlsx")
providers <- read_excel(afilename, sheet="contour-export")

providers_a <- providers %>%
  mutate(address_full = paste0(address," ",city,", IL"," ", zip))

providers_a_ng <- providers_a_g %>% filter(is.na(lon))
  
providers_a_ng_c <- providers_a_ng %>%
  mutate(address_full = address_full,
         address_full = trimws(address_full),
         address_full = str_to_upper(address_full),
         address_full = str_replace_all(address_full, "\\.",""),
         address_full = str_replace_all(address_full, "/"," at "),
         address_full = str_replace_all(address_full, "#",""),
         address_full = str_replace_all(address_full, " BLDG "," "),
         address_full = str_replace_all(address_full, " RM "," "),
         address_full = str_replace_all(address_full, " ATTN "," "),
         address_full = str_replace_all(address_full, " APT "," "),
         address_full = str_replace_all(address_full, " FL "," "),
         address_full = str_replace_all(address_full, " STE "," "),
         address_full = str_replace_all(address_full, " UNIT "," "),
         address_full = str_replace_all(address_full, " TRLR "," "),
         address_full = str_replace_all(address_full, " LOT "," "),
         address_full = str_replace_all(address_full, "\\s\\d+\\s"," "),
         address_full = str_replace_all(address_full, "\\s[[:alpha:]]+[[:digit:]]+\\s", " "),
         address_full = str_replace_all(address_full, "\\s[[:digit:]]+[[:alpha:]]+\\s", " "),
         address_full = str_replace_all(address_full, "\\s\\d+\\s"," "),
         address_full = str_replace_all(address_full, "\\s+"," "),
         address_full = trimws(address_full))

register_google(key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
getOption("ggmap")
addresses = providers_a_ng_c$address_full
length(addresses) # check number of addresses
locations_2 <- geocode(addresses, key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
providers_a_g <- providers_a %>%  bind_cols(locations) %>% drop_na(lon)

providers_a_g_c <- providers_a_ng_c %>% select(-c(lon,lat)) %>% bind_cols(locations_2)

providers_a_g_c_b <- providers_a_g %>% bind_rows(providers_a_g_c)

providers_a_g <- providers_a_g_c_b

# Add reference geographies ---------------------------------
## Convert unique geocoded VaccineFinder provider list to sf table-----------------------------------------------------------------
coordinates(providers_a_g) = ~lon+lat
providers_a_g_sf = st_as_sf(providers_a_g)
st_crs(providers_a_g_sf) = 4326
providers_a_g_sf = st_transform(providers_a_g_sf, crs = 26916)

## Import reference data from AGO (requires that ArcGIS Pro is running, open and user logged into CC maps portal)----------------------------------------------------
# Cook County Commissioner District 
# https://cookcountyil.maps.arcgis.com/home/item.html?id=9be915367e77412cafcef725beb32a5a
CC_CommissionerDistricts_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/politicalBoundary/MapServer/1")
CC_CommissionerDistricts_df <- arc.select(object = CC_CommissionerDistricts_arc)
CC_CommissionerDistricts_sf <- arc.data2sf(CC_CommissionerDistricts_df)

# Zip Code Boundary USPS 
# https://cookcountyil.maps.arcgis.com/home/item.html?id=66657c0939db491eb258ccc38110f3b7
CC_ZipCodes_arc <- arc.open("https://gis12.cookcountyil.gov/arcgis/rest/services/addressZipCode/MapServer/1")
CC_ZipCodes_df <- arc.select(object = CC_ZipCodes_arc)
CC_ZipCodes_sf <- arc.data2sf(CC_ZipCodes_df)

# CCDPH Target Community
# https://cookcountyil.maps.arcgis.com/home/item.html?id=782950628c6c42a490f4b0989b07d1e0
CC_targetcommunities_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CCDPH_Target_Community/FeatureServer/0")
CC_targetcommunities_df <- arc.select(object = CC_targetcommunities_arc)
CC_targetcommunities_sf <- arc.data2sf(CC_targetcommunities_df)

# Cook County Municipalities
# https://cookcountyil.maps.arcgis.com/home/item.html?id=782950628c6c42a490f4b0989b07d1e0
CC_Municipalities_arc <- arc.open("https://services2.arcgis.com/I5Or36sMcO7Y9vQ3/arcgis/rest/services/CCDPH_Muni_by_District/FeatureServer/0")
CC_Municipalities_df <- arc.select(object = CC_Municipalities_arc)
CC_Municipalities_sf <- arc.data2sf(CC_Municipalities_df)

# Project reference layers to UTM Z16N
CC_targetcommunities_sf = st_transform(CC_targetcommunities_sf, crs = 26916)
CC_Municipalities_sf <- st_transform(CC_Municipalities_sf, crs = 26916)
CC_CommissionerDistricts_sf <- st_transform(CC_CommissionerDistricts_sf, crs = 26916)
CC_ZipCodes_sf <- st_transform(CC_ZipCodes_sf, crs = 26916)

# Subset reference layers
CC_targetcommunities_sf_sub <- CC_targetcommunities_sf %>%
  select(priority = CITY)

CC_Municipalities_sf_sub <- CC_Municipalities_sf %>%
  select(ccdph_district = DIST)

CC_CommissionerDistricts_sf_sub <- CC_CommissionerDistricts_sf %>%
  select(commissioner_district = District_1)

CC_ZipCodes_sf_sub <- CC_ZipCodes_sf %>%
  select(cc_zip = ZIP)

# Spatially join unique VaccineFinder provider list with reference geographies
providers_a_g_j_sf <- providers_a_g_sf %>% 
  st_join(CC_targetcommunities_sf_sub) %>%
  st_join(CC_Municipalities_sf_sub) %>%
  st_join(CC_ZipCodes_sf_sub) %>%
  st_join(CC_CommissionerDistricts_sf_sub) %>%
  mutate(zip = ifelse(is.na(zip),cc_zip,zip),
         priority = ifelse(is.na(priority),"NO","YES"),
         ccdph_district=case_when(ccdph_district=="N" ~ "North",
                                  ccdph_district=="S" ~ "South",
                                  ccdph_district=="SW" ~ "Southwest",
                                  ccdph_district=="W" ~ "West",
                                  ccdph_district=="O" ~ "Outside",
                                  is.na(ccdph_district) ~ "Outside")) %>%
  select(-cc_zip) %>%
  st_drop_geometry()

providers_a_g_c_b_ll <- providers_a_g_c_b %>% select(provider_number,lon,lat)
providers_a_g_j_sf_ll <- providers_a_g_j_sf %>% left_join(providers_a_g_c_b_ll,by="provider_number")

write_csv(providers_a_g_j_sf_ll, "C:/Users/scott/Desktop/delete/master_provider_list_20210331.csv")
