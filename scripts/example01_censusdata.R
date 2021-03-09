# Name: ACS download and geometry apportionment 

# Purpose: Downloads TIGER/Line files and ACS attribute data. Joins TIGER/Line
# geometries with attribute data. Reapportions attribute data based on area of
# overlap between two geometries (e.g., municipalities and census tracts).
#
# Author: C. Scott Smith 
# Organization: Cook County Department of Public Health
# Contact Email: christopher.smith@cookcountyhealth.org
# 
# Date Created: 2/28/2020 
# Last Updated: 2/28/2020 

# Activate packages
library(censusapi)
library(tidyverse)
library(dplyr) 
library(tigris) 
library(sf) 
library(clipr)
library(xlsx)
library(leaflet)
library(scales)
library(ggmap)

CookCountyMunisByDistrict_sub <- read.csv("data/jurisdictionbymuni.csv") %>% mutate(place=as.character(place)) %>% select(GEOID_place = place, muni, label, district, partial) 

# Download  census tract geographies
# cb = cartographic boundary
# Project both to UTM North Zone 16
IL_Tracts_geom <- tracts("IL", cb=TRUE, class="sf")
IL_Places_geom <- places("IL", cb=TRUE, class="sf")
IL_Counties_geom <- counties("IL", cb=TRUE, class="sf")
IL_Tracts_geom <- st_transform(IL_Tracts_geom, crs = 26916)
IL_Places_geom <- st_transform(IL_Places_geom, crs = 26916)
IL_Counties_geom <- st_transform(IL_Counties_geom, crs = 26916)

# Filter out only Cook County census tracts
# Intersect the place and tract geometries and create a areal weight field, AREA_pct
CookCounty_Tracts_geom <- IL_Tracts_geom %>% filter(COUNTYFP=="031")
CookCounty_Tracts_geom <- CookCounty_Tracts_geom %>% select(GEOID_tract = GEOID) %>% mutate(AREA_tract = st_area(geometry))
IL_Places_geom <- IL_Places_geom %>% select(GEOID_place=GEOID,NAME_place=NAME) %>% mutate(AREA_place = st_area(geometry))
CookCounty_TractsByPlace_geom_int <- st_intersection(CookCounty_Tracts_geom,IL_Places_geom)
CookCounty_TractsByPlace_geom <- CookCounty_TractsByPlace_geom_int %>% mutate(AREA_int = st_area(geometry), AREA_pct = AREA_int/AREA_tract)
CookCounty_TractsByPlace_geom_wgs <- st_transform(CookCounty_TractsByPlace_geom, crs = 4326)
CookCounty_TractsByPlace_geom_wgs <- st_cast(CookCounty_TractsByPlace_geom_wgs$geometry,"POLYGON")

plot(CookCounty_TractsByPlace_geom_wgs['GEOID_place'])

# attach(CookCounty_TractsByPlace_geom_wgs)
# muni_popup <- paste("<strong>Municipality and Tract: </strong>", NAME_place,":",GEOID_tract,"<br><strong>Area Total (% overlap): </strong>", format(AREA_tract*0.00000038610,digits=2),"square miles",sep="")
# CookCounty_TractsByPlace_geom_wgs %>%
#   addPolygons() %>%
#   leaflet() %>% 
#   # addPolygons(stroke = FALSE, fillColor = ~pal_fun(AREA_pct), fillOpacity = 0.5, smoothFactor = 0.5, popup = muni_popup) %>%
#   # addLegend(pal = pal_fun, values = AREA_tract, opacity = 0.7, title = NULL, position = "bottomright") %>%
#   addTiles()
# detach(CookCounty_TractsByPlace_geom_wgs)

# Download complete list of ACS 5-year tables
ayear <- "2019"
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5",
  # name = "cbp",
  vintage = ayear,
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste(ayear,"_grouptable",sep=""),acs_groups_tables) # change name of dataframe

# Variables for ACS data table
# B01001: SEX BY AGE
ayear <- "2019"
agroup <- "B01001"
acs_groups_vars <- listCensusMetadata(
  name = "acs/acs5",
  vintage = ayear,
  group = agroup,
  type = "variables")
acs_groups_vars$year<-ayear
acs_groups_vars <- acs_groups_vars %>% filter(!str_detect(name,"EA"),!str_detect(name,"M"))
assign(paste("groupvars_",agroup,"_",ayear, sep=""),acs_groups_vars)
rm(acs_groups_vars)
# 
# write_clip(groupvars_B01001_2019)

# for (i in 1:40) {
#   detach(2)
# }

# List of ACS tables to be downloaded
grouplist <- c("B01001","B01001A","B01001B","B01001D","B01001H","B01001I")

# Download places
yearlist <- c(2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "place:*", # tracts
                           regionin="state:17", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group$year<-ayear 
    acs_group$GEOID_place<-paste0(state,place)
    assign(paste(agroup,"place",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

# Note that table names are identical across geometries
# Summarize age categories for census tract tables

B01001_place_2019 <- B01001_place_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001_001E),
  PopUnd5 = sum(B01001_003E,B01001_027E),
  Pop5to17 = sum(c_across(B01001_004E:B01001_006E),c_across(B01001_028E:B01001_030E)),
  Pop18to29 = sum(c_across(B01001_007E:B01001_011E),c_across(B01001_031E:B01001_035E)),
  Pop30to44 = sum(c_across(B01001_012E:B01001_014E),c_across(B01001_036E:B01001_038E)),
  Pop45to54 = sum(c_across(B01001_015E:B01001_016E),c_across(B01001_039E:B01001_040E)),
  Pop55to64 = sum(c_across(B01001_017E:B01001_019E),c_across(B01001_041E:B01001_043E)),
  Pop65to74 = sum(c_across(B01001_020E:B01001_022E),c_across(B01001_044E:B01001_046E)),
  Pop75to84 = sum(c_across(B01001_023E:B01001_024E),c_across(B01001_047E:B01001_048E)),
  Pop85Over = sum(B01001_025E,B01001_049E),
)

B01001_place_2019_sub <- B01001_place_2019 %>% select(GEOID_place, Total:Pop85Over) %>% mutate(type="All")

B01001A_place_2019 <- B01001A_place_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001A_001E),
  PopUnd5 = sum(B01001A_003E,B01001A_018E),
  Pop5to17 = sum(c_across(B01001A_004E:B01001A_006E),c_across(B01001A_019E:B01001A_021E)),
  Pop18to29 = sum(c_across(B01001A_007E:B01001A_009E),c_across(B01001A_022E:B01001A_024E)),
  Pop30to44 = sum(c_across(B01001A_010E:B01001A_011E),c_across(B01001A_025E:B01001A_026E)),
  Pop45to54 = sum(c_across(B01001A_012E),c_across(B01001A_027E)),
  Pop55to64 = sum(c_across(B01001A_013E),c_across(B01001A_028E)),
  Pop65to74 = sum(c_across(B01001A_014E),c_across(B01001A_029E)),
  Pop75to84 = sum(c_across(B01001A_015E),c_across(B01001A_030E)),
  Pop85Over = sum(B01001A_016E,B01001A_031E),
)

B01001A_place_2019_sub <- B01001A_place_2019 %>% select(GEOID_place, Total:Pop85Over) %>% mutate(type="White")

B01001B_place_2019 <- B01001B_place_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001B_001E),
  PopUnd5 = sum(B01001B_003E,B01001B_018E),
  Pop5to17 = sum(c_across(B01001B_004E:B01001B_006E),c_across(B01001B_019E:B01001B_021E)),
  Pop18to29 = sum(c_across(B01001B_007E:B01001B_009E),c_across(B01001B_022E:B01001B_024E)),
  Pop30to44 = sum(c_across(B01001B_010E:B01001B_011E),c_across(B01001B_025E:B01001B_026E)),
  Pop45to54 = sum(c_across(B01001B_012E),c_across(B01001B_027E)),
  Pop55to64 = sum(c_across(B01001B_013E),c_across(B01001B_028E)),
  Pop65to74 = sum(c_across(B01001B_014E),c_across(B01001B_029E)),
  Pop75to84 = sum(c_across(B01001B_015E),c_across(B01001B_030E)),
  Pop85Over = sum(B01001B_016E,B01001B_031E),
)

B01001B_place_2019_sub <- B01001B_place_2019 %>% select(GEOID_place, Total:Pop85Over) %>% mutate(type="Black")

B01001D_place_2019 <- B01001D_place_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001D_001E),
  PopUnd5 = sum(B01001D_003E,B01001D_018E),
  Pop5to17 = sum(c_across(B01001D_004E:B01001D_006E),c_across(B01001D_019E:B01001D_021E)),
  Pop18to29 = sum(c_across(B01001D_007E:B01001D_009E),c_across(B01001D_022E:B01001D_024E)),
  Pop30to44 = sum(c_across(B01001D_010E:B01001D_011E),c_across(B01001D_025E:B01001D_026E)),
  Pop45to54 = sum(c_across(B01001D_012E),c_across(B01001D_027E)),
  Pop55to64 = sum(c_across(B01001D_013E),c_across(B01001D_028E)),
  Pop65to74 = sum(c_across(B01001D_014E),c_across(B01001D_029E)),
  Pop75to84 = sum(c_across(B01001D_015E),c_across(B01001D_030E)),
  Pop85Over = sum(B01001D_016E,B01001D_031E),
)

B01001D_place_2019_sub <- B01001D_place_2019 %>% select(GEOID_place, Total:Pop85Over) %>% mutate(type="Asian")

B01001H_place_2019 <- B01001H_place_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001H_001E),
  PopUnd5 = sum(B01001H_003E,B01001H_018E),
  Pop5to17 = sum(c_across(B01001H_004E:B01001H_006E),c_across(B01001H_019E:B01001H_021E)),
  Pop18to29 = sum(c_across(B01001H_007E:B01001H_009E),c_across(B01001H_022E:B01001H_024E)),
  Pop30to44 = sum(c_across(B01001H_010E:B01001H_011E),c_across(B01001H_025E:B01001H_026E)),
  Pop45to54 = sum(c_across(B01001H_012E),c_across(B01001H_027E)),
  Pop55to64 = sum(c_across(B01001H_013E),c_across(B01001H_028E)),
  Pop65to74 = sum(c_across(B01001H_014E),c_across(B01001H_029E)),
  Pop75to84 = sum(c_across(B01001H_015E),c_across(B01001H_030E)),
  Pop85Over = sum(B01001H_016E,B01001H_031E),
)

B01001H_place_2019_sub <- B01001H_place_2019 %>% select(GEOID_place, Total:Pop85Over) %>% mutate(type="White_NL")

B01001I_place_2019 <- B01001I_place_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001I_001E),
  PopUnd5 = sum(B01001I_003E,B01001I_018E),
  Pop5to17 = sum(c_across(B01001I_004E:B01001I_006E),c_across(B01001I_019E:B01001I_021E)),
  Pop18to29 = sum(c_across(B01001I_007E:B01001I_009E),c_across(B01001I_022E:B01001I_024E)),
  Pop30to44 = sum(c_across(B01001I_010E:B01001I_011E),c_across(B01001I_025E:B01001I_026E)),
  Pop45to54 = sum(c_across(B01001I_012E),c_across(B01001I_027E)),
  Pop55to64 = sum(c_across(B01001I_013E),c_across(B01001I_028E)),
  Pop65to74 = sum(c_across(B01001I_014E),c_across(B01001I_029E)),
  Pop75to84 = sum(c_across(B01001I_015E),c_across(B01001I_030E)),
  Pop85Over = sum(B01001I_016E,B01001I_031E),
)

B01001I_place_2019_sub <- B01001I_place_2019 %>% select(GEOID_place, Total:Pop85Over) %>% mutate(type="Latinx")

# Note that table names are identical across geometries
# Summarize age categories for census tract tables
B01001_tract_2019 <- B01001_tract_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001_001E),
  PopUnd5 = sum(B01001_003E,B01001_027E),
  Pop5to17 = sum(c_across(B01001_004E:B01001_006E),c_across(B01001_028E:B01001_030E)),
  Pop18to29 = sum(c_across(B01001_007E:B01001_011E),c_across(B01001_031E:B01001_035E)),
  Pop30to44 = sum(c_across(B01001_012E:B01001_014E),c_across(B01001_036E:B01001_038E)),
  Pop45to54 = sum(c_across(B01001_015E:B01001_016E),c_across(B01001_039E:B01001_040E)),
  Pop55to64 = sum(c_across(B01001_017E:B01001_019E),c_across(B01001_041E:B01001_043E)),
  Pop65to74 = sum(c_across(B01001_020E:B01001_022E),c_across(B01001_044E:B01001_046E)),
  Pop75to84 = sum(c_across(B01001_023E:B01001_024E),c_across(B01001_047E:B01001_048E)),
  Pop85Over = sum(B01001_025E,B01001_049E),
)

B01001_tract_2019_sub <- B01001_tract_2019 %>% select(GEOID_tract, Total:Pop85Over) %>% mutate(type="All")

B01001A_tract_2019 <- B01001A_tract_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001A_001E),
  PopUnd5 = sum(B01001A_003E,B01001A_018E),
  Pop5to17 = sum(c_across(B01001A_004E:B01001A_006E),c_across(B01001A_019E:B01001A_021E)),
  Pop18to29 = sum(c_across(B01001A_007E:B01001A_009E),c_across(B01001A_022E:B01001A_024E)),
  Pop30to44 = sum(c_across(B01001A_010E:B01001A_011E),c_across(B01001A_025E:B01001A_026E)),
  Pop45to54 = sum(c_across(B01001A_012E),c_across(B01001A_027E)),
  Pop55to64 = sum(c_across(B01001A_013E),c_across(B01001A_028E)),
  Pop65to74 = sum(c_across(B01001A_014E),c_across(B01001A_029E)),
  Pop75to84 = sum(c_across(B01001A_015E),c_across(B01001A_030E)),
  Pop85Over = sum(B01001A_016E,B01001A_031E),
)

B01001A_tract_2019_sub <- B01001A_tract_2019 %>% select(GEOID_tract, Total:Pop85Over) %>% mutate(type="White")

B01001B_tract_2019 <- B01001B_tract_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001B_001E),
  PopUnd5 = sum(B01001B_003E,B01001B_018E),
  Pop5to17 = sum(c_across(B01001B_004E:B01001B_006E),c_across(B01001B_019E:B01001B_021E)),
  Pop18to29 = sum(c_across(B01001B_007E:B01001B_009E),c_across(B01001B_022E:B01001B_024E)),
  Pop30to44 = sum(c_across(B01001B_010E:B01001B_011E),c_across(B01001B_025E:B01001B_026E)),
  Pop45to54 = sum(c_across(B01001B_012E),c_across(B01001B_027E)),
  Pop55to64 = sum(c_across(B01001B_013E),c_across(B01001B_028E)),
  Pop65to74 = sum(c_across(B01001B_014E),c_across(B01001B_029E)),
  Pop75to84 = sum(c_across(B01001B_015E),c_across(B01001B_030E)),
  Pop85Over = sum(B01001B_016E,B01001B_031E),
)

B01001B_tract_2019_sub <- B01001B_tract_2019 %>% select(GEOID_tract, Total:Pop85Over) %>% mutate(type="Black")

B01001D_tract_2019 <- B01001D_tract_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001D_001E),
  PopUnd5 = sum(B01001D_003E,B01001D_018E),
  Pop5to17 = sum(c_across(B01001D_004E:B01001D_006E),c_across(B01001D_019E:B01001D_021E)),
  Pop18to29 = sum(c_across(B01001D_007E:B01001D_009E),c_across(B01001D_022E:B01001D_024E)),
  Pop30to44 = sum(c_across(B01001D_010E:B01001D_011E),c_across(B01001D_025E:B01001D_026E)),
  Pop45to54 = sum(c_across(B01001D_012E),c_across(B01001D_027E)),
  Pop55to64 = sum(c_across(B01001D_013E),c_across(B01001D_028E)),
  Pop65to74 = sum(c_across(B01001D_014E),c_across(B01001D_029E)),
  Pop75to84 = sum(c_across(B01001D_015E),c_across(B01001D_030E)),
  Pop85Over = sum(B01001D_016E,B01001D_031E),
)

B01001D_tract_2019_sub <- B01001D_tract_2019 %>% select(GEOID_tract, Total:Pop85Over) %>% mutate(type="Asian")

B01001H_tract_2019 <- B01001H_tract_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001H_001E),
  PopUnd5 = sum(B01001H_003E,B01001H_018E),
  Pop5to17 = sum(c_across(B01001H_004E:B01001H_006E),c_across(B01001H_019E:B01001H_021E)),
  Pop18to29 = sum(c_across(B01001H_007E:B01001H_009E),c_across(B01001H_022E:B01001H_024E)),
  Pop30to44 = sum(c_across(B01001H_010E:B01001H_011E),c_across(B01001H_025E:B01001H_026E)),
  Pop45to54 = sum(c_across(B01001H_012E),c_across(B01001H_027E)),
  Pop55to64 = sum(c_across(B01001H_013E),c_across(B01001H_028E)),
  Pop65to74 = sum(c_across(B01001H_014E),c_across(B01001H_029E)),
  Pop75to84 = sum(c_across(B01001H_015E),c_across(B01001H_030E)),
  Pop85Over = sum(B01001H_016E,B01001H_031E),
)

B01001H_tract_2019_sub <- B01001H_tract_2019 %>% select(GEOID_tract, Total:Pop85Over) %>% mutate(type="White_NL")

B01001I_tract_2019 <- B01001I_tract_2019 %>% rowwise() %>% mutate(
  Total = sum(B01001I_001E),
  PopUnd5 = sum(B01001I_003E,B01001I_018E),
  Pop5to17 = sum(c_across(B01001I_004E:B01001I_006E),c_across(B01001I_019E:B01001I_021E)),
  Pop18to29 = sum(c_across(B01001I_007E:B01001I_009E),c_across(B01001I_022E:B01001I_024E)),
  Pop30to44 = sum(c_across(B01001I_010E:B01001I_011E),c_across(B01001I_025E:B01001I_026E)),
  Pop45to54 = sum(c_across(B01001I_012E),c_across(B01001I_027E)),
  Pop55to64 = sum(c_across(B01001I_013E),c_across(B01001I_028E)),
  Pop65to74 = sum(c_across(B01001I_014E),c_across(B01001I_029E)),
  Pop75to84 = sum(c_across(B01001I_015E),c_across(B01001I_030E)),
  Pop85Over = sum(B01001I_016E,B01001I_031E),
)

B01001I_tract_2019_sub <- B01001I_tract_2019 %>% select(GEOID_tract, Total:Pop85Over) %>% mutate(type="Latinx")

# Areal weight census tracts
grouplist <- list(B01001_tract_2019_sub,B01001A_tract_2019_sub,B01001B_tract_2019_sub,B01001D_tract_2019_sub,B01001H_tract_2019_sub,B01001I_tract_2019_sub)
anum <- 0

for (agroup in grouplist) {
  rm(table_int_att_app_sum)
#   # generalize intersection table name
#   # remove unnecessary fields from tract attribute table
#   # join tract attributes with intersected tract geometries by shared GEOID id
  table_int <- CookCounty_TractsByPlace_geom
  table_att <- agroup
  table_int_att <- left_join(table_int,table_att, by=c("GEOID_tract"="GEOID_tract"))
  
  # function used to apportion attributes
  # create weighted values by geometry
  # summarize weighted values by place
  fx_weightedval <- function(x) (x*table_int_att$AREA_pct) # percent of total area
  table_int_att_app <- table_int_att %>% mutate_at(vars(Total:Pop85Over), fx_weightedval)
  table_int_att_app_sum <- table_int_att_app %>% st_drop_geometry() %>% group_by(GEOID_place, type) %>% summarize_at(vars(Total:Pop85Over),funs(sum))
  anum <- anum + 1
  assign(paste("B01001X",as.character(anum),sep="_"),table_int_att_app_sum)
}

B01001_tract_All_2019 <- B01001X_1 %>% rbind(B01001X_2,B01001X_3,B01001X_4,B01001X_5,B01001X_6) %>% left_join(CookCountyMunisByDistrict_sub, by="GEOID_place") %>% filter(partial=="YES")
B01001_place_All_2019 <- B01001_place_2019_sub %>% rbind(B01001A_place_2019_sub,B01001B_place_2019_sub,B01001D_place_2019_sub,B01001H_place_2019_sub,B01001I_place_2019_sub) %>% left_join(CookCountyMunisByDistrict_sub, by=c("GEOID_place"="GEOID_place")) %>% filter(partial=="NO")

