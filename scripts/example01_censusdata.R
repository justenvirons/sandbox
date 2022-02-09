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
library(tigris) 
library(tidyverse)
library(dplyr) 
library(sf) 
library(clipr)
library(xlsx)
library(leaflet)
library(scales)
library(ggmap)

library(tidyverse)
library(dplyr) 
library(sf) 
library(clipr)
library(readxl)
library(lubridate)
library(data.table)
library(stringr)
library(sp)
library(ggmap)
library(arcgisbinding)
library(RCurl)
library(gtools)

# Download  census tract geographies
# cb = cartographic boundary
# Project both to UTM North Zone 16

IL_BlockGroups_geom <- block_groups("IL", cb=TRUE, class="sf", year="2019") %>% filter(COUNTYFP=="031")

IL_Tracts_geom <- tracts("IL", cb=TRUE, class="sf", year="2010")
IL_Places_geom <- places("IL", cb=TRUE, class="sf")
IL_Counties_geom <- counties("IL", cb=TRUE, class="sf")
IL_Tracts_geom <- st_transform(IL_Tracts_geom, crs = 26916)
IL_Places_geom <- st_transform(IL_Places_geom, crs = 26916)
IL_Counties_geom <- st_transform(IL_Counties_geom, crs = 26916)

IL_MCDs_geom <- county_subdivisions("IL", cb=TRUE, class="sf")
st_write(IL_MCDs_geom, "C:/temp/IL_MCDs_geom.shp") 
st_write(IL_Places_geom, "C:/temp/il_places.shp")

rm(IL_Tracts_geom)

IL_Tractscc_zctas_clipped <- st_collection_extract(cc_zctas_clipped, "POLYGON") %>%
_geom_cc <- IL_Tracts_geom %>% filter(COUNTYFP=="031")

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

# Use censusapi package to retrieve attribute data from the different datasets
# Use below to list all census apis available
apis <- listCensusApis()
View(apis)

# Download complete list of ACS 5-year subject tables
ayear <- "2019"
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5/subject",
  # name = "cbp",
  vintage = ayear,
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste("grouptable_subject_",ayear,sep=""),acs_groups_tables) # change name of dataframe
rm(acs_groups_tables)

# Download complete list of ACS 5-year tables
ayear <- "2019"
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5",
  # name = "cbp",
  vintage = ayear,
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste("grouptable_","year",sep=""),acs_groups_tables) # change name of dataframe

# Variables for ACS data table
ayear <- "2019"
agroup <- "B01001"
acs_groups_vars <- listCensusMetadata(
  name = "acs/acs5",
  vintage = ayear,
  group = agroup,
  type = "variables")
acs_groups_vars$year<-ayear
acs_groups_vars <- acs_groups_vars %>% 
  filter(!str_detect(name,"EA"),!str_detect(name,"M")) %>% 
  mutate(label = str_replace_all(label, "Estimate!!",""),
         label = str_replace(label, "!!"," "),
         label = str_replace_all(label, "!!"," "))
assign(paste("groupvars_",agroup,"_",ayear, sep=""),acs_groups_vars)
rm(acs_groups_vars)

write_clip(groupvars_B28001_2019)
write_csv(groupvars_B28001_2019,"C:/Users/scott/Desktop/delete/groupvars_B28001_2019.csv")

# List of ACS tables to be downloaded
grouplist <- c("B01001")

# Download places
yearlist <- c(2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "zip code tabulation area:*", # tracts
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
    assign(paste(agroup,name,ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

# List of ACS variables to be downloaded across tables
varlist <- c("P001001")

"B01002"
tablename <- "demos_by_tract"

B01002

# Download data for places
yearlist <- c(2010)
for (ayear in yearlist) {
  census_table <- getCensus(name = "dec/sf1",
                            vintage = ayear,
                            vars = c("NAME",varlist),
                            region = "tract:*", # tracts
                            regionin="state:17+county:031", # places, counties, not msas
                            key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
  attach(census_table)
  census_table <- census_table %>% 
    mutate(year = ayear,
           GEOID = paste0(state,county, tract))
  assign(paste("P001001",ayear,sep="_"),census_table)
  rm(census_table)
  detach(census_table)
}

alist_demos <- mget(ls(pattern = "demos_by"))
alist_demos_df <- rbindlist(alist_demos)

write_rds(alist_demos_df, "C:/Users/scott/OneDrive - CCDPH/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/data/census_demos.rds")

# List of ACS variables to be downloaded across tables
varlist <- c("S1903_C01_001E")

census_table <- getCensus(name = "acs/acs5/subject",
                          vintage = "2019",
                          vars = c("NAME",varlist),
                          region = "tract:*", # tracts
                          regionin="state:17", # places, counties, not msas
                          key="72ccddf726eca9848b307e3ee67e892d63c97bca")


census_table_vars <- listCensusMetadata(name = "acs/acs5/subject",
                          vintage = "2019",
                          group = "S1903",
                          type = "variables")


ayear <- "2019"
agroup <- "B01002"
acs_groups_vars <- listCensusMetadata(
  name = "acs/acs5",
  vintage = ayear,
  group = agroup,
  type = "variables")

# Download data for places
yearlist <- c(2019)
for (ayear in yearlist) {
  census_table <- getCensus(name = "acs/acs5",
                         vintage = ayear,
                         vars = c("NAME",varlist),
                         region = "tract:*", # tracts
                         regionin="state:17", # places, counties, not msas
                         key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
  attach(census_table)
  census_table <- census_table %>% 
    mutate(year = ayear,
           GEOID = paste0(state,place))
  assign(paste(tablename,ayear,sep="_"),census_table)
  rm(census_table)
  detach(census_table)
}

# Note that table names are identical across geometries
# Summarize age categories for census tract tables

B01001_place_2010_sub <- B01001_place_2010 %>% rowwise() %>% mutate(
  Total = sum(B01001_001E),
  PopUnd5 = sum(B01001_003E,B01001_027E),
  Pop5to17 = sum(c_across(B01001_004E:B01001_006E),c_across(B01001_028E:B01001_030E)),
  Pop18to29 = sum(c_across(B01001_007E:B01001_011E),c_across(B01001_031E:B01001_035E)),
  Pop30to44 = sum(c_across(B01001_012E:B01001_014E),c_across(B01001_036E:B01001_038E)),
  Pop45to54 = sum(c_across(B01001_015E:B01001_016E),c_across(B01001_039E:B01001_040E)),
  Pop55to64 = sum(c_across(B01001_017E:B01001_019E),c_across(B01001_041E:B01001_043E)),
  Pop65to74 = sum(c_across(B01001_020E:B01001_022E),c_across(B01001_044E:B01001_046E)),
  Pop75to84 = sum(c_across(B01001_023E:B01001_024E),c_across(B01001_047E:B01001_048E)),
  Pop85Over = sum(B01001_025E,B01001_049E)
) %>%
  select(place,Total:Pop85Over)

B03002_place_2010_sub <- B03002_place_2010 %>%
  arrange(place) %>%
  rowwise() %>%
  mutate(TotalRace = B03002_001E,
         PopBlk = B03002_004E,
         PopAsn = B03002_006E,
         PopLat = B03002_012E,
         PopClr = B03002_001E-B03002_003E,
         PopWht = B03002_003E) %>%
  select(place, TotalRace:PopWht) %>%
  mutate_at(vars(PopBlk:PopWht), .funs = (list(pct = ~(./TotalRace*100))))


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


