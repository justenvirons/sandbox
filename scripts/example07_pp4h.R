## ---------------------------
##
## Script name: PP4H Evaluation Project
## Purpose of script: Develop demographic other statistics for Maywood, IL to be
## entered into PRISM simulation model
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-03-11
## Date Last Updated: 2021-03-17
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(censusapi) # https://cran.r-project.org/web/packages/censusapi/censusapi.pdf
library(dplyr) # https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
library(tidyverse) # https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf
library(tigris) # https://cran.r-project.org/web/packages/tigris/tigris.pdf
library(leaflet) # https://cran.r-project.org/web/packages/leaflet/leaflet.pdf
library(sf) # https://cran.r-project.org/web/packages/sf/sf.pdf
library(scales) # https://cran.r-project.org/web/packages/scales/scales.pdf
library(clipr)

# Import vital statistis for communities within suburban Cook County via data portal
vitalstatistics <- read_csv("https://datacatalog.cookcountyil.gov/api/views/r5wk-nc2x/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
vitalstatistics_Maywood <- vitalstatistics %>% filter(Year==2017, Place=="Maywood", `Race/Ethnicity`=="All") %>% select(`Age Group`,`Total Deaths`,`CVD Deaths`)

# Download census geographies
# Download  census tract geographies
# cb = cartographic boundary
# Project both to UTM North Zone 16
IL_Tracts_geom <- tracts("IL", cb=TRUE, class="sf")
IL_Places_geom <- places("IL", cb=TRUE, class="sf")
IL_Counties_geom <- counties("IL", cb=TRUE, class="sf")
IL_Tracts_geom <- st_transform(IL_Tracts_geom, crs = 26916)
IL_Places_geom <- st_transform(IL_Places_geom, crs = 26916)
IL_Counties_geom <- st_transform(IL_Counties_geom, crs = 26916)

# Import census data from the ACS 2015-2019 5-year estimates
grouplist <- c("B01001","B03002","B17001")
# by census tract

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
    head(acs_group)
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group <- acs_group %>% select(-contains("NAME_1"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group$year<-ayear 
    acs_group$GEOID<-paste0(state,place)
    assign(paste(agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
  }
}

B01001_2019_sub <- B01001_2019 %>%
  rowwise() %>% 
  mutate(Total = sum(B01001_001E),
  PopUnd5 = sum(B01001_003E,B01001_027E),
  Pop5to17 = sum(c_across(B01001_004E:B01001_006E),c_across(B01001_028E:B01001_030E)),
  Pop18to29 = sum(c_across(B01001_007E:B01001_011E),c_across(B01001_031E:B01001_035E)),
  Pop30to44 = sum(c_across(B01001_012E:B01001_014E),c_across(B01001_036E:B01001_038E)),
  Pop45to54 = sum(c_across(B01001_015E:B01001_016E),c_across(B01001_039E:B01001_040E)),
  Pop55to64 = sum(c_across(B01001_017E:B01001_019E),c_across(B01001_041E:B01001_043E)),
  Pop65to74 = sum(c_across(B01001_020E:B01001_022E),c_across(B01001_044E:B01001_046E)),
  Pop75to84 = sum(c_across(B01001_023E:B01001_024E),c_across(B01001_047E:B01001_048E)),
  Pop85Over = sum(c_across(B01001_025E+B01001_049E)),
  Pop30to64 = sum(c_across(Pop30to44:Pop55to64)),
  Pop65Over = sum(c_across(Pop65to74:Pop85Over))) %>%
  select(GEOID,Total:Pop65Over)

B03002_2019_sub <- B03002_2019 %>% 
  rowwise() %>%
  mutate(POPBLK = B03002_004E,
  POPASN = B03002_006E,
  POPLAT = B03002_012E,
  POPWHT = B03002_003E,
  PCTBLK = B03002_004E/B03002_001E*100,
  PCTASN = B03002_006E/B03002_001E*100,
  PCTLAT = B03002_012E/B03002_001E*100,
  PCTWHT = B03002_003E/B03002_001E*100) %>%
  select(POPBLK:PCTWHT)
  
B17001_2019_sub <- B17001_2019 %>% 
  rowwise() %>%
  mutate(POPBPOVTOT = B17001_001E,
    POPBPOV = B17001_002E,
    PCTBPOV = B17001_002E/B17001_001E*100) %>%
    select(POPBPOVTOT:PCTBPOV)

BaselineData_2019 <- B01001_2019_sub %>% bind_cols(B03002_2019_sub,B17001_2019_sub)
BaselineData_2019_geom <- IL_Places_geom %>%
  left_join(BaselineData_2019, by="GEOID") %>%
  filter(NAME=='Maywood') %>%
  mutate(SQMI=ALAND*0.00000038610,
         POPDENS=Total/SQMI,
         PCT65OVER = Pop65Over/Total*100)
plot(BaselineData_2019_geom['GEOID'])
