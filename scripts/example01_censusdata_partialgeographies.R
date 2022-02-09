# Activate packages
library(censusapi)
library(dplyr)
library(data.table)
library(textutils)

# Download places

grouplist <- c("P5")
yearlist <- c(2010)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "dec/sf1",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "county (or part):031", 
                           regionin="state:17 zip code tabulation area (or part):60302",                            
                           # regionin="state:17+place:03883", 
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group$year<-ayear 
    assign(paste(agroup,"zcta",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

grouplist <- c("P5")
yearlist <- c(2010)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "dec/sf1",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "tract (or part):17031807500", 
                           # regionin="state:17 tract (or part):17031807500",                            
                           regionin="state:17+place:03883",
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group$year<-ayear 
    assign(paste(agroup,"zcta",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}


test <- HTMLdecode("https://api.census.gov/data/2010/dec/sf1?key=8f6a0a83c8a2466e3e018a966846c86412d0bb6e&get=NAME%2Cgroup%28P5%29&for=county%20%28or%20part%29%3A031&in=state%3A17%20zip%20code%20tabulation%20area%28or%20part%29%3A60302")

test

varlist <- c("B01001_001E", "B17001_001E","B17001_002E","B03002_001E", "B03002_003E", "B03002_004E", "B03002_006E", "B03002_012E")

yearlist <- c(2010:2019)
for (ayear in yearlist) {
  agroupname = paste("group(",agroup,")",sep="")
  acs_group <- getCensus(name = "acs/acs5",
                         vintage = ayear,
                         vars = c("NAME",varlist),
                         region = "county (or part):031", # tracts
                         regionin="state:17+place:03883", # places, counties, not msas
                         key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
  attach(acs_group)
  acs_group <- acs_group %>% select(-contains("EA"))
  acs_group <- acs_group %>% select(-contains("MA"))
  acs_group <- acs_group %>% select(-contains("GEO_ID"))
  acs_group <- acs_group %>% select(-contains("M_1"))
  acs_group$year<-ayear 
  acs_group$GEOID_place<-paste0(state,place)
  assign(paste(agroup,"place",ayear,sep="_"),acs_group)
  rm(acs_group)
  detach(acs_group)
}

alist_allyears <- mget(ls(pattern = "B07001_place_"))
B07001_AllYears <- rbindlist(alist_allyears, use.names = TRUE, fill = TRUE)
rm(list=ls(pattern="_place_"))
