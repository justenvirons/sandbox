library(jsonlite)
library(dplyr)
library(tidyverse)
library(janitor)
library(sf)

IL_Places_2020_geom <- places("IL", cb=TRUE, year=2020,class="sf")
IL_Places_2019_geom <- places("IL", cb=TRUE, year=2019,class="sf")

IL_Tracts_2020_geom <- tracts("IL", cb=TRUE, year=2020,class="sf")
IL_Tracts_2019_geom <- tracts("IL", cb=TRUE, year=2019,class="sf")



cc_tracts_2020_geom <- IL_Tracts_2020_geom %>%
  filter(COUNTYFP=="031") %>% st_transform(crs=26916) %>%
  mutate(fips = GEOID,
         sqmi_2020=st_area(geometry)*0.0000003861) %>%
  select(fips,sqmi_2020) %>% left_join(cc_tracts_2010_nogeom,by="fips") %>%
  rename(sqmi_2010 = sqmi)

st_write(cc_tracts_2020_geom,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/cc_tracts_2020_districts.shp")

plot(cc_tracts_2020_geom['district'])

cc_tracts_2010_nogeom <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/ccdph_tracts_districts_mod.shp") %>% 
  st_drop_geometry() %>%
  rename(fips=gd_trct,
         district=dist) %>%
  select(-community)



cc_tracts_2020_geom %>% summarize(count = n(),
            sqmi=sum(sqmi))

ccdph_tracts_districts = st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/ccdph_tracts_districts.shp") %>%
  mutate(sqmi=st_area(geometry)*0.0000003861)


cc_tracts_2020_epsg3435 = st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/cc_tracts_2020_districts.shp") %>%
  st_transform(crs=3435) %>%
  rename(sqmi=sqmi_2020) %>%
  select(fips, sqmi, district, location)

st_write(cc_tracts_2020_epsg3435,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/cc_tracts_2020_epsg3435.shp")

cc_tracts_2020_epsg4326 = st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/cc_tracts_2020_districts.shp") %>%
  st_transform(crs=4326) %>%
  rename(sqmi=sqmi_2020) %>%
  select(fips, sqmi, district, location)

st_write(cc_tracts_2020_epsg4326,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/cc_tracts_2020_epsg4326.shp")

cc_tracts_2010_epsg3435 = st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/ccdph_tracts_districts_mod.shp") %>%
  st_transform(crs=3435) %>%
  select(fips=gd_trct, sqmi, district=dist, location)

st_write(cc_tracts_2010_epsg3435,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/cc_tracts_2010_epsg3435.shp")

cc_tracts_2010_epsg4326 = st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/ccdph_tracts_districts_mod.shp") %>%
  st_transform(crs=4326) %>%
  select(fips=gd_trct, sqmi, district=dist, location)

st_write(cc_tracts_2010_epsg4326,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/cc_tracts_2010_epsg4326.shp")

cc_tracts_2020_epsg3435_nogeom <- cc_tracts_2020_epsg3435 %>%
  st_drop_geometry()

write_csv(cc_tracts_2010_epsg4326,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/cc_tracts_2010_epsg4326.shp")

#load in sf2 variable names to match with codes
# https://www.census.gov/programs-surveys/decennial-census/about/rdo/summary-files.html
# https://api.census.gov/data/2020/dec/pl/examples.html
# https://api.census.gov/data/2020/dec/pl/variables.html

# import census tract geographies with district, jurisdiction, attributes
cc_tracts_2020_epsg3435 <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-jurisdictions/layers/cc_tracts_2020_epsg3435.shp")

pl_P1_data_vars <- fromJSON("https://api.census.gov/data/2020/dec/pl/variables?get=NAME,group(P1)&for=tract:*&in=state:17&in=county:031&key=8f6a0a83c8a2466e3e018a966846c86412d0bb6e") %>%
  as.data.frame() %>%
  row_to_names(row_number = 1) %>%
  filter(str_detect(name,"P1_")) %>% 
  mutate(label = str_replace(label, "!!"," "),
         label = str_replace_all(label, "!!"," ")) %>%
  arrange(name)

pl_p1_tracts_data_raw <- fromJSON("https://api.census.gov/data/2020/dec/pl?get=NAME,group(P1)&for=tract:*&in=state:17&in=county:031&key=8f6a0a83c8a2466e3e018a966846c86412d0bb6e") %>%
  as.data.frame() %>%
  row_to_names(row_number = 1)

pl_p1_tracts_data <- pl_p1_data %>% 
  select(-contains("NA")) %>%
  mutate(fips = substr(GEO_ID,10,21)) %>%
  mutate(across(P1_001N:P1_071N,as.numeric)) %>% 
  left_join(cc_tracts_2020_epsg3435,by="fips")

# P2 variables, Race by Ethnicity
pl_p2_data_vars <- fromJSON("https://api.census.gov/data/2020/dec/pl/variables?get=NAME,group(P2)&for=tract:*&in=state:17&in=county:031&key=8f6a0a83c8a2466e3e018a966846c86412d0bb6e") %>%
  as.data.frame() %>%
  row_to_names(row_number = 1) %>%
  filter(str_detect(name,"P2_")) %>% 
  mutate(label = str_replace(label, "!!"," "),
         label = str_replace_all(label, "!!"," ")) %>%
  arrange(name)

pl_p2_data <- fromJSON("https://api.census.gov/data/2020/dec/pl?get=NAME,group(P2)&for=tract:*&in=state:17&in=county:031&key=8f6a0a83c8a2466e3e018a966846c86412d0bb6e") %>%
  as.data.frame() %>%
  row_to_names(row_number = 1)

pl_p2_tracts_data <- pl_p2_data %>% 
  select(-contains("NA")) %>%
  mutate(fips = substr(GEO_ID,10,21)) %>%
  mutate(across(P2_001N:P2_073N,as.numeric)) %>% 
  left_join(cc_tracts_2020_epsg3435,by="fips") %>%
  st_as_sf()

pl_p2_tracts_data_sub <- pl_p2_tracts_data %>% 
  mutate(fips = substr(GEO_ID,10,21)) %>%
  mutate(across(P2_001N:P2_073N,as.numeric)) %>%
  select(-contains("NA")) %>%
  rename(total_pop=P2_001N,
         hisp=P2_002N,
         nh=P2_003N,
         nh_onerace=P2_004N,
         nh_white=P2_005N,
         nh_black=P2_006N,
         nh_american=P2_007N,
         nh_asian=P2_008N,
         nh_native=P2_009N,
         nh_other=P2_010N,
         nh_twoplus=P2_011N)

totals <- pl_p2_tracts_data_sub %>% 
  st_drop_geometry() %>%
  group_by(district) %>%
  summarise(total = sum(total_pop),
            nh_white = sum(nh_white),
            nh_black = sum(nh_black),
            nh_asian = sum(nh_asian),
            nh_other = sum(nh_american+nh_native+nh_other),
            nh_tworaces = sum(nh_twoplus),
            hispanic = sum(hisp)) %>%
  t()

pl_p2_tracts_data_sub_sum <- pl_p2_tracts_data_sub %>% select(fips,tract,sqmi,district,location,total_pop:nh_twoplus) %>% st_drop_geometry()

write_csv(pl_p2_tracts_data_sub_sum,"E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-data-sets/2020/decennial-2020-race-ethnicity-by-tract.csv")

write_clip(totals)
