
## ---------------------------
##
## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-09-25
## Date Last Updated: 2021-09-25
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes: Caclculates measurs of race/ethnic isolation, segregation for CCDPH jurisdiction
## https://www.censusscope.org/about_exposure.html
## https://crd230.github.io/lab6a.html#Neighborhood_Diversity
## https://rpubs.com/corey_sparks/473785
##
## ---------------------------


# download race by hispanic origin data for study years, 2010-2019
grouplist = c("B03002")
yearlist <- c(2010:2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "tract:*", # tracts
                           regionin="state:17+county:031", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"),
                                      -contains("MA"),
                                      -contains("GEO_ID"),
                                      -contains("M_1"),
                                      -contains("M")) %>%
      mutate(year = ayear,
             fips = paste0(state,county,tract))
    assign(paste(agroup,"tract",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

# merge datasets
apattern = "B03002_tract"
alist_tract <- mget(ls(pattern = apattern))
B03002_All <- rbindlist(alist_tract)
rm(list=ls(pattern=apattern))

# join with tracts by public health district layer
B03002_All_districts <- ccdph_tracts_districts_test %>% 
  left_join(B03002_All, by=c("gd_trct"="fips")) %>%
  rename(fips=gd_trct) %>%
  st_as_sf() %>%
  filter(as.numeric(sqmi) > 0)

# subset to single year, remove tracts with area = 0
B03002_All_districts_sub <- B03002_All_districts %>%
  rowwise() %>%
  mutate(
    fips_county = substr(fips,1,5),
    fips_county_year = paste0(fips_county,"_",year),
    TotalRace = B03002_001E,
    PopBlk = B03002_004E,
    PopAsn = B03002_006E,
    PopLat = B03002_012E,
    PopClr = B03002_001E-B03002_003E,
    PopWht = B03002_003E,
    PopOth = TotalRace-PopBlk-PopAsn-PopLat-PopWht) %>%
  select(fips, fips_county, fips_county_year, TotalRace:PopOth, year, dist, location, community) %>%
  # mutate_at(vars(PopBlk:PopOth), .funs = (list(pct = ~(./TotalRace*100))))
  mutate_at(vars(PopBlk:PopOth), .funs = (list(pct = ~(./TotalRace))))

B03002_All_districts_sub_max <- B03002_All_districts_sub %>% mutate(maxpct = max(c_across(c("PopBlk_pct", "PopAsn_pct", "PopLat_pct", "PopWht_pct")))) %>% 
  mutate(maxcat = ifelse(PopBlk_pct == maxpct, "BLACK",
                         ifelse(PopWht_pct == maxpct, "WHITE",
                                ifelse(PopAsn_pct == maxpct, "ASIAN",
                                       ifelse(PopLat_pct == maxpct, "LATINX", "OTHER"))))) %>%
  mutate(majcat = if_else(maxpct<0.5,"NONE",maxcat)) %>%
  select(fips, year, dist, contains("pct"),maxpct, maxcat, majcat)

write_clip(B03002_All_districts_sub_max)
st_write(B03002_All_districts_sub_max, "layers/max_race.shp", append=FALSE)

# run calculations by year for cook county, ccdph
B03002_All_districts_cc_sum <- B03002_All_districts_sub %>%
  group_by(fips_county_year) %>%
  summarise(cc_total = sum(TotalRace),
            cc_Wht = sum(PopWht),
            cc_Blk = sum(PopBlk),
            cc_Asn = sum(PopAsn),
            cc_Lat = sum(PopLat),
            cc_Oth = sum(PopOth)) %>%
  mutate_at(vars(cc_Wht:cc_Oth), .funs = (list(pct = ~(./cc_total)))) %>%
  st_drop_geometry()

B03002_All_districts_ccdph_sum <- B03002_All_districts_sub %>%
  filter(dist!="outside") %>%
  group_by(fips_county_year) %>%
  summarise(ccdph_total = sum(TotalRace),
            ccdph_Wht = sum(PopWht),
            ccdph_Blk = sum(PopBlk),
            ccdph_Asn = sum(PopAsn),
            ccdph_Lat = sum(PopLat),
            ccdph_Oth = sum(PopOth)) %>%
  mutate_at(vars(ccdph_Wht:ccdph_Oth), .funs = (list(pct = ~(./ccdph_total)))) %>% 
  st_drop_geometry()

B03002_All_districts_chi_sum <- B03002_All_districts_sub %>%
  filter(location=="chicago") %>%
  group_by(fips_county_year) %>%
  summarise(chi_total = sum(TotalRace),
            chi_Wht = sum(PopWht),
            chi_Blk = sum(PopBlk),
            chi_Asn = sum(PopAsn),
            chi_Lat = sum(PopLat),
            chi_Oth = sum(PopOth)) %>%
  mutate_at(vars(chi_Wht:chi_Oth), .funs = (list(pct = ~(./chi_total)))) %>% 
  st_drop_geometry()

B03002_All_districts_sum <- B03002_All_districts_sub %>% 
  left_join(B03002_All_districts_cc_sum, by="fips_county_year") %>%
  left_join(B03002_All_districts_ccdph_sum, by="fips_county_year") %>%
  left_join(B03002_All_districts_chi_sum, by="fips_county_year")

# segregation indices, ccdph
segregation_indices_ccdph<-B03002_All_districts_sum %>%
  filter(dist != "outside") %>%
  mutate(d.wb=abs(PopWht/ccdph_Wht - PopBlk/ccdph_Blk),
         d.wl=abs(PopWht/ccdph_Wht - PopLat/ccdph_Lat),
         d.wa=abs(PopWht/ccdph_Wht - PopAsn/ccdph_Asn),
         in.bw=(PopBlk/ccdph_Blk*PopWht/TotalRace),
         in.lw=(PopLat/ccdph_Lat*PopWht/TotalRace),
         in.aw=(PopAsn/ccdph_Asn*PopWht/TotalRace),
         is.bw=(PopBlk/ccdph_Blk*PopBlk/TotalRace),
         is.lw=(PopLat/ccdph_Lat*PopLat/TotalRace),
         is.aw=(PopAsn/ccdph_Asn*PopAsn/TotalRace))%>%
  group_by(fips_county_year)%>%
  summarise(dis.wb = .5*sum(d.wb, na.rm=T),
            dis.wl = .5*sum(d.wl, na.rm=T),
            dis.wa = .5*sum(d.wa, na.rm=T),
            int.bw = sum(in.bw, na.rm=T),
            int.lw = sum(in.lw, na.rm=T),
            int.aw = sum(in.aw, na.rm=T),
            iso.bw = sum(is.bw, na.rm=T),
            iso.lw = sum(is.lw, na.rm=T),
            iso.aw = sum(is.aw, na.rm=T)) %>%
  st_drop_geometry() %>%
  mutate(location="CCDPH")

# segregation indices, cook county
segregation_indices_cc<-B03002_All_districts_sum %>%
  mutate(d.wb=abs(PopWht/cc_Wht - PopBlk/cc_Blk),
         d.wl=abs(PopWht/cc_Wht - PopLat/cc_Lat),
         d.wa=abs(PopWht/cc_Wht - PopAsn/cc_Asn),
         in.bw=(PopBlk/cc_Blk*PopWht/TotalRace),
         in.lw=(PopLat/cc_Lat*PopWht/TotalRace),
         in.aw=(PopAsn/cc_Asn*PopWht/TotalRace),
         is.bw=(PopBlk/cc_Blk*PopBlk/TotalRace),
         is.lw=(PopLat/cc_Lat*PopLat/TotalRace),
         is.aw=(PopAsn/cc_Asn*PopAsn/TotalRace))%>%
  group_by(fips_county_year)%>%
  summarise(dis.wb = .5*sum(d.wb, na.rm=T),
            dis.wl = .5*sum(d.wl, na.rm=T),
            dis.wa = .5*sum(d.wa, na.rm=T),
            int.bw = sum(in.bw, na.rm=T),
            int.lw = sum(in.lw, na.rm=T),
            int.aw = sum(in.aw, na.rm=T),
            iso.bw = sum(is.bw, na.rm=T),
            iso.lw = sum(is.lw, na.rm=T),
            iso.aw = sum(is.aw, na.rm=T)) %>%
  st_drop_geometry()  %>%
  mutate(location="Cook County")

# segregation indices, chicago
segregation_indices_chi<-B03002_All_districts_sum %>%
  filter(location=="chicago") %>%
  mutate(d.wb=abs(PopWht/chi_Wht - PopBlk/chi_Blk),
         d.wl=abs(PopWht/chi_Wht - PopLat/chi_Lat),
         d.wa=abs(PopWht/chi_Wht - PopAsn/chi_Asn),
         in.bw=(PopBlk/chi_Blk*PopWht/TotalRace),
         in.lw=(PopLat/chi_Lat*PopWht/TotalRace),
         in.aw=(PopAsn/chi_Asn*PopWht/TotalRace),
         is.bw=(PopBlk/chi_Blk*PopBlk/TotalRace),
         is.lw=(PopLat/chi_Lat*PopLat/TotalRace),
         is.aw=(PopAsn/chi_Asn*PopAsn/TotalRace))%>%
  group_by(fips_county_year)%>%
  summarise(dis.wb = .5*sum(d.wb, na.rm=T),
            dis.wl = .5*sum(d.wl, na.rm=T),
            dis.wa = .5*sum(d.wa, na.rm=T),
            int.bw = sum(in.bw, na.rm=T),
            int.lw = sum(in.lw, na.rm=T),
            int.aw = sum(in.aw, na.rm=T),
            iso.bw = sum(is.bw, na.rm=T),
            iso.lw = sum(is.lw, na.rm=T),
            iso.aw = sum(is.aw, na.rm=T)) %>%
  st_drop_geometry() %>%
  mutate(location="City of Chicago")

# merge segregation indices datasets

apattern = "segregation_indices"
alist_tract <- mget(ls(pattern = apattern))
segregation_indices_all <- rbindlist(alist_tract)

# entropy index H

entropy_index_tracts <- B03002_All_districts_sum %>%
  mutate(e1 = PopWht_pct * log(1/PopWht_pct),
         e2 = PopBlk_pct * log(1/PopBlk_pct),
         e3 = PopAsn_pct * log(1/PopAsn_pct),
         e4 = PopLat_pct * log(1/PopLat_pct),
         e5 = PopOth_pct * log(1/PopOth_pct),
         e1_ccdph = ccdph_Wht_pct * log(1/ccdph_Wht_pct),
         e2_ccdph = ccdph_Blk_pct * log(1/ccdph_Blk_pct),
         e3_ccdph = ccdph_Asn_pct * log(1/ccdph_Asn_pct),
         e4_ccdph = ccdph_Lat_pct * log(1/ccdph_Lat_pct),
         e5_ccdph = ccdph_Oth_pct * log(1/ccdph_Oth_pct),
         e1_cc = cc_Wht_pct * log(1/cc_Wht_pct),
         e2_cc = cc_Blk_pct * log(1/cc_Blk_pct),
         e3_cc = cc_Asn_pct * log(1/cc_Asn_pct),
         e4_cc = cc_Lat_pct * log(1/cc_Lat_pct),
         e5_cc = cc_Oth_pct * log(1/cc_Oth_pct),
         e1_chi = chi_Wht_pct * log(1/chi_Wht_pct),
         e2_chi = chi_Blk_pct * log(1/chi_Blk_pct),
         e3_chi = chi_Asn_pct * log(1/chi_Asn_pct),
         e4_chi = chi_Lat_pct * log(1/chi_Lat_pct),
         e5_chi = chi_Oth_pct * log(1/chi_Oth_pct)) %>%
  mutate_at(vars(e1:e5_cc), ~replace(.,is.nan(.),0)) %>%
  mutate(
    entropy = e1+e2+e3+e4+e5,
    entropy_ccdph = e1_ccdph+e2_ccdph+e3_ccdph+e4_ccdph+e5_ccdph,
    H_ccdph = TotalRace*(entropy_ccdph-entropy)/(entropy_ccdph*ccdph_total),
    entropy_cc = e1_cc+e2_cc+e3_cc+e4_cc+e5_cc,
    H_cc = TotalRace*(entropy_cc-entropy)/(entropy_cc*cc_total),
    entropy_chi = e1_chi+e2_chi+e3_chi+e4_chi+e5_chi,
    H_chi = TotalRace*(entropy_chi-entropy)/(entropy_chi*chi_total)
  )

entropy_index_tracts_2019 <- entropy_index_tracts %>% filter(year==2019) %>%
  select(fips, ent_2010 = entropy)
entropy_index_tracts_2010 <- entropy_index_tracts %>% filter(year==2010) %>%
  select(fips, ent_2019 = entropy)

entropy_index_tracts_spread <- entropy_index_tracts %>% 
  select(fips, year, dist, entropy) %>%
  spread(year, entropy) %>%
  mutate(ent_chg = `2019`-`2010`,
         ent_pctchg = (`2019`-`2010`)/`2010`*100)

st_write(entropy_index_tracts_spread, "layers/entropy_change.shp", append=FALSE)
st_write(entropy_index_tracts, "layers/entropy_race.shp", append=FALSE)

entropy_index_tracts %>%
  group_by(fips_county_year) %>%
  summarise(H=sum(H_cc, na.rm=TRUE))

entropy_index_tracts %>%
  filter(dist != "outside") %>%
  group_by(fips_county_year) %>%
  summarise(H=sum(H_ccdph, na.rm=TRUE))

entropy_index_tracts %>%
  filter(location == "chicago") %>%
  group_by(fips_county_year) %>%
  summarise(H=sum(H_chi, na.rm=TRUE))