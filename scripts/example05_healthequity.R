library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(clipr)

# set your working/project directory location
setwd("C:/Users/scott/Box/PEDAGOGY/GEO_346_Spring2020/Exercises/HealthEquity")

# import project data
load("Data_ByZCTA_20200604.RData")

# download and import data from Chicago Data Portal
Chicago_COVID19_ByZCTA <- read_csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")
Chicago_COVID19_ByZCTA <- Chicago_COVID19_ByZCTA %>% rename("ZCTA5"="ZIP Code", "WeekNo"="Week Number","StartDate"="Week Start","EndDate"="Week End","CasesWk"="Cases - Weekly","CasesCm"="Cases - Cumulative","CasesWkRt"="Case Rate - Weekly","CasesCmRt"="Case Rate - Cumulative","TestsWk"="Tests - Weekly","TestsCm"="Tests - Cumulative","TestsWkRt"="Test Rate - Weekly","TestsCmRt"="Test Rate - Cumulative","DeathsWk"="Deaths - Weekly","DeathsCm"="Deaths - Cumulative","DeathsWkRt"="Death Rate - Weekly","DeathsCmRt"="Death Rate - Cumulative", "PctPosWk"="Percent Tested Positive - Weekly", "PctPosCum"="Percent Tested Positive - Cumulative","TotPop"="Population","RowID"="Row ID","GEOM"="ZIP Code Location")
Chicago_COVID19_ByZCTA$GEOM <- NULL
Chicago_COVID19_ByZCTA$StartDate <- as.Date(Chicago_COVID19_ByZCTA$StartDate, "%m/%d/%y") # reformat date text to date data type
Chicago_COVID19_ByZCTA$EndDate <- as.Date(Chicago_COVID19_ByZCTA$EndDate, "%m/%d/%y") # reformat date text to date data type
Chicago_COVID19_ByZCTA_geom <- left_join(Chicago_COVID19_ByZCTA, US_ZCTAs_cb, by = "ZCTA5")
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom %>% filter(StartDate>=as.Date("3/15/2020","%m/%d/%y"))
Chicago_COVID19_ByZCTA_geom <- Chicago_COVID19_ByZCTA_geom[, !duplicated(colnames(Chicago_COVID19_ByZCTA_geom))]
Chicago_COVID19_ByZCTA_geom[is.na(Chicago_COVID19_ByZCTA_geom)] <- 0

# Create dominant race/ethnicity fields with apply
# which(colnames(Chicago_COVID19_ByZCTA_geom)=="PCTBLK") # Get row number
Chicago_COVID19_ByZCTA_geom$RMAXPCT <- apply(Chicago_COVID19_ByZCTA_geom[,c("PCTBLK","PCTASN","PCTWHT","PCTLAT")],1, max)
Chicago_COVID19_ByZCTA_geom$RMAXCAT <- "NONE"
Chicago_COVID19_ByZCTA_geom <- mutate(Chicago_COVID19_ByZCTA_geom, RMAXCAT = ifelse(PCTBLK == RMAXPCT, "BLACK",
                                                                                    ifelse(PCTWHT == RMAXPCT, "WHITE",
                                                                                           ifelse(PCTASN == RMAXPCT, "ASIAN",
                                                                                                  ifelse(PCTLAT == RMAXPCT, "LATINX", "OTHER")))))
# Chicago_COVID19_ByZCTA_geom <- mutate(Chicago_COVID19_ByZCTA_geom, RMAXCAT = ifelse(RMAXPCT<25, "NONE",RMAXCAT))
Chicago_COVID19_ByZCTA_geom$SurvCm <- Chicago_COVID19_ByZCTA_geom$TOTPOP-Chicago_COVID19_ByZCTA_geom$DeathsCm
Chicago_COVID19_ByZCTA_geom$NotTestedCm <- Chicago_COVID19_ByZCTA_geom$TOTPOP-Chicago_COVID19_ByZCTA_geom$TestsCm
Chicago_COVID19_ByZCTA_geom$NotInfCm <- Chicago_COVID19_ByZCTA_geom$TOTPOP-Chicago_COVID19_ByZCTA_geom$CasesCm

# write shapefile to project directory
st_write(Chicago_COVID19_ByZCTA_geom, "Layers/Chicago_COVID19_ByZCTA_geom.shp", append=FALSE)

# Summary table
Chicago_COVID19_ByZCTA_geom %>% select(RMAXCAT,TOTPOP,DeathsCm,CasesCm, TestsCm, EndDate) %>% filter(EndDate==as.Date("5/23/2020","%m/%d/%y")) %>% group_by(RMAXCAT) %>% summarise(ZCTAs = n(),Population = sum(TOTPOP),Deaths = sum(DeathsCm),DeathRt = sum(DeathsCm)/sum(TOTPOP)*100000, Tests = sum(TestsCm),TestsRt = sum(TestsCm)/sum(TOTPOP)*1000, Cases = sum(CasesCm),CasesRt = sum(CasesCm)/sum(TOTPOP)*1000)

# get date list
aEndDateList <- Chicago_COVID19_ByZCTA_geom %>% select(EndDate) %>% group_by(EndDate) %>% summarise()
aEndDateList <- aEndDateList[4:11,] # return subset of list
aEndDateList # view date list

# run chi-squared analyses for deaths, tests, cases
for (i in 1:nrow(aEndDateList)) {
  # ct_CountsbyCategory <- Chicago_COVID19_ByZCTA_geom %>% filter(EndDate == aEndDateList$EndDate[i]) %>% select(DeathsCm,SurvCm,RMAXCAT) %>% group_by(RMAXCAT) %>% summarise(Died = sum(DeathsCm), Survived = sum(SurvCm))
  # ct_CountsbyCategory <- Chicago_COVID19_ByZCTA_geom %>% filter(EndDate == aEndDateList$EndDate[i]) %>% select(TestsCm,NotTestedCm,RMAXCAT) %>% group_by(RMAXCAT) %>% summarise(Tested = sum(TestsCm), "Not Tested" = sum(NotTestedCm))
  # ct_CountsbyCategory <- Chicago_COVID19_ByZCTA_geom %>% filter(EndDate == aEndDateList$EndDate[i]) %>% select(CasesCm,NotInfCm,RMAXCAT) %>% group_by(RMAXCAT) %>% summarise(Confirmed = sum(CasesCm), "Not Infected" = sum(NotInfCm))
  ct_CountsbyCategory[is.na(ct_CountsbyCategory)] <- 0
  ct_CountsbyCategory_t <- t(ct_CountsbyCategory[,-1])
  colnames(ct_CountsbyCategory_t) <- ct_CountsbyCategory$RMAXCAT
  chisq_Results <- chisq.test(ct_CountsbyCategory_t)
  achisqtable_obs <-  chisq_Results$observed
  achisqtable_exp <- round(chisq_Results$expected,2)
  achisqtable_exp <- achisqtable_exp[1,]
  achisqtable_obs <- achisqtable_obs[1,]
  achisqtable_cbind <- cbind(achisqtable_exp,achisqtable_obs)
  achisqtable_cbind <- as.data.frame.matrix(achisqtable_cbind)
  achisqtable_cbind$EndDate <- aEndDateList$EndDate[i]
  achisqtable_cbind$PValue <- chisq_Results$p.value
  achisqtable_cbind$XSqrd <- chisq_Results$statistic
  achisqtable_cbind$df <- chisq_Results$parameter
  achisqtable_cbind <- cbind(rownames(achisqtable_cbind), data.frame(achisqtable_cbind, row.names=NULL))
  achisqtable_All <- rbind(achisqtable_cbind,achisqtable_All)
}

achisqtable_All <- achisqtable_All %>% rename("Race"=`rownames(achisqtable_cbind)`,"Expected"='achisqtable_exp',"Observed"='achisqtable_obs')
achisqtable_All$Residual <- achisqtable_All$Observed - achisqtable_All$Expected
write_clip(achisqtable_All)

# create residual plot
ggplot(achisqtable_All) + geom_point(aes(x=EndDate,y=Residual, color = Race), shape=1, size = 3, stroke=2) + geom_line(aes(x=EndDate,y=Residual, color = Race), size=1.5) + scale_color_manual(values=c('#A6758D','#8DB1D5','#FFC000','#FBA2A2')) + theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank(), legend.key=element_blank(), axis.ticks=element_line(size=1), text = element_text(family="arial", face="bold", size=18), axis.text.x = element_text(angle = 90)) 

# run these lines before running subsequent chi-square analyses
achisqtable_All <- achisqtable_cbind
achisqtable_All <- achisqtable_All[c(), ]