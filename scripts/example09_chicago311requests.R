library(dplyr)
library(tidyverse)

chicago311servicerequests <- read_csv("https://data.cityofchicago.org/api/views/v6vf-nfxy/rows.csv?accessType=DOWNLOAD")

typesummary <- chicago311servicerequests %>%
  filter(SR_SHORT_CODE == "AAF" | SR_SHORT_CODE == "AAE") %>%
  select(SR_TYPE, 
         SR_SHORT_CODE) %>%
  group_by(SR_TYPE,
           SR_SHORT_CODE) %>%
  summarise

select_water_311service_calls <- chicago311servicerequests %>%
    filter(SR_SHORT_CODE == "AAF" | SR_SHORT_CODE == "AAE")
  
write_csv(select_water_311service_calls, "C:/temp/select_water_311service_calls.csv")  
  
write_csv(typesummary,"C:/Users/scott/Desktop/delete/chicago311_requestsbytype.csv")
