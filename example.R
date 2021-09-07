

library(MCOE)
library(tidyverse)


con <- mcoe_sql_con()



headenr <- tbl(con, "DASH_ALL") %>% 
    # filter(CountyCode == "27",
    #        DistrictCode == "10272",
    #        AcademicYear >= "2018-19") %>%
#    head(20) %>%
    collect()

grad_four <- headenr  %>% 
    filter(ReportingCategory %in% c("TA","SE","SH","SF"),
           AggregateLevel == "S",
           CharterSchool == "All",
           DASS == "All")








codebook.susp <- codebook %>% 
    filter(table == "SUSP",
           field_name == "reporting_category") 






growth.mry <- growth %>% 
    filter( str_starts(cds, "27") )
