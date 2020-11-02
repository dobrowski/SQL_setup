

library(MCOE)
library(tidyverse)


con <- mcoe_sql_con()



headenr <- tbl(con, "ENROLLMENT") %>% 
    filter(ETHNIC == 6) %>%
    head(20)

headenr




codebook.susp <- codebook %>% 
    filter(table == "SUSP",
           field_name == "reporting_category")
