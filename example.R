

library(MCOE)
library(tidyverse)


con <- mcoe_sql_con()



headenr <- tbl(con, "EXP") %>% 
#    filter(ETHNIC == 6) %>%
    head(20) %>%
    collect()

headenr




codebook.susp <- codebook %>% 
    filter(table == "SUSP",
           field_name == "reporting_category")
