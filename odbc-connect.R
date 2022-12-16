
library(DBI)
library(odbc)
library(here)
library(tidyverse)
library(keyring)


con <- dbConnect(odbc(), 
                 Driver = "ODBC Driver 17 for SQL Server", 
                 Server = "sql-prod-02.mcoe.monterey.k12.ca.us", 
                 Database = "Ed_Services_Data", 
                 UID      = key_get("sql username"),
                 PWD      = key_get("sql password") 
)





# db_drop_table(con, "CLASSENROLL")





headenr <- tbl(con, "ABSENT") %>% 
    filter(Academic_Year == "2021-22") %>%
    head(20) %>%
    collect()


headenr <- tbl(con, "CHRONIC") %>% 
    filter(YEAR == "chrabs2021.txt",
           CountyCode == "27",
           CharterYN == "No") 



tbl(con, "CODEBOOK") %>% 
    filter(table == "ENROLLMENT",
           field == "ETHNIC",
        variable == 6) %>%
    head(20)


years <- collect(headenr)

clipr::write_clip(years)
