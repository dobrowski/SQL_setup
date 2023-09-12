
library(DBI)
library(odbc)
library(here)
library(tidyverse)
library(keyring)


con <- dbConnect(odbc(), 
                 Driver = "ODBC Driver 17 for SQL Server", 
  #  Driver =  "/usr/local/lib/libmsodbcsql.18.dylib",
  # Driver = "/usr/local/Cellar/mssql-tools/17.10.1.1"
                 Server = "sql-prod-02.mcoe.monterey.k12.ca.us", 
                 Database = "Ed_Services_Data", 
                 UID      = key_get("sql username"),
                 PWD      = key_get("sql password") 
)





# db_drop_table(con, "CLASSENROLL")





headenr <- tbl(con, "RECLASS") %>% 
    filter(District == "Chualar Union",
    #       charter_school == "No",
    #       dass == "No",
     #      reporting_category == "TA"
    ) %>%
  #  filter(academic_year == "2021-22") %>%
    head(200) %>%
    collect()


headenr <- tbl(con, "ELAS") %>% 
    filter(YEAR == "2223",
           # CountyCode == "27",
           # CharterYN == "No"
           ) %>%
    head(200) %>%
    collect()




tbl(con, "CODEBOOK") %>% 
    filter(table == "ENROLLMENT",
           field == "ETHNIC",
        variable == 6) %>%
    head(20)


years <- collect(headenr)

clipr::write_clip(years)
