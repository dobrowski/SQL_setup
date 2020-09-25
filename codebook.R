

library(MCOE)
library(tidyverse)
library(here)
library(googlesheets4)


con <- mcoe_sql_con()

sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"


### Gets the current codebook and then writes it to SQL -----

codebook <- read_sheet(sheet_id,
                       col_types = "ccccccD")


copy_to(con, codebook, name = "CODEBOOK",  temporary = FALSE, overwrite = TRUE)

# Looks for long descriptions that may exceed allowable character length in SQL
codebook2 <- codebook %>%
    mutate(length = str_count(definition)) %>%
    filter(length > 250)






### Adding variable names ----

# This overwrites the new tab with all the field names from the table passed on
get_column_names_into_sheet <- function(col){
    
    column_names <- tbl(con, col) %>% 
        head(0) 
    
    column_names <- collect(column_names) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        transmute(table = col,
                  source = "",
                  field_name = rowname
               )
    
    
    
    write_sheet(column_names , ss = sheet_id, sheet =  "new")
    
    
}


get_column_names_into_sheet("DASH_CCI")

