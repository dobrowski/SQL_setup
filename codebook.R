

library(MCOE)
library(tidyverse)
library(here)
library(googlesheets4)


con <- mcoe_sql_con()

codebook <- read_sheet("https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0",
                       col_types = "ccccccD")


codebook2 <- codebook %>%
    mutate(length = str_count(definition))

copy_to(con, codebook, name = "CODEBOOK",  temporary = FALSE, overwrite = TRUE)
