

library(vroom)


import_files <- function(dir,globy,naming){
    setwd(dir)
    
    files <- fs::dir_ls(glob = globy)
    
    print(files)
    
    output <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = naming), id = "YEAR"))
    
    setwd(here())
    
    output
}


####  Enrollment -----

enr_vroom <- import_files(here("data","enrollment"),"enr*txt","none") 

enr <- enr_vroom %>%
    mutate(YEAR = str_extract(YEAR,"\\d\\d"))



copy_to(con, enr, name = "ENROLLMENT",  temporary = FALSE)


####  ELA Status and LTELS  -----

elas_vroom <- import_files(here("data","elas"),"Lte*txt","none") 

elas <- elas_vroom %>%
    mutate(YEAR = str_extract(str_remove(YEAR,"-"),"[:digit:]+"))



copy_to(con, elas, name = "ELAS",  temporary = FALSE)


####  FRPM  ----
# https://www.cde.ca.gov/ds/sd/sd/filessp.asp


library(readxl)

setwd(here("data","frpm"))

files <- fs::dir_ls( glob = "frpm*")

frpm <- sapply(files,
               read_excel,
               sheet = 2,
               skip =1  ,
               .name_repair = ~ janitor::make_clean_names(., case = "snake"),
               simplify=FALSE
               ) %>% 
    bind_rows(.id = "id")

setwd(here())


copy_to(con, frpm, name = "FRPM",  temporary = FALSE)





####  Unduplicated Pupil Count  ----
# https://www.cde.ca.gov/ds/sd/sd/filescupc.asp


library(readxl)

setwd(here("data","upc"))

files <- fs::dir_ls( glob = "cupc*")

upc1 <- sapply(files,
               read_excel,
               sheet = 3,
               skip =1  ,
               .name_repair = ~ janitor::make_clean_names(., case = "snake"),
               simplify=FALSE
               ) %>% 
    bind_rows(.id = "id")



upc2 <- sapply(files,
               read_excel,
               sheet = 3,
               skip =1  ,
               .name_repair = ~ janitor::make_clean_names(., case = "snake"),
               simplify=FALSE
               ) %>% 
    bind_rows(.id = "id")

upc <- upc1 %>%
    bind_rows(upc2) %>%
    mutate(across(school_type:high_grade, na_if, "N/A"))

setwd(here())


copy_to(con, upc, name = "UPC",  temporary = FALSE)




####  College Going Rate  -----
# https://www.cde.ca.gov/ds/sd/sd/filescgr12.asp

cgr <- import_files(here("data","cgr"),"cgr*txt","none") 

copy_to(con, cgr, name = "CGR",  temporary = FALSE)





####  Expulsions  -----
# https://www.cde.ca.gov/ds/sd/sd/filesed.asp

exp <- import_files(here("data","exp"),"exp*txt","snake") 

exp <- exp %>%
    mutate(charter_yn = if_else(is.na(charter_yn),charter_y_n,charter_yn)) %>%
    select(-charter_y_n) %>%
    mutate(charter_yn = recode(charter_yn, Y = "Yes", N = "No")) %>%
    mutate(school_name = iconv(enc2utf8(school_name),sub="byte"))

copy_to(con, exp, name = "EXP",  temporary = FALSE)




####  Suspensions  -----
# https://www.cde.ca.gov/ds/sd/sd/filessd.asp

susp <- import_files(here("data","susp"),"susp*txt","snake") 

susp <- susp  %>%
    mutate(charter_yn = if_else(is.na(charter_yn),charter_y_n,charter_yn)) %>%
    select(-charter_y_n) %>%
    mutate(charter_yn = recode(charter_yn, Y = "Yes", N = "No")) %>%
    mutate(school_name = iconv(enc2utf8(school_name),sub="byte"))

copy_to(con, susp, name = "SUSP",  temporary = FALSE)





