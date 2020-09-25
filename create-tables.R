

library(tidyverse)
library(vroom)
library(here)
library(readxl)


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





####  4 year adjusted grad cohort  -----
# https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp

grad4 <- import_files(here("data","grad4"),"cohort*txt","none") 

grad4 <- grad4  %>% mutate_at(vars(CohortStudents:Still_Enrolled_Rate), funs(as.numeric) ) 

gradhead <- head(grad4, 200000)

copy_to(con, grad4, name = "GRAD_FOUR",  temporary = FALSE, overwrite = TRUE)


tbl(con,"GRAD_FOUR") %>%
    count()


tbl(con,"GRAD_FOUR") %>%
    count()


####  5 year adjusted grad cohort  -----
# https://www.cde.ca.gov/ds/sd/sd/filesfycgr.asp

grad5 <- import_files(here("data","grad5"),"cohort*txt","none") 

grad5 <- grad5  %>% mutate_at(vars(Cohort_Students:Dropout_Rate), funs(as.numeric) ) 

copy_to(con, grad5, name = "GRAD_FIVE",  temporary = FALSE, overwrite = TRUE)


tbl(con,"GRAD_FIVE") %>%
    count()



####  Reclassification  -----  
# https://www.cde.ca.gov/ds/sd/sd/filesreclass.asp

reclass <- import_files(here("data","reclass"),"files*txt","none") 


copy_to(con, reclass, name = "RECLASS",  temporary = FALSE, overwrite = TRUE)


tbl(con,"RECLASS") %>%
    count()




####  Chronic Absenteeism  -----  
# https://www.cde.ca.gov/ds/sd/sd/filesabd.asp

chronic <- import_files(here("data","chronic"),"chr*txt","none") 

chronic <- chronic  %>% 
    mutate_at(vars(CumulativeEnrollment:ChronicAbsenteeismRate ), funs(as.numeric) )  %>%
    mutate(SchoolName = iconv(enc2utf8(SchoolName),sub="byte"))


copy_to(con, chronic, name = "CHRONIC",  temporary = FALSE, overwrite = TRUE)


tbl(con,"CHRONIC") %>%
    count()






####  Staff Assign  -----  
# https://www.cde.ca.gov/ds/sd/df/filesassign.asp


staff <- import_files(here("data","staff"),"Staff*zip","none") 

staff <- staff %>% 
    mutate(SchoolName = iconv(enc2utf8(SchoolName),sub="byte"),
           ClassID    = iconv(enc2utf8(ClassID),sub="byte"))

copy_to(con, staff, name = "STAFF",  temporary = FALSE, overwrite = TRUE)




####  Course Enroll  -----  
# https://www.cde.ca.gov/ds/sd/df/filesassign.asp


courseenroll <- import_files(here("data","staff"),"CourseE*zip","none") 

courseenroll <- courseenroll %>% 
    mutate(SchoolName = iconv(enc2utf8(SchoolName),sub="byte"),
           ClassID    = iconv(enc2utf8(ClassID),sub="byte"))

copy_to(con, courseenroll, name = "COURSEENROLL",  temporary = FALSE, overwrite = TRUE)


temp <- courseenroll[2792148,]


tbl(con,"CLASSENROLL") %>%
    count()



####  Course Taught  -----  
# https://www.cde.ca.gov/ds/sd/df/filesassign.asp


coursetaught <- import_files(here("data","staff"),"CoursesT*zip","none") 

coursetaught <- coursetaught %>% 
    mutate(SchoolName = iconv(enc2utf8(SchoolName),sub="byte"),
           ClassID    = iconv(enc2utf8(ClassID),sub="byte"))

copy_to(con, coursetaught, name = "COURSETAUGHT",  temporary = FALSE, overwrite = TRUE)



####  Class Enroll  -----  
# https://www.cde.ca.gov/ds/sd/df/filesassign.asp


classenroll <- import_files(here("data","staff"),"Class*zip","none") 

classenroll <- classenroll %>% 
    mutate(SchoolName = iconv(enc2utf8(SchoolName),sub="byte"),
           ClassID    = iconv(enc2utf8(ClassID),sub="byte"),
           AcademicYear = if_else(is.na(AcademicYear),academicyear,AcademicYear)
           ) %>%
    select(-academicyear)

copy_to(con, classenroll, name = "CLASSENROLL",  temporary = FALSE, overwrite = TRUE)


####  Staff Assignment Codebook  -----  
# https://www.cde.ca.gov/ds/sd/df/filesassign.asp


codebook_assignment <- read_excel(here("data","staff","AssignmentCodes12On.xlsx") ) 
    
copy_to(con, codebook_assignment, name = "CODEBOOK_ASSIGNMENT",  temporary = FALSE, overwrite = TRUE)




####  SAT  -----   
# https://www.cde.ca.gov/ds/sp/ai/   

setwd(here("data","sat"))

files <- fs::dir_ls(glob = "sat*txt")

print(files)

sat <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(.,replace = c("_" = "" ) ,case = "snake"), id = "id"))

setwd(here())



sat <- sat %>%
    mutate_at(vars(enroll12:pctbothbenchmark), funs(as.numeric) ) %>%
    mutate_at(vars(numtsttakr12: pctbothbenchmark11), funs(as.numeric) ) %>%
    mutate(year = if_else(is.na(year),"2016-17",year))

    

copy_to(con, sat, name = "SAT",  temporary = FALSE, overwrite = TRUE)



####  ACT  -----  
# https://www.cde.ca.gov/ds/sp/ai/


setwd(here("data","act"))

files <- fs::dir_ls(glob = "act*txt")

print(files)

act <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(.,replace = c("_" = "" ) ,case = "snake"), id = "id"))

setwd(here())


act <- act %>%
    mutate_at(vars(enroll12:pctge21), funs(as.numeric) ) %>%
    mutate(year = if_else(is.na(year),"2016-17",year))
    
copy_to(con, act, name = "ACT",  temporary = FALSE, overwrite = TRUE)



####  AP  -----  
# https://www.cde.ca.gov/ds/sp/ai/


setwd(here("data","ap"))

files <- fs::dir_ls(glob = "ap*txt")

print(files)

ap <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(.,replace = c("_" = "" ) ,case = "snake"), id = "id"))

setwd(here())


ap <- ap %>%
    mutate_at(vars(enroll1012:numscr5), funs(as.numeric) ) %>%
    mutate(year = if_else(is.na(year),"2016-17",year),
           dcode = if_else(is.na(dcode),cdcode,dcode),
           studentgroup = if_else(is.na(studentgroup),"ALL" ,studentgroup)) %>%
    select(-cdcode)

copy_to(con, ap, name = "AP",  temporary = FALSE, overwrite = TRUE)



####  Dashboard  CCI -----  
# https://www.cde.ca.gov/ta/ac/cm/

setwd(here("data","dash"))

files <- fs::dir_ls(glob = "cci*txt")

print(files)

dash_cci <- map_df(files,
                   ~vroom(.x,
                          col_types = c(reportingyear = "c") ,
                          .name_repair = ~ janitor::make_clean_names(., case = "none"),
                          id = "id"))

setwd(here())


copy_to(con, dash_cci, name = "DASH_CCI",  temporary = FALSE, overwrite = TRUE)





####  Dashboard  Census Enrollment -----  
# https://www.cde.ca.gov/ta/ac/cm/


dash_census <- import_files(here("data","dash"),"cen*txt","none") 

copy_to(con, dash_census, name = "DASH_CENSUS",  temporary = FALSE, overwrite = TRUE)




####  Dashboard Chronic Absenteeism -----  
# https://www.cde.ca.gov/ta/ac/cm/


dash_chronic <- import_files(here("data","dash"),"chr*txt","none") 


dash_chronic <- dash_chronic %>%
    mutate(schoolname = iconv(enc2utf8(schoolname),sub="byte"))

copy_to(con, dash_chronic, name = "DASH_CHRONIC",  temporary = FALSE, overwrite = TRUE)




####  Dashboard ELA -----  
# https://www.cde.ca.gov/ta/ac/cm/


setwd(here("data","dash"))

files <- fs::dir_ls(glob = "ela*txt")

print(files)

dash_ela <- map_df(files[1:4],
                   ~vroom(.x,
                          .name_repair = ~ janitor::make_clean_names(., case = "none"),
                          id = "id"))

setwd(here())

dash_ela <- dash_ela %>%
    mutate(schoolname = iconv(enc2utf8(schoolname),sub="byte")) %>%
    mutate(reportingyear = if_else(is.na(ReportingYear),reportingyear,as.character( ReportingYear))) %>%
    select(-ReportingYear)




copy_to(con, dash_ela, name = "DASH_ELA",  temporary = FALSE, overwrite = TRUE)




####  Dashboard ELA Participation -----  
# https://www.cde.ca.gov/ta/ac/cm/


dash_ela_part <- import_files(here("data","dash"),"elap*txt","none") 


dash_ela_part <- dash_ela_part %>%
    mutate(schoolname = iconv(enc2utf8(schoolname),sub="byte"))

copy_to(con, dash_ela_part, name = "DASH_ELA_PART",  temporary = FALSE, overwrite = TRUE)



####  Dashboard Grad -----  
# https://www.cde.ca.gov/ta/ac/cm/


setwd(here("data","dash"))

files <- fs::dir_ls(glob = "grad*txt")

print(files)

dash_grad <- map_df(files,
                   ~vroom(.x,
                          col_types = c(reportingyear = "c") ,
                          .name_repair = ~ janitor::make_clean_names(., case = "none"),
                          id = "id"))

setwd(here())




dash_grad <- dash_grad %>%
    mutate(schoolname = iconv(enc2utf8(schoolname),sub="byte"))

copy_to(con, dash_grad, name = "DASH_GRAD",  temporary = FALSE, overwrite = TRUE)



####  Dashboard Math -----  
# https://www.cde.ca.gov/ta/ac/cm/


setwd(here("data","dash"))

files <- fs::dir_ls(glob = "math*txt")

print(files)

dash_math <- map_df(files[1:4],
                   ~vroom(.x,
                          .name_repair = ~ janitor::make_clean_names(., case = "none"),
                          id = "id"))

setwd(here())

dash_math <- dash_math %>%
    mutate(schoolname = iconv(enc2utf8(schoolname),sub="byte")) %>%
    mutate(reportingyear = if_else(is.na(ReportingYear),reportingyear,as.character( ReportingYear))) %>%
    select(-ReportingYear)




copy_to(con, dash_math, name = "DASH_MATH",  temporary = FALSE, overwrite = TRUE)



####  Dashboard Math Participation -----  
# https://www.cde.ca.gov/ta/ac/cm/


dash_math_part <- import_files(here("data","dash"),"mathp*txt","none") 


dash_math_part <- dash_math_part %>%
    mutate(schoolname = iconv(enc2utf8(schoolname),sub="byte"))

copy_to(con, dash_math_part, name = "DASH_MATH_PART",  temporary = FALSE, overwrite = TRUE)




####  Dashboard Local Priorities -----  
#  SKipping for now.  Files to variable

# 
# local <- import_files(here("data","dash"),"Local*txt","none") 
# 
# pr <- import_files(here("data","dash"),"Pr*txt","none") 
# setwd(here("data","dash"))
# 
# files <- fs::dir_ls(glob = "Pr*txt")
# 
# print(files)
# 
# pr <- map_df(files,
#                     ~read_delim(.x,
#                            col_types = c(CDSCode = "c",
#                                          priority_number = "c",
#                                          priorityNumber = "c",
#                                          meetingDate = "D",
#                                          meeting_date = "D",
#                                          year = "d",
#                                          ActivityGroupNeeds = "c",
#                                          ActivityIndividualNeeds = "c",
#                                          ActivitySupport = "c" ,
#                                             activity_group_needs = "c",
#                                          activity_individual_needs = "c",
#                                          activity_support = "c") ,
#   #                         .name_repair = ~ janitor::make_clean_names(., case = "none"),
#                            delim = "|",
#                            # id = "id"
#   ))
# 
# setwd(here())
# 
# 
# pr2 <- pr %>%
#     clean_names()
# 
# 
# pr3 <- pr2 %>%
#     unite("cds_code", c(cds_code,cds_code_2), na.rm = TRUE, remove = TRUE) %>%
#     unite("lea", c(lea,lea_2), na.rm = TRUE, remove = TRUE) %>%
#     unite("priority_number", c(priority_number,priority_number_2), na.rm = TRUE, remove = TRUE) %>%
#     unite("year", c(school_year,year, year_2), na.rm = TRUE, remove = TRUE) %>%
#     unite("performance", c(performance,performance_2), na.rm = TRUE, remove = TRUE) %>%
#     unite("policy_dev", c(policy_dev,policy_dev_2), na.rm = TRUE, remove = TRUE)
#     



####  Dashboard Suspension -----  
# https://www.cde.ca.gov/ta/ac/cm/

setwd(here("data","dash"))
files <- fs::dir_ls(glob = "susp*txt")
print(files)
dash_susp <- map_df(files,
                    ~vroom(.x,
                           col_types = c(reportingyear = "c") ,
                           .name_repair = ~ janitor::make_clean_names(., case = "none"),
                           id = "id"))
setwd(here())



dash_susp <- dash_susp %>%
    mutate(schoolname = iconv(enc2utf8(schoolname),sub="byte"))

copy_to(con, dash_susp, name = "DASH_SUSP",  temporary = FALSE, overwrite = TRUE)




####  CAASPP -----  
# https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList?ps=true&lstTestType=B&lstTestYear=2015&lstCounty=00&lstCntyNam=Select%20County...&lstFocus=btnApplySelections


setwd(here("data","caaspp"))
files <- fs::dir_ls(glob = "sb*txt")
print(files)
caaspp <- map_df(files,
                    ~vroom(.x,
                           col_types = c("Total Tested At Entity Level" = "c",
                                         "CAASPP Reported Enrollment" = "c",
                                         "Students Tested" = "c",
                                         "Students with Scores" = "c",
                                         "Total Tested with Scores" = "c") ,
                           .name_repair = ~ janitor::make_clean_names(., case = "none"),
                            id = "id"
  ))
setwd(here())


copy_to(con, caaspp, name = "CAASPP",  temporary = FALSE, overwrite = TRUE)



####  Initial ELPAC -----  
# https://caaspp-elpac.cde.ca.gov/elpac/ResearchFilesIA?ps=true&lstTestYear=2019&lstTestType=IA&lstGroup=1&lstSubGroup=001&lstGrade=13&lstCounty=00&lstDistrict=00000&lstSchool=0000000#dl




setwd(here("data","elpac"))
files <- fs::dir_ls(glob = "ia*zip")
print(files)
ielpac <- map_df(files,
                 ~vroom(.x,
                        delim = "^",
                        .name_repair = ~ janitor::make_clean_names(., case = "none"),
                        id = "id"
                 ))
setwd(here())


ielpac <- ielpac %>% 
  mutate_at(vars(TotalEnrolled:TotalTestedWithScores,OverallMeanSclScr:WritLangTotal), funs(as.numeric) ) 


copy_to(con, ielpac, name = "IELPAC",  temporary = FALSE, overwrite = TRUE)



###  Summative ELPAC -----  
# https://caaspp-elpac.cde.ca.gov/elpac/ResearchFilesSA?ps=true&lstTestYear=2019&lstTestType=SA&lstGroup=1&lstSubGroup=001&lstGrade=13&lstCounty=00&lstDistrict=00000&lstSchool=0000000#dl


carotfile <- read_delim(here("data","elpac","sa_elpac2019_all_csv_v2.zip"),
                        delim = "^")
write_delim(carotfile,
            here("data","elpac","sa_elpac2019_all_csv_v2carot.txt"),
            delim = ",")


setwd(here("data","elpac"))
files <- fs::dir_ls(glob = "sa*txt")
print(files)
selpac <- map_df(files,
                 ~vroom(.x,
                        delim = ",",
                        .name_repair = ~ janitor::make_clean_names(., case = "none"),
                        id = "id"
                 ))
setwd(here())


selpac <- selpac %>% 
  mutate_at(vars(TotalEnrolled:TotalTested,OverallMeanSclScr:TotalTestedWithScores), funs(as.numeric) ) 


copy_to(con, selpac, name = "SELPAC",  temporary = FALSE, overwrite = TRUE)



####  CAST -----  
# https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileListCAST?ps=true&lstTestYear=2019&lstTestType=X&lstGroup=1&lstGrade=13&lstSchoolType=A&lstCounty=00&lstDistrict=00000&lstSchool=0000000#dl



cast <- import_files(here("data","cast"),"cast*txt","none") 


cast <- cast %>% 
  mutate_at(vars(Total_Number_Tested_at_Entity_Level_and_Demographic,
                 Total_Number_Tested_at_this_Demographic_with_Valid_Scores,
                 CAST_Reported_Enrollment:Earth_and_Space_Sciences_Domain_Percent_Above_Standard),
            funs(as.numeric) ) 


copy_to(con, cast, name = "CAST",  temporary = FALSE, overwrite = TRUE)


