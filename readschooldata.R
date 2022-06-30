#readschooldata.r
### VERY rough run-through.  It is after all, "1a"

## read the schools data & associate it with the geography objects generated
## in readgeos.R, which must be run first in the same session.

library(tidyverse)
# and using a call from library(foreign) for .dbf, which must be install.packages("foreign)

# Public schools read 5 data files ####

# NOTE in order to do this, some summarization must occur.
# directory
ccd_sch_029_2021_w_1a_080621 <- read_csv("Data/ccd_sch_029_2021_w_1a_080621.csv")
glimpse(ccd_sch_029_2021_w_1a_080621)
draw <- ccd_sch_029_2021_w_1a_080621 

directory <- draw %>%
  select(key=NCESSCH, school=SCH_NAME, type=SCH_TYPE_TEXT, GSLO, GSHI, LEVEL)
nrow(directory)
#membership
ccd_SCH_052_2021_l_1a_080621 <- read_csv("Data/ccd_SCH_052_2021_l_1a_080621.csv")
#View(ccd_sch_052_1516_w_2a_011717)
nrow(ccd_SCH_052_2021_l_1a_080621)

mraw <- ccd_SCH_052_2021_l_1a_080621

membership <- mraw |> 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) |>
  group_by(key=NCESSCH) |> 
  filter(TOTAL_INDICATOR=='Education Unit Total') |> 
  summarize(students=sum(STUDENT_COUNT))

sum(membership$students)
View(membership)
# membership <- mraw |> 
#   select(key=NCESSCH,  STUDENT_COUNT, MEMBER, AE)
# PK,KG,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12,G13,UG,AE,

ccd_sch_059_2021_l_1a_080621 <- read_csv("Data/ccd_sch_059_2021_l_1a_080621.csv")
#View(ccd_sch_059_1516_w_2a_011717)                                                    
staffraw <- ccd_sch_059_2021_l_1a_080621
unique(staffraw$TOTAL_INDICATOR)

staff <-  staffraw %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) |>
  select(key=NCESSCH, teachers=TEACHERS)
sum(staff$teachers)  

ccd_sch_129_2021_w_1a_080621 <- read_csv("Data/ccd_sch_129_2021_w_1a_080621.csv")

#View(ccd_sch_129_1516_w_2a_011717)
schoolraw <- ccd_sch_129_2021_w_1a_080621 

school <- schoolraw |>  
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) |>
  select(key = NCESSCH, magnet = MAGNET_TEXT, NSLP = NSLP_STATUS_TEXT, virtual=VIRTUAL_TEXT)
  
ccd_sch_033_2021_l_1a_080621 <- read_csv("Data/ccd_sch_033_2021_l_1a_080621.csv")
#View(ccd_sch_033_1516_w_2a_011717)

lunch <- ccd_sch_033_2021_l_1a_080621 %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) |>
  select (key = NCESSCH, lunchcount = STUDENT_COUNT, lunchpro = LUNCH_PROGRAM)
sum(lunch$lunchcount)

# Join the data Five files  ####
nrow(membership)
nrow(staff)
nrow(school)
nrow(lunch) ## Still defective
nrow(directory)
pub21data <- inner_join(membership, staff, by="key") %>%
  inner_join(directory, by="key") |> 
  inner_join(school, by="key") %>%
#  inner_join(lunch, by="key") %>%
  inner_join(directory, by="key")

glimpse(pub21data)
nrow(pub21data)

pub21z <- pub21data %>% select(key,students,teachers,name=school.x,
                               type=type.x,GSLO=GSLO.x,
                               GSHI=GSHI.x,magnet,NSLP,virtual) |> 
  mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) |> 
  mutate_if(is.character, funs(replace(., is.na(.), 0)))

pub21z$students <- as.integer(pub21z$students)   ## It's a count, whilst the teachers are FTE


## Combine the data with the geography and write to DBF  ####
publicschools21 <- pubstates |>  inner_join(pub21z, by="key") |> 
  rename(name=name.x) |> select(-name.y)

nrow(publicschools21)
View(publicschools21)  ## always check yo data

## check out some school stats ####

totalstudents <- sum(publicschools21$students)
totalfte <- sum(publicschools21$teachers)

(student_teacher_ratio <- totalstudents/totalfte)

totalstudents
totalfte

library(foreign)
foreign::write.dbf(as.data.frame(publicschools21), "USpublicschools2122.dbf", factor2char = FALSE)

