#ReadSchools ####
library(readxl)
library(naniar)
library(tidyverse)
library(visdat)
library(sf)
library(sfarrow)
# Whenever possible, begin your script with a tribble that you will need later on ####
## *altho did not use this tribble*

fipskey2statekey <- tribble(
  ~ ANSIKEY, ~STATEKEY,
  "01","AL",
  "02","AK",
  "04","AZ",
  "05","AR",
  "06","CA",
  "08","CO",
  "09","CT",
  "10","DE",
  "11","DC",
  "12","FL",
  "13","GA",
  "15","HI",
  "16","ID",
  "17","IL",
  "18","IN",
  "19","IA",
  "20","KS",
  "21","KY",
  "22","LA",
  "23","ME",
  "24","MD",
  "25","MA",
  "26","MI",
  "27","MN",
  "28","MS",
  "29","MO",
  "30","MT",
  "31","NE",
  "32","NV",
  "33","NH",
  "34","NJ",
  "35","NM",
  "36","NY",
  "37","NC",
  "38","ND",
  "39","OH",
  "40","OK",
  "41","OR",
  "42","PA",
  "44","RI",
  "45","SC",
  "46","SD",
  "47","TN",
  "48","TX",
  "49","UT",
  "50","VT",
  "51","VA",
  "53","WA",
  "54","WV",
  "55","WI",
  "56","WY",
  "60","AS",
  "66","GU",
  "69","MP",
  "72","PR",
  "74","UM",
  "78","VI"
)

# # Load Schools geography files ####
## Finding these files is up to you

Public2022 <- read_csv("Data/Public_School_Locations_20220628_Current.csv")
Private2022 <- read_csv("Data/Private_School_Locations_20220628_Current.csv")



pubgeo <- Public2022 |> 
    select(key=NCESSCH, name=NAME, state=STATE, county=CNTY, latitude=Y, longitude=X, SCHOOLYEAR, CD)
pubgeo$typePP <- "PUB"


vis_dat(pubgeo, warn_large_data = FALSE)  # make sure they are all there  (looking good)

pubstates <- pubgeo |> filter( state != 'VI' & state !='PR'  & state !='MP' & state !='GU'  & state !='AS') 

nrow(pubgeo)
nrow(pubstates)
unique(pubstates$state)

privgeo <- Private2022 |> 
  select(key=PPIN, name=NAME, state=STATE, county=CNTY, latitude=Y, longitude=X, SCHOOLYEAR, CD) 
privgeo$typePP <- "PRIV"

unique(pubgeo$state)
unique(privgeo$state)
# test the state code improvement
wytestpubgeo <- c("560123")

schoolscombo <- full_join(pubstates, privgeo)
nrow(schoolscombo)
foreign::write.dbf(as.data.frame(schoolscombo),"allschoolsnow.dbf",factor2char = TRUE)

view(schoolscombo)
nrow(pubstates)

view(pubstates)
quit("finish K12")

# Puerto Rico ####
pubgeo %>% filter(state=="PR") -> prschools
# look at it (just for grins)
plot(prschools$longitude,prschools$latitude)

foreign::write.dbf(as.data.frame(prschools), "prschools2021.dbf", factor2char = TRUE)

