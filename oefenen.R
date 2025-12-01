dir.create("data_output")
dir.create("fig_output")
dir.create("data")
library(readxl)
library(readr)
# Load if using RStudio (interactive session)
library(rstudioapi)
download.file(
  "https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/SAFI_clean.csv",
  "data/SAFI_clean.csv", mode = "wb"
)


area_hectares <- 2.5.0
(area_hectares <- 2.5)
area_hectares
2.47 * area_hectares
area_hectares <- 2.5
area_hectares
2.47 *area_hectares
area_acres <- 2.47*area_hectares
area_hectares <- 50
r_length <- 2
r_width <- 3
r_area <-  r_length * r_width
r_area
round(3.23443, digits =2)
hh_members <-c(3,7,10,6)
hh_members
respondent_wall_type <- c("muddaub","burntbricks","sunbricks")
respondent_wall_type
length(hh_members)
typeof(hh_members)
typeof(respondent_wall_type)
possessions <- c("bicycle","radio","television")
possessions <- c("car", possessions)
possessions <- c(possessions, "mobile_phone")
possessions
num_char <- c(1,2,3,"a")
num_logical <- c("a","b","c", TRUE)
char_logical <- c("a","b","c", TRUE)
tricky <- c(1,2,3,"4")
num_char
num_logical
char_logical
tricky

combined_logical <- c(num_logical,char_logical)
combined_logical
respondent_wall_type[c(3,2)]
more_respondent_wall_type <- respondent_wall_type[c(1,2,3,2,1,3)]
more_respondent_wall_type
hh_members > 5
hh_members[hh_members>5]
hh_members[hh_members >= 4 & hh_members <= 7]
possessions %in% c("car", "bicycle")
possessions %in% c()
rooms <- c(2,1,1,NA,7)
mean(rooms)
max(rooms)
max(rooms, na.rm=TRUE)
rooms[!is.na(rooms)]
sum(is.na(rooms))
read_csv()
(library(tidyverse))
install.packages("conflicted")
conflicted::conflict_scout()
conflict_prefer("function","package_prefered")
stas::filter()
install.packages("here")
library(tidyverse)
library(here)
interviews <- read_csv(here("data", "SAFI_clean.csv"),na = "NULL")
interviews
class(interviews)
dim(interviews)
nrow(interviews)
ncol(interviews)
head(interviews)
tail(interviews)
glimpse(interviews)
interviews[1,6]
interviews[[1]]
memb_assoc <- interviews$memb_assoc
memb_assoc <-  as.factor(memb_assoc)
plot(memb_assoc)
memb_assoc <- interviews$memb_assoc
memb_assoc[is.na(memb_assoc)] <- "undetermined"
memb_assoc <- as.factor(memb_assoc)
memb_assoc
plot(memb_assoc)
