
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


all_animals <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5b_Greg_time_step_dist_travelled.csv")

all_animals$local_time <- as.POSIXct(all_animals$local_time,  tz = "Australia/Adelaide")



################################################################################
####    remove the animals tha dont have collars     ###########################
################################################################################

list_of_sheep_VF <- c(2,3,5,13,17,22,30,35,
                      12,23,25,
                      10,15,21,27,33,36)

all_animals_collars <- all_animals %>% filter(sheep == list_of_sheep_VF)
str(all_animals_collars)
all_animals_collars %>%  group_by(treatment) %>% distinct(sheep) %>% arrange(treatment  )

###############################################################################
#### create a ID varaible if the animal is in the exclusion zone or not  #####
### prehaps something like time spent in the VF ####
str(all_animals_collars)
all_animals_collars$VF_EX <- as.factor(all_animals_collars$VF_EX )
unique(all_animals$VF_EX)



### this is not working like i want it to
test <- all_animals_collars %>%  group_by(sheep, VF_EX, date) %>% 
  summarise(VF_EX = n())

unique(all_animals$VF_EX)

control <- all_animals %>%  filter(treatment == "control")
percent33 <- all_animals %>%  filter(treatment == "33_percent")
percent100 <- all_animals %>%  filter(treatment == "100_percent")
percent66 <- all_animals %>%  filter(treatment == "66_percent")