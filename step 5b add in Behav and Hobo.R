################################################################################
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
################################################################################

#Bring in the behavioual data and HOBO data and merge to step 5

################################################################################
#### --------------    Bring in data   -------------- ####
################################################################################

all_animals <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5_Greg_time_step_dist_travelled.csv")
names(all_animals)
all_animals <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5_Greg_time_step_dist_travelled.csv")


################################################################################
#### --------------    Bring in Behavioural data   -------------- ####
################################################################################


behav <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/Behavioural.csv")
str(behav)
behav <- behav %>% select("date_time", "treatment",  "Sheep Cued" ,  "Cue"   )
behav <- behav %>% rename(sheep = "Sheep Cued" )

##round the time
behav <- behav %>% 
  dplyr::mutate(round_local_time =round_date(date_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep) )



################################################################################
#### --------------    Bring in Hobo data   -------------- ####
################################################################################

hobo <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/HOBO_VFtest.csv")
str(hobo)

hobo <- hobo %>% select("Date Time", "treatment",  "sheep" ,  "Est.State"   )
hobo <- hobo %>% rename(date_time = "Date Time" )
##round the time
hobo <- hobo %>% 
  dplyr::mutate(round_local_time =round_date(date_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep) )


hobo <- 

###############################################################################
#### --------------    Merge Hobo and Beha data   -------------- ####
################################################################################


