library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

GPS_Sheep <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/GPS_sheep_all_reg_time_step_dist.csv") 
GPS_Sheep$local_time <- as.POSIXct(GPS_Sheep$local_time,  tz = "Australia/Adelaide")          
GPS_Sheep$time_step <- as.POSIXct(GPS_Sheep$time_step,  tz = "Australia/Adelaide") 

str(GPS_Sheep)
names(GPS_Sheep)

reg_time_step <- GPS_Sheep %>% 
  distinct(time_step)

###############################################################################  
## ----- function to produce distance between animal matrix
################################################################################

sheep_list <- GPS_Dist %>% distinct(sheep) %>%  arrange(sheep)
sheep_list <- c(1:36)
#sheep_list <- c(1:3)

### as a function
for (sheep_list in sheep_list){
  
  
  GPS_Sheep_per_animal <- GPS_Sheep %>% 
    dplyr::select(time_step, Time_sheep, ID, sheep, X, Y )
  GPS_Sheep_per_animal <- GPS_Sheep_per_animal %>% filter(sheep == sheep_list) 
  
   GPS_Sheep_per_animal <- GPS_Sheep_per_animal %>%
     rename(!!paste0('X_sheep', sheep_list) := X,
            !!paste0('Y_sheep', sheep_list) := Y)
           
  
  GPS_Sheep_per_animal <- GPS_Sheep_per_animal %>%
    dplyr::select(- Time_sheep, 
                  -ID, 
                  -sheep)
  name <- paste0("GPS_Sheep_per_animal", sheep_list)
  assign(name,GPS_Sheep_per_animal)
  
}       
  

reg_time_step
GPS_Sheep_per_animal1


GPS_Sheep_per_animal_all <-list(reg_time_step, 
            GPS_Sheep_per_animal1,
            GPS_Sheep_per_animal2,
            GPS_Sheep_per_animal3,
            GPS_Sheep_per_animal4,
            GPS_Sheep_per_animal5,
            GPS_Sheep_per_animal6,
            GPS_Sheep_per_animal7,
            GPS_Sheep_per_animal8,
            GPS_Sheep_per_animal9,
            GPS_Sheep_per_animal10,
            GPS_Sheep_per_animal11,
            GPS_Sheep_per_animal12,
            GPS_Sheep_per_animal13,
            GPS_Sheep_per_animal14,
            GPS_Sheep_per_animal15,
            GPS_Sheep_per_animal16,
            GPS_Sheep_per_animal17,
            GPS_Sheep_per_animal18,
            GPS_Sheep_per_animal19,
            GPS_Sheep_per_animal20,
            GPS_Sheep_per_animal21,
            GPS_Sheep_per_animal22,
            GPS_Sheep_per_animal23,
            GPS_Sheep_per_animal24,
            GPS_Sheep_per_animal25,
            GPS_Sheep_per_animal26,
            GPS_Sheep_per_animal27,
            GPS_Sheep_per_animal28,
            GPS_Sheep_per_animal29,
            GPS_Sheep_per_animal30,
            GPS_Sheep_per_animal31,
            GPS_Sheep_per_animal32,
            GPS_Sheep_per_animal33,
            GPS_Sheep_per_animal34,
            GPS_Sheep_per_animal35,
            GPS_Sheep_per_animal36) %>% 
  reduce(full_join, by = "time_step")

### remove the temp files

rm(GPS_Sheep_per_animal1,
   GPS_Sheep_per_animal2,
   GPS_Sheep_per_animal3,
   GPS_Sheep_per_animal4,
   GPS_Sheep_per_animal5,
   GPS_Sheep_per_animal6,
   GPS_Sheep_per_animal7,
   GPS_Sheep_per_animal8,
   GPS_Sheep_per_animal9,
   GPS_Sheep_per_animal10,
   GPS_Sheep_per_animal11,
   GPS_Sheep_per_animal12,
   GPS_Sheep_per_animal13,
   GPS_Sheep_per_animal14,
   GPS_Sheep_per_animal15,
   GPS_Sheep_per_animal16,
   GPS_Sheep_per_animal17,
   GPS_Sheep_per_animal18,
   GPS_Sheep_per_animal19,
   GPS_Sheep_per_animal20,
   GPS_Sheep_per_animal21,
   GPS_Sheep_per_animal22,
   GPS_Sheep_per_animal23,
   GPS_Sheep_per_animal24,
   GPS_Sheep_per_animal25,
   GPS_Sheep_per_animal26,
   GPS_Sheep_per_animal27,
   GPS_Sheep_per_animal28,
   GPS_Sheep_per_animal29,
   GPS_Sheep_per_animal30,
   GPS_Sheep_per_animal31,
   GPS_Sheep_per_animal32,
   GPS_Sheep_per_animal33,
   GPS_Sheep_per_animal34,
   GPS_Sheep_per_animal35,
   GPS_Sheep_per_animal36)




###---- up to here ---- ###
################################################################################
#### Do some cals  distance between sheep  ---- ####
################################################################################
GPS_Sheep_per_animal_all <- GPS_Sheep_per_animal_all %>% 
  arrange(time_step)

names(GPS_Sheep_per_animal_all)

GPS_Sheep2_3_matrix <- GPS_Sheep2_3_matrix %>% 
  dplyr::mutate(
    dist_1to2 = sqrt(  ((X_sheep1 - X_sheep2)^ 2) + (Y_sheep1 - Y_sheep2)^ 2),
    dist_1to2 = sqrt(  ((X_sheep1 - X_sheep2)^ 2) + (Y_sheep1 - Y_sheep2)^ 2),
    dist_1to2 = sqrt(  ((X_sheep1 - X_sheep2)^ 2) + (Y_sheep1 - Y_sheep2)^ 2),
    dist_1to2 = sqrt(  ((X_sheep1 - X_sheep2)^ 2) + (Y_sheep1 - Y_sheep2)^ 2),
    dist_1to2 = sqrt(  ((X_sheep1 - X_sheep2)^ 2) + (Y_sheep1 - Y_sheep2)^ 2)
    
    
    
    
    )


################################################################################
#### What is the center location of the animals for each time point?
#### looks like you might be able to take an average of points using group by
#### the data should be long , group by time step.
### but will need to remove pts that are too far away - not sure what this would be
### also need to think about the cords and projects - not sure if there is a trick here??
################################################################################

#https://stackoverflow.com/questions/65379274/calculate-a-centre-point-of-multiple-lat-long-points-in-a-data-frame
