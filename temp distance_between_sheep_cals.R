library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

GPS_Sheep2_3 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/GPS_sheep2_3_reg_time_step_dist.csv") 
GPS_Sheep2_3$local_time <- as.POSIXct(GPS_Sheep2_3$local_time,  tz = "Australia/Adelaide")          
GPS_Sheep2_3$time_step <- as.POSIXct(GPS_Sheep2_3$time_step,  tz = "Australia/Adelaide") 

str(GPS_Sheep2_3)
names(GPS_Sheep2_3)
GPS_Sheep2_3 <- GPS_Sheep2_3 %>% 
  dplyr::select(time_step, Time_sheep, ID, sheep, X, Y )
GPS_Sheep2 <- GPS_Sheep2_3 %>% filter(sheep ==2) %>% 
  rename(X_sheep2 = X, 
         Y_sheep2 = Y) %>% 
  dplyr::select(- Time_sheep, 
                -ID, 
                -sheep)

GPS_Sheep3 <- GPS_Sheep2_3 %>% filter(sheep ==3) %>% 
  rename(X_sheep3 = X, 
         Y_sheep3 = Y)%>% 
  dplyr::select(- Time_sheep, 
                -ID, 
                -sheep)

GPS_Sheep2_3_matrix <- full_join(GPS_Sheep2, GPS_Sheep3)

names(GPS_Sheep2_3_matrix)

################################################################################
#### Do some cals  distance between sheep 2 and 3  ---- ####
################################################################################
GPS_Sheep2_3_matrix <- GPS_Sheep2_3_matrix %>% 
  arrange(time_step)

GPS_Sheep2_3_matrix <- GPS_Sheep2_3_matrix %>% 
  dplyr::mutate(dist_2to3 = sqrt(  ((X_sheep2 - X_sheep3)^ 2) + (Y_sheep2 - Y_sheep3)^ 2) )


################################################################################
#### What is the center location of the animals for each time point?
#### looks like you might be able to take an average of points using group by
#### the data should be long , group by time step.
### but will need to remove pts that are too far away - not sure what this would be
### also need to think about the cords and projects - not sure if there is a trick here??
################################################################################

#https://stackoverflow.com/questions/65379274/calculate-a-centre-point-of-multiple-lat-long-points-in-a-data-frame
