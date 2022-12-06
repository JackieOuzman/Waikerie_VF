library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)

  
GPS_Sheep <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/GPS_sheep_all_reg_time_step_dist.csv") 
GPS_Sheep$local_time <- as.POSIXct(GPS_Sheep$local_time,  tz = "Australia/Adelaide")          
GPS_Sheep$time_step <- as.POSIXct(GPS_Sheep$time_step,  tz = "Australia/Adelaide") 

str(GPS_Sheep)
names(GPS_Sheep)

reg_time_step <- GPS_Sheep %>% 
  distinct(time_step)
df <- reg_time_step

sheep_list <- c("2vs3",
               "3vs2",
               "5vs2",
               "13vs2",
               "14vs2",
               "17vs2",
               "22vs2",
               "30vs2",
               "35vs2",
               "3vs2",
               "5vs2",
               "13vs2",
               "14vs2",
               "17vs2",
               "22vs2",
               "30vs2",
               "35vs2",
               "13vs5",
               "14vs5",
               "17vs5",
               "22vs5",
               "30vs5",
               "35vs5",
               "14vs13",
               "17vs13",
               "22vs13",
               "30vs13",
               "35vs13",
               "17vs14",
               "22vs14",
               "30vs14",
               "35vs14",
               "22vs17",
               "30vs17",
               "35vs17",
               "30vs22",
               "35vs22",
               "35vs30"
)
for (sheep_list in sheep_list){
  
  comparison_a_b <- as.data.frame(str_split(sheep_list, "vs"),
                               col.names = "Sheep_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]

  GPS_Sheep_comparsion_a <- GPS_Sheep %>% filter(sheep == comparison_a) %>% mutate(comparison_label = "a")
  GPS_Sheep_comparsion_b <- GPS_Sheep %>% filter(sheep == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_Sheep_comparsion <- rbind(GPS_Sheep_comparsion_a, GPS_Sheep_comparsion_b)
  
  GPS_Sheep_comparsion <- GPS_Sheep_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  GPS_Sheep_comparsion
  
  ## make it wide
  GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
    GPS_Sheep_comparsion_wide
  
    
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>% 
      dplyr::mutate(
    dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))#,
    #comparison = paste0(comparison_a,"vs", comparison_b))
  
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>%  dplyr::select(time_step , dist)
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>%
      #rename(paste0("dist",comparison_a,"vs", comparison_b)= dist)
      rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
    
    df = left_join(df, GPS_Sheep_comparsion_wide)
    rm(
      comparison_a,
      comparison_b,
      comparison_a_b,
      GPS_Sheep_comparsion_a,
      GPS_Sheep_comparsion_b,
      GPS_Sheep_comparsion,
      GPS_Sheep_comparsion_wide
    )  
}    
