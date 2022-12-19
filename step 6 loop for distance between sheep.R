library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(readxl)

  
GPS_Sheep <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5b_Greg_time_step_dist_travelled.csv")
#GPS_Sheep <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5_Greg_time_step_dist_travelled.csv") 
GPS_Sheep$local_time <- as.POSIXct(GPS_Sheep$local_time,  tz = "Australia/Adelaide")          
GPS_Sheep$time_step <- as.POSIXct(GPS_Sheep$time_step,  tz = "Australia/Adelaide") 

str(GPS_Sheep)
names(GPS_Sheep)

reg_time_step <- GPS_Sheep %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)


list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Waikerie/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")

#list_of_comparsions <- list_of_comparsions %>%  filter(is.na(replication)) 
list_of_comparsions <- list_of_comparsions %>%  filter(is.na(problem))

##---- select---###
list_of_comparsions_x <- list_of_comparsions %>%  filter(treatment == "control") #0.66 and 0.33 1 control
sheep_list_x <- list_of_comparsions_x



sheep_list_x <- as.list(sheep_list_x)
sheep_list_x <- c(sheep_list_x$comparison)

sheep_list <- sheep_list_x


df <- reg_time_step %>%  arrange(time_step)

for (sheep_list in sheep_list){
  
  comparison_a_b <- as.data.frame(str_split(sheep_list, "vs"),
                               col.names = "Sheep_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]

  GPS_Sheep_comparsion_a <- GPS_Sheep %>% filter(sheep == comparison_a) %>% mutate(comparison_label = "a")
  GPS_Sheep_comparsion_b <- GPS_Sheep %>% filter(sheep == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_Sheep_comparsion <- rbind(GPS_Sheep_comparsion_a, GPS_Sheep_comparsion_b)
  
  GPS_Sheep_comparsion <- GPS_Sheep_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  str(GPS_Sheep_comparsion)
  
  ## make it wide
  GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
    GPS_Sheep_comparsion_wide
  
    
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>% 
      dplyr::mutate(
    dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))
    
  
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>%  dplyr::select(time_step , dist)
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>%
        rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
    
    df = left_join(df, GPS_Sheep_comparsion_wide)
    df <- df %>% arrange(time_step)
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


#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_100percent.csv")
#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_33percent.csv")
write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_control.csv")
#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_66.csv")


### missing number 19 not sure why?