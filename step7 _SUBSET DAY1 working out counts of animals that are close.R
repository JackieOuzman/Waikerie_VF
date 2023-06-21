library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library("readxl")


#####################################################################################
### number of other animals close to animal min dist is 1.5 - this can be changed ###
#####################################################################################


## we have 4 treatments 66,33,100,control
Min_dist <-1.5 #the filter for the min distance between animals



list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Waikerie/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "dist_between_animals")
##sheep 19 has no data so remove it
list_of_comparsions <- list_of_comparsions %>%  filter(is.na(problem)) 
list_of_comparsions$sheep <- as.character(list_of_comparsions$sheep)
list_of_comparsions <- list_of_comparsions %>% mutate(comparison = paste0("dist",comparison))

### ---- for treatment      ----####
#list_of_comparsions <- list_of_comparsions %>%  filter(treatment == "0.66") 
#list_of_comparsions <- list_of_comparsions %>%  filter(treatment == "0.33") 
list_of_comparsions <- list_of_comparsions %>%  filter(treatment == "1") 
#list_of_comparsions <- list_of_comparsions %>%  filter(treatment == "control")



list_of_sheep <- list_of_comparsions
list_of_sheep <- list_of_sheep %>%  distinct(sheep, .keep_all = TRUE)
list_of_sheep <- as.list(list_of_sheep)
list_of_sheep <- c(list_of_sheep$sheep )



### ---- for treatment      ----####
#matrix <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_66.csv") 
#matrix <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_33percent.csv")
matrix <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_100percent.csv") 
#matrix <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_control.csv") 

reg_time_step <- matrix %>%   distinct(time_step) #creates a df with regular time step


### ---- make the reg_time_step smaller to reflect the start of the trial      ----####
str(reg_time_step)
#for 66 and 33 we only want to retain day 1 = 15/3/2019 and 100% retain day 1 = 13/3/2019 

reg_time_step <- reg_time_step %>% 
   mutate(date = date(time_step))

#reg_time_step <- reg_time_step %>%  filter(date == "2018-03-15")#for 66 and 33
reg_time_step <- reg_time_step %>%  filter(date == "2018-03-13")#for 100

df <- data.frame(reg_time_step=POSIXct(),
                 numb_sheep_close=integer(),
                 sheep=character()) #df for the loop to write to






### loop use a list of sheep as a variable to create a list and the list becomes the input for the loop
for (list_of_sheep in list_of_sheep){
  

sheep_list_df_x <- list_of_comparsions %>%  filter(sheep == list_of_sheep) %>% select(comparison)
sheep_list_x <- as.list(sheep_list_df_x)
sheep_list_comparisonx <- c(sheep_list_x$comparison)
names(matrix)
sheep_x <-   matrix %>%  dplyr::select(all_of(sheep_list_comparisonx)) #remove the time step and only select clms that relate to the sheep

sheep_x[sheep_x > Min_dist] <- NA #replace values >1.5 with NA

sheep_x <- sheep_x %>%  
  dplyr::mutate(count = ncol(sheep_x) - rowSums(is.na(sheep_x) | sheep_x == ""))  %>% 
  dplyr::select(count )

sheep_matrix <-   matrix %>%  dplyr::select(time_step)
sheep_matrix <- cbind(sheep_matrix, sheep_x)


sheep_matrix <- sheep_matrix %>%
  rename(!!paste0(paste0("numb_sheep_close")) := count) %>% 
  mutate(sheep = list_of_sheep)
 

df_temp = left_join(reg_time_step, sheep_matrix)

df <- rbind(df, df_temp) 

}    


#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_66percent_DOT1.csv", row.names = FALSE)
#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_33percent_DOT1.csv", row.names = FALSE)
write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_100percent_DOT1.csv", row.names = FALSE)
#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_control_DOT1.csv", row.names = FALSE)
