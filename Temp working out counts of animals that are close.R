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

### ---- start with 66 ----####

list_of_comparsions <- read_excel("W:/VF/Optimising_VF/Waikerie/data_prep/list of animal comparisons.xlsx", 
                                  sheet = "66 percent per sheep comaprsion")
##sheep 19 has no data so remove it
list_of_comparsions <- list_of_comparsions %>%  filter(is.na(problem)) 
list_of_comparsions$sheep <- as.character(list_of_comparsions$sheep)


matrix_66percent <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_66.csv") 


reg_time_step <- matrix_66percent %>%   distinct(time_step) #creates a df with regular time step

df <- data.frame(reg_time_step=POSIXct(),
                 numb_sheep_close=integer(),
                 sheep=character()) #df for the loop to write to


### ---- for treatment = 66 % for sheep7 ----####
#list_of_sheep <- "7"
#list_of_sheep <- "10"
list_of_sheep <- c("7","10")



### loop use a list of sheep as a variable to create a list and the list becomes the input for the loop
for (list_of_sheep in list_of_sheep){
  
#sheep_list_df_x <- list_of_comparsions %>%  filter(sheep == list_of_sheep[[1]]) %>% select(comparsions)
sheep_list_df_x <- list_of_comparsions %>%  filter(sheep == list_of_sheep) %>% select(comparsions)
sheep_list_x <- as.list(sheep_list_df_x)
sheep_list_comparisonx <- c(sheep_list_x$comparsions)
  #sheep_list_comparisonx <- "dist10vs7"
names(matrix_66percent) 
sheep_x <-   matrix_66percent %>%  dplyr::select(all_of(sheep_list_comparisonx)) #remove the time step and only select clms that relate to the sheep

sheep_x[sheep_x > Min_dist] <- NA #replace values >1.5 with NA

sheep_x <- sheep_x %>%  
  dplyr::mutate(count = ncol(sheep_x) - rowSums(is.na(sheep_x) | sheep_x == ""))  %>% 
  dplyr::select(count )

sheep_matrix <-   matrix_66percent %>%  dplyr::select(time_step)
sheep_matrix <- cbind(sheep_matrix, sheep_x)


sheep_matrix <- sheep_matrix %>%
  rename(!!paste0(paste0("numb_sheep_close")) := count) %>% 
  mutate(sheep = list_of_sheep)
  #mutate(sheep = list_of_sheep[[1]])

df_temp = left_join(reg_time_step, sheep_matrix)

df <- rbind(df, df_temp) 

}    




