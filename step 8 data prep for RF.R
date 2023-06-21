
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
####    remove the animals that don't have collars     ###########################
################################################################################

list_of_sheep_VF <- c(2,3,5,13,14, 17,22,30,35)


all_animals_collars <- all_animals %>%  filter( sheep %in% list_of_sheep_VF)




###############################################################################
#### create a ID variable if the animal is in the exclusion zone or not  #####
### perhaps something like time spent in the VF ####



count_VF_occurance_per_animal <- all_animals_collars %>%  group_by( sheep, VF_EX) %>% 
  summarise(count_records = n())
count_VF_occurance_per_animal

#express as a percentage


count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal %>% 
  pivot_wider(names_from = VF_EX, 
              values_from = count_records)

#replace NA values with zero
count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal_wide %>% dplyr::mutate(outside_VF = replace_na(outside_VF, 0))
count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal_wide %>% dplyr::mutate(inside_VF  = replace_na(inside_VF , 0))


#sum of total counts

count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal_wide %>% 
  dplyr::mutate(total_counts = inside_VF + outside_VF,
                prop_exclusion_zone = ((outside_VF /total_counts)*100)) %>% 
  arrange(sheep)


count_VF_occurance_per_animal_wide

### turn the time spent in exclusion zone into categorical data.

count_VF_occurance_per_animal_wide <- count_VF_occurance_per_animal_wide %>% 
  dplyr::mutate(compliance_score =
    case_when(prop_exclusion_zone > 5 ~ "non_compliant",
              prop_exclusion_zone <= 5 ~ "compliant"))

sheep_compliance_score <- count_VF_occurance_per_animal_wide %>% dplyr::select(sheep, compliance_score)

rm(all_animals, count_VF_occurance_per_animal, count_VF_occurance_per_animal_wide, list_of_sheep_VF)


#### join sheep_compliance_score to the original dataset.
str(all_animals_collars)
all_animals_collars <- left_join(all_animals_collars, sheep_compliance_score)


rm(sheep_compliance_score)

###################################################################################
###                 write out df ready for the next step                      ###
###################################################################################

output_path <- "W:/VF/Optimising_VF/Waikerie/data_prep/"  

write.csv(all_animals_collars, 
          paste0(output_path,"/step8_all_animals_collars.csv"), 
          row.names=FALSE)
###################################################################################
### condense the data so it can be simply used in RF model - one row per sheep  ###
###################################################################################


