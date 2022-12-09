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



hobo_summary <-hobo %>% group_by(Time_sheep, Est.State, sheep) %>% summarise(count_Est.State_count = n())


#test_2 <- hobo_summary %>%  filter(sheep == 2)
hobo_summary <- hobo_summary %>% rename(state = Est.State,
                 count = count_Est.State_count)


#make it wide
hobo_summary_wide <- hobo_summary %>% pivot_wider(
  names_from = state ,
  values_from = count
)

hobo_summary_wide <- hobo_summary_wide %>%  arrange(sheep, Time_sheep)

hobo_summary_wide <- hobo_summary_wide %>% mutate(rsd = ifelse(is.na(rsd), 0, rsd ),
                                      lsd = ifelse(is.na(lsd), 0, lsd ),
                                      s =   ifelse(is.na(s), 0, s ),
                                      w =   ifelse(is.na(w), 0, w ),
                                      r =   ifelse(is.na(r), 0, r ),
                                      )


hobo_summary_wide <- hobo_summary_wide %>% 
  mutate(total = sum(rsd, lsd, s,w,r, na.rm = TRUE),
         resting = (rsd+lsd)/total ,
         standing = s/total,
         walking = w/total,
         running = r/total
         
         )

hobo_summary_wide <- ungroup(hobo_summary_wide)

hobo_summary_wide <- hobo_summary_wide %>% select(
  Time_sheep,
  sheep,
  resting, 
  standing, 
  walking, 
  running)


hobo_summary_wide <- hobo_summary_wide %>% mutate(across(c(resting, standing, walking, running), round, 2))

###############################################################################
#### --------------    Merge Hobo and Beha data   -------------- ####
################################################################################
behav_hobo <- full_join(behav, hobo_summary_wide)

behav_hobo <- behav_hobo %>%  select(Time_sheep, Cue, resting: running  )


##############################################################################
#### --------------    Merge Hobo and Beha data  to the biggere data set -------------- ####
################################################################################
all_animals_with_beh_hob <- left_join(all_animals,behav_hobo)



output_path <- "W:/VF/Optimising_VF/Waikerie/data_prep/"  #animals_GPS_trim_time


write.csv(all_animals_with_beh_hob, 
          paste0(output_path,"/step5b_Greg_time_step_dist_travelled.csv"), 
          row.names=FALSE)
