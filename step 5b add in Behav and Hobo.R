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

#Bring in the behavioural data and HOBO data and merge to step 5

################################################################################
#### --------------    Bring in data   -------------- ####
################################################################################



all_animals <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5_Greg_time_step_dist_travelled.csv")
names(all_animals)

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


### Add and ID clm
behav <- behav %>% dplyr::mutate( ID = row_number())

# I want to make sure when there are duplicates entries for the same time I get the audio
# keep only one record for A and only record for S

behav_A <- behav %>% filter(Cue == "A")
behav_S <- behav %>% filter(Cue == "S")

behav_A <- behav_A %>% 
  dplyr::distinct(Time_sheep, .keep_all = TRUE) 

behav_S <- behav_S %>% 
  dplyr::distinct(Time_sheep, .keep_all = TRUE)       
  
behav_A_S <- rbind(behav_A, behav_S)

behav_A_S <- behav_A_S %>% arrange(Time_sheep)

duplication_report_behav <- behav_A_S %>% count(Time_sheep)

behav_A_S <- left_join(behav_A_S,duplication_report_behav ) %>% rename(occurance = n) 
str(behav_A_S)

behav_A_S <- behav_A_S %>% mutate(
  what_to_retain = case_when(
    occurance == 2 & Cue == "S" ~ "retain",
    occurance == 1 & Cue == "A" ~ "retain",
    TRUE                      ~ "discard"
  )
) 

behav_A_S <- behav_A_S %>%filter(what_to_retain == "retain")
str(behav_A_S)
behav_A_S <- behav_A_S %>% arrange(sheep, round_local_time)


rm(behav,behav_A, behav_S, duplication_report_behav)
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


duplication_report_hobo <- hobo_summary_wide %>% count(Time_sheep)
duplication_report_hobo #this is all ones so no duplication :)

hobo_summary_wide <- hobo_summary_wide %>% mutate(across(c(resting, standing, walking, running), round, 2))

###############################################################################
#### --------------    Merge Hobo and Beha data   -------------- ####
################################################################################
behav_hobo <- full_join(behav_A_S, hobo_summary_wide)

behav_hobo <- behav_hobo %>%  select(Time_sheep, Cue, resting: running  )


##############################################################################
#### --------------    Merge Hobo and Beha data  to the biggere data set -------------- ####
################################################################################
all_animals_with_beh_hob <- left_join(all_animals,behav_hobo)


duplication_report_all_animals_with_beh_hob <- all_animals_with_beh_hob %>% count(Time_sheep)
duplication_report_all_animals_with_beh_hob #this is all ones so no duplication :)



output_path <- "W:/VF/Optimising_VF/Waikerie/data_prep/"  #animals_GPS_trim_time


write.csv(all_animals_with_beh_hob, 
          paste0(output_path,"/step5b_Greg_time_step_dist_travelled.csv"), 
          row.names=FALSE)



################################################################################

## double check that there are not duplication - looks like its fixed no duplication


duplication_report_all_with_Beha_Hobo <- all_animals_with_beh_hob %>% count(Time_sheep)
duplication_report_all_with_Beha_Hobo #
unique(duplication_report_all_with_Beha_Hobo$n)

##why do I have duplication of my timestep a with sheep?

duplication_report_all_with_all_animals <- all_animals %>% count(Time_sheep)
duplication_report_all_with_all_animals #this is all ones so no duplication :)

unique(duplication_report_all_with_all_animals$n)


##why do I have duplication of my timestep a with sheep?

duplication_report_all_with_behav_hobo <- behav_hobo %>% count(Time_sheep)
duplication_report_all_with_behav_hobo #this is all ones so no duplication :)

unique(duplication_report_all_with_behav_hobo$n)
