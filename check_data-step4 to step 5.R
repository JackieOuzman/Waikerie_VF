

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
#### --------------    Bring in data  step 4 -------------- ####
################################################################################

step4 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step4_dist_line_VF_zone.csv")
names(GPS_Dist)

### subset the data to the clms I want.

step4 <- step4 %>% dplyr::select (ID, sheep, treatment,DOT,herd_postion,local_time, date,DOY, X , Y, dist_to_VF, VF_EX)

step4$local_time <- as.POSIXct(step4$local_time,  tz = "Australia/Adelaide")


##############################################

#################################################################################
#### check step 4 #####################################################################
#################################################################################
str(step4)
Step4_GPS_all_df_Exc_only <- step4 %>% filter(VF_EX == "outside_VF")


date_13_14_keep_these <- c(2,3,5,13,14,17,22,30,35) #100% trial was run on the 13th and 14th
date_15_16_keep_these <- c(12,23,25, 10,15,21,27,33,36)  # 33% and 66% trial was run 15th and 16th


Check_1_step4 <- Step4_GPS_all_df_Exc_only %>%
  filter(date == "2018-03-13" | date == "2018-03-13")  %>%
  filter(sheep %in% date_13_14_keep_these)


Check_2_step4 <-  Step4_GPS_all_df_Exc_only %>%  
  filter(date == "2018-03-15" |date == "2018-03-16")  %>%
  filter(sheep %in% date_15_16_keep_these)


step4_count_exclusion_zone_occurance_per_animal_1 <- Check_1_step4 %>%  group_by( sheep) %>% 
  summarise(count_records = n())
step4_count_exclusion_zone_occurance_per_animal_1

step4_count_exclusion_zone_occurance_per_animal_2 <- Check_2_step4 %>%  group_by( sheep) %>% 
  summarise(count_records = n())
step4_count_exclusion_zone_occurance_per_animal_2



###############################################################################
#### --------------    Bring in data  step 5 -------------- ####
################################################################################

step5 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5_Greg_time_step_dist_travelled.csv")

### subset the data to the clms I want.
str(step5)
step5 <- step5 %>% dplyr::select (ID, sheep, time_step, Time_sheep, round_local_time, treatment,DOT,herd_postion,local_time, date,DOY, X , Y, dist_to_VF, VF_EX)
str(step5)
step5$local_time <- as.POSIXct(step5$local_time,  tz = "Australia/Adelaide")


##############################################

#################################################################################
#### check step 5 #####################################################################
#################################################################################
str(step5)
Step5_GPS_all_df_Exc_only <- step5 %>% filter(VF_EX == "outside_VF")


date_13_14_keep_these <- c(2,3,5,13,14,17,22,30,35) #100% trial was run on the 13th and 14th
date_15_16_keep_these <- c(12,23,25, 10,15,21,27,33,36)  # 33% and 66% trial was run 15th and 16th


Check_1_step5 <- Step5_GPS_all_df_Exc_only %>%
  filter(date == "2018-03-13" | date == "2018-03-13")  %>%
  filter(sheep %in% date_13_14_keep_these)


Check_2_step5 <-  Step5_GPS_all_df_Exc_only %>%  
  filter(date == "2018-03-15" |date == "2018-03-16")  %>%
  filter(sheep %in% date_15_16_keep_these)


step5_count_exclusion_zone_occurance_per_animal_1 <- Check_1_step5 %>%  group_by( sheep) %>% 
  summarise(count_records = n())
step5_count_exclusion_zone_occurance_per_animal_1

step5_count_exclusion_zone_occurance_per_animal_2 <- Check_2_step5 %>%  group_by( sheep) %>% 
  summarise(count_records = n())
step5_count_exclusion_zone_occurance_per_animal_2


#################################################################################
#### the challenge here is that I have a group of records that all occur in a narrow window of time
#### for example 100% 13/3/2018 sheep 2 at 10:47 has records at 10:47:13, 32,38,40,42 etc
### which will all be rounded to the closest 10min window so 10:50.
### what currently happens is that the data is grouped based on timestep with animal and it uses the first record.
### but I would like to make sure that I am not missing an time when the animal went into the exclusion zone
### so how would I do this?




################################################################################
#### --------------    Bring in data   -------------- ####
################################################################################

GPS_Dist <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step4_dist_line_VF_zone.csv")
names(GPS_Dist)

### subset the data to the clms I want.

GPS_Dist <- GPS_Dist %>% dplyr::select (ID, sheep, treatment,DOT,herd_postion,local_time, date,DOY, X , Y, dist_to_VF, VF_EX)

GPS_Dist$local_time <- as.POSIXct(GPS_Dist$local_time,  tz = "Australia/Adelaide")

str(GPS_Dist)
################################################################################
#### --------------    what is the length of the trail?   -------------- ####
################################################################################


start <- min(GPS_Dist$local_time, na.rm = TRUE)  # "2018-03-13 09:30:40 ACDT"
end <-   max(GPS_Dist$local_time, na.rm = TRUE) # "2018-03-16 15:30:00 ACDT"
#Since we’re dealing with elapsed time between two dates, let’s start with Intervals. We can define an Interval using the %--% operator.
start <- round_date(start, unit="10 mins") #"2018-03-13 09:30:00 ACDT"
end <- round_date(end, unit="10 mins") #"2018-03-16 15:30:00 ACDT"

time.interval <- start %--% end
time.interval
#To create a Duration between these two dates, we can use the as.duration function.

time.duration <- as.duration(time.interval)
time.duration # "280760s (~3.25 days)"

################################################################################
#### --------------    make a regular time step   -------------- ####
################################################################################


regular_time_interval <-data.frame(time_step = seq(from = ymd_hms(start),
                                                   to = ymd_hms(end), 
                                                   by = '10 mins'))

#####################################################################
#################################################################################

################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep),
                Time_sheep_zone = paste0(round_local_time,"_", sheep, "_", VF_EX)) # add this line here to make sure I capture all the breaches of VF



rm(end,start, time.duration, time.interval)


sheep_list <- 2


  ################################################################################  
  #regular_time_interval_per_sheep_ID
  ################################################################################
  regular_time_interval_sheep <- regular_time_interval %>% 
    dplyr::mutate(Time_sheep = paste0(time_step,"_", sheep_list))
  
  ################################################################################
  #### --------------    Join regular time step to dataset  -------------- ####
  ################################################################################  
  
  GPS_sheep <- GPS_Dist %>%  filter(sheep == sheep_list)
  
 GPS_sheep <- GPS_sheep %>% 
    #dplyr::distinct(Time_sheep, .keep_all = TRUE)   #remove this line           
    dplyr::distinct(Time_sheep_zone, .keep_all = TRUE)
  
 
 
 ## the occurrence of a duplicated time_sheep
 
 duplication_report <- GPS_sheep %>% count(Time_sheep)
 
 GPS_sheep <- left_join(GPS_sheep,duplication_report ) %>% rename(occurance = n )
 str(GPS_sheep)
 
 GPS_sheep <- GPS_sheep %>% mutate(
   what_to_retain = case_when(
     occurance == 2 & VF_EX == "outside_VF" ~ "retain",
     occurance == 1 & VF_EX == "inside_VF" ~ "retain",
     TRUE                      ~ "discard"
   )
 ) 
            
# remove the rows tp discard
 GPS_sheep <- GPS_sheep %>% filter(what_to_retain == "retain")
 
 
 
 GPS_sheep_reg_time <- left_join(regular_time_interval_sheep, GPS_sheep)
  
  #### Trim the regular time step to match the end of sheep time
  
  start_sheep <- min(GPS_sheep$local_time, na.rm = TRUE)  
  end_sheep <-   max(GPS_sheep$local_time, na.rm = TRUE) 
  start_sheep <- round_date(start_sheep, unit="10 mins")
  end_sheep <- round_date(end_sheep, unit="10 mins") 
  
  ## trim the joined data to the sheeps ID time in the trial 
  
  #names(GPS_sheep_reg_time)
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    dplyr::filter(between(time_step, ymd_hms(start_sheep), ymd_hms(end_sheep))) 
  
  
  ################################################################################
  #### Do some cals  steps or distance travelled since last logged point ---- ####
  ################################################################################ 
  
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    arrange(local_time)
  
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    dplyr::mutate(step = sqrt( ((lead(X) - X)^ 2) + ((lead(Y) - Y)^ 2) ) )
  
  rm(
    GPS_sheep,
    regular_time_interval_sheep
  )
 
   