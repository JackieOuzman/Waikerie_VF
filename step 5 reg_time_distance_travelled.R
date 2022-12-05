### reg time step and distnace travelled

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



################################################################################
#### --------------    Bring in data   -------------- ####
################################################################################

GPS_Dist <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/GPS_all_dist_line_VF_zone_test.csv")
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

################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep) )



rm(end,start, time.duration, time.interval)

sheep_list <- GPS_Dist %>% distinct(sheep) %>%  arrange(sheep)
### 26 sheep ID I need regular time interval for each sheep
### List of sites I want to run analysis for:

sheep_list <- c(1:36)
#sheep_list <- c(1:2)

### as a function
for (sheep_list in sheep_list){
  
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
    dplyr::distinct(Time_sheep, .keep_all = TRUE)              
  
  GPS_sheep_reg_time <- left_join(regular_time_interval_sheep, GPS_sheep)

  #### Trim the regular time step to match the end of sheep time

  start_sheep <- min(GPS_sheep$local_time, na.rm = TRUE)  
  end_sheep <-   max(GPS_sheep$local_time, na.rm = TRUE) 
  start_sheep <- round_date(start_sheep, unit="10 mins")
  end_sheep <- round_date(end_sheep, unit="10 mins") 
  
  ## trim the joined data to the sheeps ID time in the trial 
  
  
  GPS_sheep_reg_time <- GPS_sheep_reg_time %>% 
    dplyr::filter(between(local_time, ymd_hms(start_sheep), ymd_hms(end_sheep)))  
  
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
  name <- paste0("GPS_sheep_reg_time_step", sheep_list)
  assign(name,GPS_sheep_reg_time)
  
  }       

file_list <- data.frame(name_df = paste0("GPS_sheep_reg_time_step",c(1:36)))
                
GPS_sheep_reg_time_step_all <- rbind(
  GPS_sheep_reg_time_step1,
  GPS_sheep_reg_time_step2,
  GPS_sheep_reg_time_step3,
  GPS_sheep_reg_time_step4,
  GPS_sheep_reg_time_step5,
  GPS_sheep_reg_time_step6,
  GPS_sheep_reg_time_step7,
  GPS_sheep_reg_time_step8,
  GPS_sheep_reg_time_step9,
  GPS_sheep_reg_time_step10,
  GPS_sheep_reg_time_step11,
  GPS_sheep_reg_time_step12,
  GPS_sheep_reg_time_step13,
  GPS_sheep_reg_time_step14,
  GPS_sheep_reg_time_step15,
  GPS_sheep_reg_time_step16,
  GPS_sheep_reg_time_step17,
  GPS_sheep_reg_time_step18,
  GPS_sheep_reg_time_step19,
  GPS_sheep_reg_time_step20,
  GPS_sheep_reg_time_step21,
  GPS_sheep_reg_time_step22,
  GPS_sheep_reg_time_step23,
  GPS_sheep_reg_time_step24,
  GPS_sheep_reg_time_step25,
  GPS_sheep_reg_time_step26,
  GPS_sheep_reg_time_step27,
  GPS_sheep_reg_time_step28,
  GPS_sheep_reg_time_step29,
  GPS_sheep_reg_time_step30,
  GPS_sheep_reg_time_step31,
  GPS_sheep_reg_time_step32,
  GPS_sheep_reg_time_step33,
  GPS_sheep_reg_time_step34,
  GPS_sheep_reg_time_step35,
  GPS_sheep_reg_time_step36)




rm(GPS_sheep_reg_time_step1,
   GPS_sheep_reg_time_step2,
   GPS_sheep_reg_time_step3,
   GPS_sheep_reg_time_step4,
   GPS_sheep_reg_time_step5,
   GPS_sheep_reg_time_step6,
   GPS_sheep_reg_time_step7,
   GPS_sheep_reg_time_step8,
   GPS_sheep_reg_time_step9,
   GPS_sheep_reg_time_step10,
   GPS_sheep_reg_time_step11,
   GPS_sheep_reg_time_step12,
   GPS_sheep_reg_time_step13,
   GPS_sheep_reg_time_step14,
   GPS_sheep_reg_time_step15,
   GPS_sheep_reg_time_step16,
   GPS_sheep_reg_time_step17,
   GPS_sheep_reg_time_step18,
   GPS_sheep_reg_time_step19,
   GPS_sheep_reg_time_step20,
   GPS_sheep_reg_time_step21,
   GPS_sheep_reg_time_step22,
   GPS_sheep_reg_time_step23,
   GPS_sheep_reg_time_step24,
   GPS_sheep_reg_time_step25,
   GPS_sheep_reg_time_step26,
   GPS_sheep_reg_time_step27,
   GPS_sheep_reg_time_step28,
   GPS_sheep_reg_time_step29,
   GPS_sheep_reg_time_step30,
   GPS_sheep_reg_time_step31,
   GPS_sheep_reg_time_step32,
   GPS_sheep_reg_time_step33,
   GPS_sheep_reg_time_step34,
   GPS_sheep_reg_time_step35,
   GPS_sheep_reg_time_step36)

##### Think about how to deal with yarding times - I think you may need to remove first step cal after yarding time.
## I dont think I need to do anything here - the animals we yarded over night and the data is trimmed to this alreday.
### If you had a yard in and out and the data wasnt trimmed this might be a problem.

# day_13 =  between(local_time, ymd_hms("2018-03-13 09:30:00"), ymd_hms("2018-03-13 15:30:00")) )
# day_14 = between(local_time, ymd_hms("2018-03-14 09:30:00"), ymd_hms("2018-03-14 15:30:00")) )
# day_15 = between(local_time, ymd_hms("2018-03-15 09:30:00"), ymd_hms("2018-03-15 15:30:00")) )
# day_16 = between(local_time, ymd_hms("2018-03-16 09:30:00"), ymd_hms("2018-03-16 15:30:00")) )





output_path <- "W:/VF/Optimising_VF/Waikerie/data_prep/"  #animals_GPS_trim_time


write.csv(GPS_sheep_reg_time_step_all, 
          paste0(output_path,"/GPS_sheep_all_reg_time_step_dist.csv"), 
          row.names=FALSE)
