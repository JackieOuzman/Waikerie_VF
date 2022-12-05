### template for creating some variable

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

GPS_Dist %>% distinct(sheep)  ### 26 sheep ID I need regular time interval for each sheep


#regular_time_interval_per_sheep_ID ? this may need to be a loop or function

regular_time_interval_2 <- regular_time_interval %>% 
  dplyr::mutate(Time_sheep = paste0(time_step,"_", 2))
regular_time_interval_3 <- regular_time_interval %>% 
  dplyr::mutate(Time_sheep = paste0(time_step,"_", 3))

################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS_Dist)
# Need to round the local time to the closest 10 min
GPS_Dist <- GPS_Dist %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep) )


################################################################################
#### --------------    Join regular time step to dataset  -------------- ####
################################################################################
#For all joins, rows will be duplicated if one or more rows in x matches multiple rows in y.
# Need to deal with duplicates before joining the data.


##----- sheep 2 -----###
GPS_sheep2 <- GPS_Dist %>%  filter(sheep == 2)
GPS_sheep2 <- GPS_sheep2 %>% 
  dplyr::distinct(Time_sheep, .keep_all = TRUE) # this is where you could do other options group_by average, max ??

GPS_sheep2_reg_time <- left_join(regular_time_interval_2, GPS_sheep2)


##----- sheep 3 -----###
GPS_sheep3 <- GPS_Dist %>%  filter(sheep == 3)
GPS_sheep3 <- GPS_sheep3 %>% 
  dplyr::distinct(Time_sheep, .keep_all = TRUE) # this is where you could do other options group_by average, max ??

GPS_sheep3_reg_time <- left_join(regular_time_interval_3, GPS_sheep3)




#--- comment here if I have multiple rows for the same 10 mins this method only hangs onto the first row that matches (due to removing duplictae step).
#--- This is not ideal but is similar to what the newer collars do.
#--- an option could be to have a smaller time step and then do the calculations that are needed. 
#--- Say distance from the fence or number of steps.
#--- then you can use a groupby function using '10min time step clm' or 'round to 10min clm' and report on the max value for the variable.

#--- other comments is the missing rows of data will need to be filled in but how?


## why do I have so much un - joined data - the sheep 2 did not run for the length of the trial.

## so for this example I just need to trim the df to match the end of sheep 2

### -------------------------------------------------------------####

start_sheep2 <- min(GPS_sheep2$local_time, na.rm = TRUE)  
end_sheep2 <-   max(GPS_sheep2$local_time, na.rm = TRUE) 
start_sheep2 <- round_date(start_sheep2, unit="10 mins") #"2018-03-13 09:50:00 ACDT"
end_sheep2 <- round_date(end_sheep2, unit="10 mins") #"2018-03-14 15:30:00 ACDT"

## trim the joined data to the sheeps ID time in the trial 

GPS_sheep2_reg_time <- GPS_sheep2_reg_time %>% 
  dplyr::filter(between(local_time, ymd_hms(start_sheep2), ymd_hms(end_sheep2)))

### -------------------------------------------------------------####

start_sheep3 <- min(GPS_sheep3$local_time, na.rm = TRUE)  
end_sheep3 <-   max(GPS_sheep3$local_time, na.rm = TRUE) 
start_sheep3 <- round_date(start_sheep3, unit="10 mins") #"2018-03-13 10:20:00 ACDT"
end_sheep3 <- round_date(end_sheep3, unit="10 mins") #"2018-03-14 15:30:00 ACDT"

## trim the joined data to the sheeps ID time in the trial 

GPS_sheep3_reg_time <- GPS_sheep3_reg_time %>% 
  dplyr::filter(between(local_time, ymd_hms(start_sheep3), ymd_hms(end_sheep3)))






################################################################################
#### Do some cals  steps or distance travelled since last logged point ---- ####
################################################################################
GPS_sheep2_reg_time <- GPS_sheep2_reg_time %>% 
  arrange(local_time)

GPS_sheep2_reg_time <- GPS_sheep2_reg_time %>% 
  dplyr::mutate(step = sqrt( ((lead(X) - X)^ 2) + ((lead(Y) - Y)^ 2) ) )

### -------------------------------------------------------------####
GPS_sheep3_reg_time <- GPS_sheep3_reg_time %>% 
  arrange(local_time)

GPS_sheep3_reg_time <- GPS_sheep3_reg_time %>% 
  dplyr::mutate(step = sqrt( ((lead(X) - X)^ 2) + ((lead(Y) - Y)^ 2) ) )


str(GPS_sheep2_reg_time)
str(GPS_sheep3_reg_time)

##### Think about how to deal with yarding times - I think you may need to remove first step cal after yarding time.
## I dont think I need to do anything here - the animals we yarded over night and the data is trimmed to this alreday.
### If you had a yard in and out and the data wasnt trimmed this might be a problem.

# day_13 =  between(local_time, ymd_hms("2018-03-13 09:30:00"), ymd_hms("2018-03-13 15:30:00")) )
# day_14 = between(local_time, ymd_hms("2018-03-14 09:30:00"), ymd_hms("2018-03-14 15:30:00")) )
# day_15 = between(local_time, ymd_hms("2018-03-15 09:30:00"), ymd_hms("2018-03-15 15:30:00")) )
# day_16 = between(local_time, ymd_hms("2018-03-16 09:30:00"), ymd_hms("2018-03-16 15:30:00")) )


rm(GPS_Dist, GPS_sheep2, GPS_sheep3, regular_time_interval, regular_time_interval_2, regular_time_interval_3)

GPS_sheep2_3 <- rbind(GPS_sheep2_reg_time, GPS_sheep3_reg_time)
output_path <- "W:/VF/Optimising_VF/Waikerie/data_prep/"  #animals_GPS_trim_time


write.csv(GPS_sheep2_3, 
          paste0(output_path,"/GPS_sheep2_3_reg_time_step_dist.csv"), 
          row.names=FALSE)
