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
step1_2_3 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/animal_GPS_data_step1_2_3.csv")

names(step1_2_3)

### subset the data to the clms I want.

GPS <- step1_2_3 %>% dplyr::select (ID, sheep, treatment,DOT,herd_postion,local_time, date,DOY, X , Y)

GPS$local_time <- as.POSIXct(GPS$local_time,  tz = "Australia/Adelaide")

str(GPS)
################################################################################
#### --------------    what is the length of the trail?   -------------- ####
################################################################################


start <- min(GPS$local_time, na.rm = TRUE)  # "2018-03-13 09:30:40 ACDT"
end <-   max(GPS$local_time, na.rm = TRUE) # "2018-03-16 15:30:00 ACDT"
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

GPS %>% distinct(sheep)  ### 26 sheep ID I need regular time interval for each sheep

### ---- stuck here but not sure if this is the correct way ----###
#regular_time_interval_per_sheep_ID 

regular_time_interval <- regular_time_interval %>% 
  dplyr::mutate(Time_sheep = paste0(time_step,"_", 2))


################################################################################
#### ----   Need to round the local time to the closest 10 min  -------------- ####
################################################################################
str(GPS)
# Need to round the local time to the closest 10 min
GPS <- GPS %>% 
  dplyr::mutate(round_local_time =round_date(local_time, unit="10 mins"),
                Time_sheep = paste0(round_local_time,"_", sheep) )


################################################################################
#### --------------    Join regular time step to dataset  -------------- ####
################################################################################
#For all joins, rows will be duplicated if one or more rows in x matches multiple rows in y.
# Need to deal with duplicates before joining the data.



GPS_sheep2 <- GPS %>%  filter(sheep == 2)
GPS_sheep2 <- GPS_sheep2 %>% 
  dplyr::distinct(Time_sheep, .keep_all = TRUE) # this is where you could do other options group_by average, max ??
str(GPS_sheep2)

str(regular_time_interval) 


test_join <- left_join(regular_time_interval, GPS_sheep2)

#--- comment here if I have multiple rows for the same 10 mins this method only hangs onto the first row that matches (due to removing duplictae step).
#--- This is not ideal but is similar to what the newer collars do.
#--- an option could be to have a smaller time step and then do the calculations that are needed. 
#--- Say distance from the fence or number of steps.
#--- then you can use a groupby function using '10min time step clm' or 'round to 10min clm' and report on the max value for the variable.

#--- other comments is the missing rows of data will need to be filled in but how?


## why do I have so much un - joined data - the sheep 2 did not run for the length of the trial.

## so for this example I just need to trim the df to match the end of sheep 2



start_sheep2 <- min(GPS_sheep2$local_time, na.rm = TRUE)  
end_sheep2 <-   max(GPS_sheep2$local_time, na.rm = TRUE) 
start_sheep2 <- round_date(start_sheep2, unit="10 mins") #"2018-03-13 09:50:00 ACDT"
end_sheep2 <- round_date(end_sheep2, unit="10 mins") #"2018-03-14 15:30:00 ACDT"

## trim the joined data to the sheeps ID time in the trial 

df <- test_join %>% 
  dplyr::filter(between(local_time, ymd_hms(start_sheep2), ymd_hms(end_sheep2)))


################################################################################
#### Do some cals  steps or distance travelled since last logged point ---- ####
################################################################################
df <- df %>% 
  arrange(local_time)


df <- df %>% 
  dplyr::mutate(step = sqrt( ((lead(X) - X)^ 2) + ((lead(Y) - Y)^ 2) ) )
