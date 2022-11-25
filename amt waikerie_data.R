# amt Waikarie dataset

#https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html

# amt 
#install.packages("amt")

library(dplyr)
library(ggplot2)
library(amt)

library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)

## get data
df<- read.csv("W:/VF/Optimising_VF/Waikerie/data_prep/animal_GPS_data_step1_2_3.csv")
df <- df %>%  dplyr::select(ID , time:DOY, X, Y )
df$sheep <- as.character(df$sheep)
df$local_time <- as.POSIXct(df$local_time,  tz = "Australia/Melbourne")
unique(df$sheep)
df_sheep1 <- df %>% filter(sheep == "1")
str(df_sheep1)


## arrange the data by time
df_sheep1 <- df_sheep1 %>% 
  arrange(local_time)



# Before creating a track, we have to do some data cleaning:
#   
#   check if any coordinates are missing (and if so, remove the relocation),
# parse the date and time,
# create a time stamp,
# check for duplicated time stamps, and
# create two new columns for the id and month of the year.

# check if all observations are complete
all(complete.cases(df_sheep1)) # TRUE = no action required

# parse date and time and create time stamps
df_sheep1 <- df_sheep1 %>% 
  mutate(ts_1 =local_time )
str(df_sheep1)
df_sheep1 <-  df_sheep1 %>% separate(local_time , into = c("dummy_date", "local_time_hms"), sep=" ")

df_sheep1 <- df_sheep1 %>% 
  mutate(ts = as.POSIXct(lubridate::ymd(dummy_date) + lubridate::hms(local_time_hms))) 

         
         
         
         
         
         
         
         
# check for duplicated time stamps
any(duplicated(df_sheep1$ts))

# We have some duplicated time stamps, these need to be removed prior to
# creating a track.
df <- df[!duplicated(df$ts), ]

#Now we can create a track.
str(df_sheep1)


sheep1_tr1_a <- make_track(df_sheep1, X, Y, ts, id = sheep)

sheep1_tr1 <- make_track(df_sheep1, X, Y, ts, id = sheep,
                  crs = sp::CRS("+init=epsg:28354")) ## I get a warning message but the tut says its ok??
sheep1_tr1


summarize_sampling_rate(sheep1_tr1)


# Notes
#This suggests that a sampling rate for 6 hours might be adequate. 
# We can then use the function track_resample to resample the track and only keep relocations that are approximately 6 hours apart (within some tolerance, that can be specified). 
# We will use the function lubridate::hours to specify the sampling rate and lubridate::minutes to specify the tolerance. 
# Both arguments rate and tolerance are expected to be a Period.


#Maybe 8 seconds for my dataset or round to 10 seconds # I am getting an error message here?? but if I up it to mins it is happy.

sheep1_tr1_resample <- sheep1_tr1 %>% track_resample(rate = minutes(5), tolerance = minutes(1)) 
sheep1_tr1_resample

# sheep1_tr1_resample_step <- sheep1_tr1_resample %>% 
#   mutate(sl_ = step_lengths(.),
#          driection = direction_rel(.),
#          direction_abs = direction_abs(.), full_circle = TRUE, zero_dir = "N")
# summary(sheep1_tr1_resample_step$sl_)

test <- sheep1_tr1_resample %>%
  mutate(
    dirction_abs = direction_abs(sheep1_tr1_resample, full_circle = TRUE) %>% as_degree(),
    steps = step_lengths(sheep1_tr1_resample),
    nsd = nsd(sheep1_tr1_resample)
  )
