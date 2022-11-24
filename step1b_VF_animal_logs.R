
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)







############################################################################################
############       bring in data created in step1a           ##############################
############################################################################################

path_step1 <- "W:/VF/Optimising_VF/Waikerie/data_prep/"

GPS <- read_csv(paste0(path_step1, "GPS_cood_step_1a.csv")) 
HOBO <- read_csv(paste0(path_step1, "HOBO_VFtest.csv")) 
Behavioural <- read_csv(paste0(path_step1, "Behavioural.csv")) 

## enure the date clms are set to local time

GPS$local_time <- as.POSIXct(GPS$local_time,  tz = "Australia/Melbourne")
str(Behavioural)
Behavioural$date_time <- as.POSIXct(Behavioural$date_time,  tz = "Australia/Melbourne")
str(HOBO) ## I have no idea what if we can use the date / time clm
HOBO$date_time <- as.POSIXct(HOBO$date_time,  tz = "Australia/Melbourne")

############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
str(GPS)

#turn into spatial data
GPS_sf <-
  st_as_sf(GPS,
           coords = c("lon", "lat"),
           crs = 4326, 
           agr = "constant")

GPS_sf_trans <-
  st_transform(GPS_sf, crs = 28354) 



############################################################################################
############                  bring in boundaries             ##############################
############################################################################################




hard_fence_bound <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Boundary_25m.shp")  # this is the hard fences

VF <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Boundary_VF_paddock.shp")




str(GPS)

ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF, color = "black", fill = NA) +
  geom_sf(data = GPS_sf_trans ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "")


################################################################################
#### filtering out GPS data based on times start and end of the trial

GPS_sf_trans <- GPS_sf_trans %>% 
  filter(
    local_time >=  ymd_hms("2018-03-13 09:30:00", tz= "Australia/Melbourne"))

GPS_sf_trans <- GPS_sf_trans %>% 
  filter(
    local_time <=  ymd_hms("2018-03-16 15:30:00", tz= "Australia/Melbourne"))


# Times sheep were brought in each day from what I can understand from the write up HOBO data;

#### each day the animals were yarded so i need to remove this data

# let divide the data per day
day_13 <- GPS_sf_trans %>%  filter(date == "2018-03-13") 
day_14 <- GPS_sf_trans %>%  filter(date == "2018-03-14")
day_15 <- GPS_sf_trans %>%  filter(date == "2018-03-15")
day_16 <- GPS_sf_trans %>%  filter(date == "2018-03-16")

# let divide the data per day and again by treatment
day_13 <-day_13 %>%  filter(treatment == "100_percent"| treatment == "control")
day_14 <-day_14 %>%  filter(treatment == "100_percent"| treatment == "control")

day_15 <-day_15 %>%  filter(treatment == "33_percent"| treatment == "66_percent")
day_16 <-day_16 %>%  filter(treatment == "33_percent"| treatment == "66_percent")


str(day_13)
#-----Up to here----- The below code is not working#

#Next steps boundaries trim location and time
## merge in other animal data
## look for weather data ? anything else??

# keep everything after before yarding and after yarding

min(day_13$local_time)
max(day_13$local_time)


day_13_clean <- day_13%>% filter( between(local_time, ymd_hms("2018-03-13 09:30:00"), ymd_hms("2022-06-29 15:30:00")) )
day_14_clean <- day_14%>% filter( between(local_time, ymd_hms("2018-03-13 09:30:00"), ymd_hms("2022-06-29 15:30:00")) )
day_15_clean <- day_15%>% filter( between(local_time, ymd_hms("2018-03-13 09:30:00"), ymd_hms("2022-06-29 15:30:00")) )
day_16_clean <- day_16%>% filter( between(local_time, ymd_hms("2018-03-13 09:30:00"), ymd_hms("2022-06-29 15:30:00")) )





### put it back togther 

GPS_trim_time <- rbind(day_13_clean, day_14_clean, day_15_clean, day_16_clean)

rm(day_28, day_29_clean, day_30_clean, day_1_clean, day_2)

########################################################################################





### remove the water and other animals logs


animals_GPS_trim_time <- animals_GPS_trim_time %>% 
  filter(Sheep_ID !=  "other") %>% 
  filter(Sheep_ID !=  "water_pt")


ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.05) +
  facet_wrap(.~ date)+
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "All animal logs between 28/06 at 09:50 and 02/07 at 10:10",
       subtitle = "log when animals were yarded removed")



# -------------------------------------------------------------------------------------------------- ###


#I think this should be the end of step 1.







########################################################################################################



output_path <- "W:/VF/Sheep_Chiswick_2022/animal_logs/jax_working"  #animals_GPS_trim_time


############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(animals_GPS_trim_time))
animals_GPS_trim_time_df <- as.data.frame(animals_GPS_trim_time)

animals_GPS_trim_time_df <- animals_GPS_trim_time_df %>% 
  dplyr::select(-"geometry")


animals_GPS_trim_time <-   cbind(animals_GPS_trim_time_df,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


animals_GPS_trim_time$local_time <-   format(animals_GPS_trim_time$local_time, usetz=TRUE)
animals_GPS_trim_time$GMT        <-   format(animals_GPS_trim_time$GMT, usetz=TRUE)
animals_GPS_trim_time$start_fence <-  format(animals_GPS_trim_time$start_fence, usetz=TRUE)
animals_GPS_trim_time$end_fence    <- format(animals_GPS_trim_time$end_fence, usetz=TRUE)
animals_GPS_trim_time$start_trial    <- format(animals_GPS_trim_time$start_trial, usetz=TRUE)

write.csv(animals_GPS_trim_time, 
          paste0(output_path,"/animals_GPS_trim_time_step1.csv"), 
          row.names=FALSE)
#############################################################



