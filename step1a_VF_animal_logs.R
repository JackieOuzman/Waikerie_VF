library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
library(readxl)



### bring in animal logs for VF for only 100% trial






### bring in animal logs for VF all
path_100 <- "W:/VF/Optimising_VF/raw_data/Waikerie/GPS Rawdata/100%/"

Waikerie_100_sheep35_1 <- read_csv(paste0(path_100, "Sheep 35 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 35,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_100_sheep35_2 <- read_csv(paste0(path_100, "Sheep 35 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 35,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "herd")
Waikerie_100_sheep2_1 <- read_csv(paste0(path_100, "Sheep 2 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 2,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_100_sheep2_2 <- read_csv(paste0(path_100, "Sheep 2 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 2,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "herd")
Waikerie_100_sheep3_1 <- read_csv(paste0(path_100, "Sheep 3 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 3,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "follower")
Waikerie_100_sheep3_2 <- read_csv(paste0(path_100, "Sheep 3 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 3,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "follower")
Waikerie_100_sheep5_1 <- read_csv(paste0(path_100, "Sheep 5 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 5,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_100_sheep5_2 <- read_csv(paste0(path_100, "Sheep 5 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 5,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "herd")
Waikerie_100_sheep13_1 <- read_csv(paste0(path_100, "Sheep 13 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 13,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "follower")
Waikerie_100_sheep13_2 <- read_csv(paste0(path_100, "Sheep 13 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 13,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "follower")
Waikerie_100_sheep14_1 <- read_csv(paste0(path_100, "Sheep 14 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 14,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "leader")
Waikerie_100_sheep14_2 <- read_csv(paste0(path_100, "Sheep 14 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 14,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "leader")
Waikerie_100_sheep17_1 <- read_csv(paste0(path_100, "Sheep 17 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 17,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_100_sheep17_2 <- read_csv(paste0(path_100, "Sheep 17 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 17,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "herd")
Waikerie_100_sheep22_1 <- read_csv(paste0(path_100, "Sheep 22  day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 22,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "leader")
Waikerie_100_sheep22_2 <- read_csv(paste0(path_100, "Sheep 22 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 22,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "leader")
Waikerie_100_sheep30_1 <- read_csv(paste0(path_100, "Sheep 30 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 30,
                treatment = "100_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_100_sheep30_2 <- read_csv(paste0(path_100, "Sheep 30 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 30,
                treatment = "100_percent",
                DOT = 2,
                herd_postion = "herd")

Waikerie_100_percent_all <- rbind(Waikerie_100_sheep2_1,
                                  Waikerie_100_sheep2_2,
                                  Waikerie_100_sheep3_1,
                                  Waikerie_100_sheep3_2,
                                  Waikerie_100_sheep5_1,
                                  Waikerie_100_sheep5_2,
                                  Waikerie_100_sheep14_1,
                                  Waikerie_100_sheep14_2,
                                  Waikerie_100_sheep13_2,
                                  Waikerie_100_sheep13_2,
                                  Waikerie_100_sheep17_1,
                                  Waikerie_100_sheep17_2,
                                  Waikerie_100_sheep22_1,
                                  Waikerie_100_sheep22_2,
                                  Waikerie_100_sheep30_1,
                                  Waikerie_100_sheep30_2,
                                  Waikerie_100_sheep35_1,
                                  Waikerie_100_sheep35_2,
                                  Waikerie_100_sheep14_1,
                                  Waikerie_100_sheep14_2)
                                  



rm(Waikerie_100_sheep2_1,
   Waikerie_100_sheep2_2,
   Waikerie_100_sheep3_1,
   Waikerie_100_sheep3_2,
   Waikerie_100_sheep5_1,
   Waikerie_100_sheep5_2,
   Waikerie_100_sheep13_1,
   Waikerie_100_sheep13_2,
   Waikerie_100_sheep14_1,
   Waikerie_100_sheep14_2,
   Waikerie_100_sheep17_1,
   Waikerie_100_sheep17_2,
   Waikerie_100_sheep22_1,
   Waikerie_100_sheep22_2,
   Waikerie_100_sheep30_1,
   Waikerie_100_sheep30_2,
   Waikerie_100_sheep35_1,
   Waikerie_100_sheep35_2,
   Waikerie_100_sheep14_1,
   Waikerie_100_sheep14_2)





Waikerie_all <- Waikerie_100_percent_all

str(Waikerie_all)


#format time and date clm from character to time
Waikerie_all <-
  Waikerie_all %>%
  dplyr::select(ID:time, sheep:herd_postion) %>% 
  mutate(timeOfEvent = as.POSIXct(time, tz = "GMT", 
                                  format = "%Y-%m-%d %H:%M:%S"))


  Waikerie_all <- Waikerie_all %>% 
  mutate(GMT = ymd_hms(time, tz = "GMT"))

  Waikerie_all <- Waikerie_all %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Melbourne"))

## Add a clm for ID_jaxs
  Waikerie_all <- Waikerie_all %>% 
  dplyr::mutate( ID_jaxs = row_number())



## ok lets just remove the Nulls
  Waikerie_all <- Waikerie_all %>% 
       filter(lat!= "NULL")
     
str(Waikerie_all)






Waikerie_all <- Waikerie_all %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Melbourne"),
         DOY = yday(date))



############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
#str(animal_GPS_data)



#turn into spatial data
Waikerie_all_sf <-
  st_as_sf(Waikerie_all,
           coords = c("lon", "lat"),
           crs = 4326, 
           agr = "constant")

Waikerie_all_sf_trans <-
  st_transform(Waikerie_all_sf, crs = 28354) 


rm(Waikerie_all_sf )

## write out the file in CSV format
write.csv(Waikerie_all,row.names = FALSE,
          file = "W:/VF/Optimising_VF/Waikerie/data_prep/step1a_GPS_cood.csv")
 


############################################################################################
############                  bring in HOBO data            ##############################
############################################################################################



### bring in animal logs for VF 100% trial data only
path_HOBO_VFtest <- "W:/VF/Optimising_VF/raw_data/Waikerie/Raw_hobo_behav/Hobo_revised/"
treatment <- "100%d1"

sheep35_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep35clip.csv")) %>%   dplyr::mutate(sheep = 35,  day = 1)
sheep2_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep2clip.csv")) %>%   dplyr::mutate(sheep = 2,  day = 1)
sheep3_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep3clip.csv")) %>%   dplyr::mutate(sheep = 3,  day = 1)
sheep5_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep5clip.csv")) %>%   dplyr::mutate(sheep = 5,  day = 1)
sheep13_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep13clip.csv")) %>%   dplyr::mutate(sheep = 13,  day = 1)
sheep14_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep14clip.csv")) %>%   dplyr::mutate(sheep = 14,  day = 1)
sheep17_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep17clip.csv")) %>%   dplyr::mutate(sheep = 17,  day = 1)
sheep22_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep22clip.csv")) %>%   dplyr::mutate(sheep = 22,  day = 1)
sheep30_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep30clip.csv")) %>%   dplyr::mutate(sheep = 30,  day = 1)

sheep_day1_100_percent <- rbind(sheep35_1, sheep2_1, sheep3_1, sheep5_1, sheep13_1, sheep14_1, sheep17_1, sheep22_1, sheep30_1)
rm(sheep35_1, sheep2_1, sheep3_1, sheep5_1, sheep13_1, sheep14_1, sheep17_1, sheep22_1, sheep30_1)

treatment <- "100%d2"


sheep35_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep35clip.csv")) %>%   dplyr::mutate(sheep = 35,  day = 2)
sheep2_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep20clip.csv")) %>%   dplyr::mutate(sheep = 2,  day = 2)
sheep3_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep3clip.csv")) %>%   dplyr::mutate(sheep = 3,  day = 2)
sheep5_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep5clip.csv")) %>%   dplyr::mutate(sheep = 5,  day = 2)
#sheep13_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep13clip.csv")) %>%   dplyr::mutate(sheep = 13,  day = 2) #missing
sheep14_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep14clip.csv")) %>%   dplyr::mutate(sheep = 14,  day = 2)
sheep17_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep17clip.csv")) %>%   dplyr::mutate(sheep = 17,  day = 2)
sheep22_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep22clip.csv")) %>%   dplyr::mutate(sheep = 22,  day = 2)
sheep30_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep30clip.csv")) %>%   dplyr::mutate(sheep = 30,  day = 2)

sheep_day2_100_percent <- rbind(sheep35_2, sheep2_2, sheep3_2, sheep5_2, sheep14_2, sheep17_2, sheep22_2, sheep30_2) # missing this file sheep13_2,
rm(sheep35_2, sheep2_2, sheep3_2, sheep5_2,  sheep14_2, sheep17_2, sheep22_2, sheep30_2)

Sheep100percent <-rbind(sheep_day1_100_percent, sheep_day2_100_percent)
Sheep100percent <- Sheep100percent %>%  mutate(treatment = "100_percent")

rm(sheep_day1_100_percent, sheep_day2_100_percent)



Hobo_trial_data <- Sheep100percent
str(Hobo_trial_data)
################################################################################

str(Hobo_trial_data)

HOBO_VFtest_1 <- Hobo_trial_data

HOBO_VFtest_1$`Date Time` <- as.POSIXct(HOBO_VFtest_1$`Date Time`, format="%d/%m/%Y %H:%M:%S")
str(HOBO_VFtest_1)
## remove dummy clm and rows with no data



## write out the file in CSV format
# write.csv(HOBO_PreVF,row.names = FALSE,
#           file = "W:/VF/Optimising_VF/Waikerie/data_prep/HOBO_PreVF.csv")
# write.csv(HOBO_PostVF,row.names = FALSE,
#           file = "W:/VF/Optimising_VF/Waikerie/data_prep/HOBO_PostVF.csv")
write.csv(HOBO_VFtest_1,row.names = FALSE,
          file = "W:/VF/Optimising_VF/Waikerie/data_prep/HOBO_VFtest.csv")

################################################################################
#####                      Behavioural data              #######################
###############################################################################

### bring in animal logs for VF all
path_behavioural <- "W:/VF/Optimising_VF/raw_data/Waikerie/Raw_hobo_behav/"


Behav_100 <- read_excel(paste0(path_behavioural, "VF_Behaviour.xlsx"), sheet = "100%") %>% 
  mutate(treatment = "100_percent")

Behav <-Behav_100
rm(Behav_100)


### clean up the time clm the time was manually entered and is not GMT so convert to string

Behav$Time <- as.character(Behav$Time)
str(Behav)
Behav <-  Behav %>% separate(Time, into = c("dummy_date", "local_time"), sep=" ")
## add a real date

Behav <-  Behav %>%
  mutate(
    date = case_when(
      treatment == "100_percent" & Day == 1 ~ "13/03/2018",
      treatment == "100_percent" & Day == 2 ~ "14/03/2018"
      ))

Behav <-  Behav %>%
  mutate(date_time = paste0(date," ", local_time))

str(Behav)


Behav$date_time <- as.POSIXct(Behav$date_time, format="%d/%m/%Y %H:%M:%S")

## remove dummy clm and rows with no data

str(Behav)
Behav <- Behav %>%  dplyr::select(-dummy_date)
Behav <- Behav %>% filter(!is.na(date_time))


write.csv(Behav,row.names = FALSE,
          file = "W:/VF/Optimising_VF/Waikerie/data_prep/Behavioural.csv")
