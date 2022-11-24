library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
library(readxl)



### bring in animal logs for VF all
path_33 <- "W:/VF/Optimising_VF/raw_data/Waikerie/GPS Rawdata/33%/"


Waikerie_33_sheep34_1 <- read_csv(paste0(path_33, "Sheep 34 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 34,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_33_sheep34_2 <- read_csv(paste0(path_33, "Sheep 34 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 34,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "herd")




Waikerie_33_sheep32_1 <- read_csv(paste0(path_33, "Sheep 32 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 32,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "herd") 
Waikerie_33_sheep32_2 <- read_csv(paste0(path_33, "Sheep 32 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 32,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "herd")



Waikerie_33_sheep31_1 <- read_csv(paste0(path_33, "Sheep 31 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 31,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "herd") 
Waikerie_33_sheep31_2 <- read_csv(paste0(path_33, "Sheep 31 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 31,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "herd")


Waikerie_33_sheep25_1 <- read_csv(paste0(path_33, "Sheep 25 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 25,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "leader") 
Waikerie_33_sheep25_2 <- read_csv(paste0(path_33, "Sheep 25 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 25,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "leader")



Waikerie_33_sheep23_1 <- read_csv(paste0(path_33, "Sheep 23 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 23,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "follower") 
Waikerie_33_sheep23_2 <- read_csv(paste0(path_33, "Sheep 23 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 23,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "follower")


Waikerie_33_sheep12_1 <- read_csv(paste0(path_33, "Sheep 12 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 12,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "herd") 
Waikerie_33_sheep12_2 <- read_csv(paste0(path_33, "Sheep 12 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 12,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "herd")


Waikerie_33_sheep11_1 <- read_csv(paste0(path_33, "Sheep 11 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 11,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "herd") 
Waikerie_33_sheep11_2 <- read_csv(paste0(path_33, "Sheep 11 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 11,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "herd")


Waikerie_33_sheep9_1 <- read_csv(paste0(path_33, "Sheep 9 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 9,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "follower") 
Waikerie_33_sheep9_2 <- read_csv(paste0(path_33, "Sheep 9 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 9,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "follower")
                                  

Waikerie_33_sheep1_1 <- read_csv(paste0(path_33, "Sheep 1 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 1,
                treatment = "33_percent",
                DOT = 1,
                herd_postion = "leader") 
Waikerie_33_sheep1_2 <- read_csv(paste0(path_33, "Sheep 1 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 1,
                treatment = "33_percent",
                DOT = 2,
                herd_postion = "leader")
                                  
                                  
                                  
                                  
                                  
                                 
Waikerie_33_percent_all <- rbind(Waikerie_33_sheep1_1,
                                 Waikerie_33_sheep1_2,
                                 Waikerie_33_sheep9_1,
                                 Waikerie_33_sheep9_2,
                                 Waikerie_33_sheep11_1,
                                 Waikerie_33_sheep11_2,
                                 Waikerie_33_sheep12_1,
                                 Waikerie_33_sheep12_2,
                                 Waikerie_33_sheep23_1,
                                 Waikerie_33_sheep23_2,
                                 Waikerie_33_sheep25_1,
                                 Waikerie_33_sheep25_2,
                                 Waikerie_33_sheep31_1,
                                 Waikerie_33_sheep31_2,
                                 Waikerie_33_sheep32_1,
                                 Waikerie_33_sheep32_2,
                                 Waikerie_33_sheep34_1,
                                 Waikerie_33_sheep34_2
                                 )                               
                                  
                                  

rm(Waikerie_33_sheep1_1,
   Waikerie_33_sheep1_2,
   Waikerie_33_sheep9_1,
   Waikerie_33_sheep9_2,
   Waikerie_33_sheep11_1,
   Waikerie_33_sheep11_2,
   Waikerie_33_sheep12_1,
   Waikerie_33_sheep12_2,
   Waikerie_33_sheep23_1,
   Waikerie_33_sheep23_2,
   Waikerie_33_sheep25_1,
   Waikerie_33_sheep25_2,
   Waikerie_33_sheep31_1,
   Waikerie_33_sheep31_2,
   Waikerie_33_sheep32_1,
   Waikerie_33_sheep32_2,
   Waikerie_33_sheep34_1,
   Waikerie_33_sheep34_2)




### bring in animal logs for VF all
path_66 <- "W:/VF/Optimising_VF/raw_data/Waikerie/GPS Rawdata/66%/"


Waikerie_66_sheep36_1 <- read_csv(paste0(path_66, "Sheep 36 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 36,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_66_sheep36_2 <- read_csv(paste0(path_66, "Sheep 36 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 36,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "herd")

Waikerie_66_sheep7_1 <- read_csv(paste0(path_66, "Sheep 7 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 7,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "follower")
Waikerie_66_sheep7_2 <- read_csv(paste0(path_66, "Sheep 7 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 7,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "follower")

Waikerie_66_sheep10_1 <- read_csv(paste0(path_66, "Sheep 10 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 10,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_66_sheep10_2 <- read_csv(paste0(path_66, "Sheep 10 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 10,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "herd")

Waikerie_66_sheep15_1 <- read_csv(paste0(path_66, "Sheep 15 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 15,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_66_sheep15_2 <- read_csv(paste0(path_66, "Sheep 15 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 15,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "herd")
Waikerie_66_sheep19_1 <- read_csv(paste0(path_66, "Sheep 19 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 19,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "leader")
Waikerie_66_sheep19_2 <- read_csv(paste0(path_66, "Sheep 19 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 19,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "leader")
Waikerie_66_sheep21_1 <- read_csv(paste0(path_66, "Sheep 21 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 21,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "follower")
Waikerie_66_sheep21_2 <- read_csv(paste0(path_66, "Sheep 21 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 21,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "follower")
Waikerie_66_sheep27_1 <- read_csv(paste0(path_66, "Sheep 27 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 27,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "leader")
Waikerie_66_sheep27_2 <- read_csv(paste0(path_66, "Sheep 27 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 27,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "leader")
Waikerie_66_sheep28_1 <- read_csv(paste0(path_66, "Sheep 28 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 28,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_66_sheep28_2 <- read_csv(paste0(path_66, "Sheep 28 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 28,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "herd")
Waikerie_66_sheep33_1 <- read_csv(paste0(path_66, "Sheep 33 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 33,
                treatment = "66_percent",
                DOT = 1,
                herd_postion = "herd")
Waikerie_66_sheep33_2 <- read_csv(paste0(path_66, "Sheep 33 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 33,
                treatment = "66_percent",
                DOT = 2,
                herd_postion = "herd")


Waikerie_66_percent_all <- rbind(Waikerie_66_sheep36_1,
                                 Waikerie_66_sheep36_2,
                                 Waikerie_66_sheep7_1,
                                 Waikerie_66_sheep7_2,
                                 Waikerie_66_sheep10_1,
                                 Waikerie_66_sheep10_2,
                                 Waikerie_66_sheep15_1,
                                 Waikerie_66_sheep15_2,
                                 Waikerie_66_sheep19_1,
                                 Waikerie_66_sheep19_2,
                                 Waikerie_66_sheep21_1,
                                 Waikerie_66_sheep21_2,
                                 Waikerie_66_sheep27_1,
                                 Waikerie_66_sheep27_2,
                                 Waikerie_66_sheep28_1,
                                 Waikerie_66_sheep28_2,
                                 Waikerie_66_sheep33_1,
                                 Waikerie_66_sheep33_2)

rm(Waikerie_66_sheep36_1,
   Waikerie_66_sheep36_2,
   Waikerie_66_sheep7_1,
   Waikerie_66_sheep7_2,
   Waikerie_66_sheep10_1,
   Waikerie_66_sheep10_2,
   Waikerie_66_sheep15_1,
   Waikerie_66_sheep15_2,
   Waikerie_66_sheep19_1,
   Waikerie_66_sheep19_2,
   Waikerie_66_sheep21_1,
   Waikerie_66_sheep21_2,
   Waikerie_66_sheep27_1,
   Waikerie_66_sheep27_2,
   Waikerie_66_sheep28_1,
   Waikerie_66_sheep28_2,
   Waikerie_66_sheep33_1,
   Waikerie_66_sheep33_2)



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
                                  Waikerie_100_sheep13_1,
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


### bring in animal logs for VF all
path_control <- "W:/VF/Optimising_VF/raw_data/Waikerie/GPS Rawdata/control/"


Waikerie_control_sheep4_1 <- read_csv(paste0(path_control, "Sheep 4 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 4,
                treatment = "control",
                DOT = 1,
                herd_postion = "herd")
Waikerie_control_sheep4_2 <- read_csv(paste0(path_control, "Sheep 4 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 4,
                treatment = "control",
                DOT = 2,
                herd_postion = "herd")
Waikerie_control_sheep6_1 <- read_csv(paste0(path_control, "Sheep 6 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 6,
                treatment = "control",
                DOT = 1,
                herd_postion = "herd")
Waikerie_control_sheep6_2 <- read_csv(paste0(path_control, "Sheep 6 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 6,
                treatment = "control",
                DOT = 2,
                herd_postion = "herd")
Waikerie_control_sheep8_1 <- read_csv(paste0(path_control, "Sheep 8 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 8,
                treatment = "control",
                DOT = 1,
                herd_postion = "follower")
Waikerie_control_sheep8_2 <- read_csv(paste0(path_control, "Sheep 8 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 8,
                treatment = "control",
                DOT = 2,
                herd_postion = "follower")
Waikerie_control_sheep16_1 <- read_csv(paste0(path_control, "Sheep 16 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 16,
                treatment = "control",
                DOT = 1,
                herd_postion = "leader")
Waikerie_control_sheep16_2 <- read_csv(paste0(path_control, "Sheep 16 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 16,
                treatment = "control",
                DOT = 2,
                herd_postion = "leader")
Waikerie_control_sheep18_1 <- read_csv(paste0(path_control, "Sheep 18 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 18,
                treatment = "control",
                DOT = 1,
                herd_postion = "herd")
Waikerie_control_sheep18_2 <- read_csv(paste0(path_control, "Sheep 18 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 18,
                treatment = "control",
                DOT = 2,
                herd_postion = "herd")
Waikerie_control_sheep20_1 <- read_csv(paste0(path_control, "Sheep 20 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 20,
                treatment = "control",
                DOT = 1,
                herd_postion = "herd")
Waikerie_control_sheep20_2 <- read_csv(paste0(path_control, "Sheep 20 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 20,
                treatment = "control",
                DOT = 2,
                herd_postion = "herd")
Waikerie_control_sheep24_1 <- read_csv(paste0(path_control, "Sheep 24 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 24,
                treatment = "control",
                DOT = 1,
                herd_postion = "follower")
Waikerie_control_sheep24_2 <- read_csv(paste0(path_control, "Sheep 24 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 24,
                treatment = "control",
                DOT = 2,
                herd_postion = "follower")

Waikerie_control_sheep26_1 <- read_csv(paste0(path_control, "Sheep 26 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 26,
                treatment = "control",
                DOT = 1,
                herd_postion = "herd")
Waikerie_control_sheep26_2 <- read_csv(paste0(path_control, "Sheep 26 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 26,
                treatment = "control",
                DOT = 2,
                herd_postion = "herd")

Waikerie_control_sheep29_1 <- read_csv(paste0(path_control, "Sheep 29 day 1.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 29,
                treatment = "control",
                DOT = 1,
                herd_postion = "leader")
Waikerie_control_sheep29_2 <- read_csv(paste0(path_control, "Sheep 29 day 2.csv"), skip = 42) %>% 
  dplyr::mutate(sheep = 29,
                treatment = "control",
                DOT = 2,
                herd_postion = "leader")



Waikerie_control_percent_all <- rbind(Waikerie_control_sheep4_1,
                                      Waikerie_control_sheep4_2,
                                      Waikerie_control_sheep6_1,
                                      Waikerie_control_sheep6_2,
                                      Waikerie_control_sheep8_1,
                                      Waikerie_control_sheep8_2,
                                      Waikerie_control_sheep16_1,
                                      Waikerie_control_sheep16_2,
                                      Waikerie_control_sheep18_1,
                                      Waikerie_control_sheep18_2,
                                      Waikerie_control_sheep20_1,
                                      Waikerie_control_sheep20_2,
                                      Waikerie_control_sheep24_1,
                                      Waikerie_control_sheep24_2,
                                      Waikerie_control_sheep26_1,
                                      Waikerie_control_sheep26_2,
                                      Waikerie_control_sheep29_1,
                                      Waikerie_control_sheep29_2)

rm(Waikerie_control_sheep4_1,
   Waikerie_control_sheep4_2,
   Waikerie_control_sheep6_1,
   Waikerie_control_sheep6_2,
   Waikerie_control_sheep8_1,
   Waikerie_control_sheep8_2,
   Waikerie_control_sheep16_1,
   Waikerie_control_sheep16_2,
   Waikerie_control_sheep18_1,
   Waikerie_control_sheep18_2,
   Waikerie_control_sheep20_1,
   Waikerie_control_sheep20_2,
   Waikerie_control_sheep24_1,
   Waikerie_control_sheep24_2,
   Waikerie_control_sheep26_1,
   Waikerie_control_sheep26_2,
   Waikerie_control_sheep29_1,
   Waikerie_control_sheep29_2)

Waikerie_all <- rbind(Waikerie_100_percent_all, Waikerie_control_percent_all, Waikerie_66_percent_all, Waikerie_33_percent_all)

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
          file = "W:/VF/Optimising_VF/Waikerie/data_prep/GPS_cood_step_1a.csv")
 


############################################################################################
############                  bring in HOBO data            ##############################
############################################################################################

### bring in animal logs for VF all
path_HOBO_PreVF <- "W:/VF/Optimising_VF/raw_data/Waikerie/Raw_hobo_behav/Hobo/PreVF/"

sheep4_1 <- read_csv(paste0(path_HOBO_PreVF, "Yellow_Sheep4_day1.csv")) %>% 
  dplyr::mutate(sheep = 4,
                DOT = 1)
sheep4_2 <- read_csv(paste0(path_HOBO_PreVF, "Yellow_Sheep4_day2.csv")) %>% 
  dplyr::mutate(sheep = 4,
                DOT = 2)

sheep1_1 <- read_csv(paste0(path_HOBO_PreVF, "Red_Sheep1_day1.csv")) %>% 
  dplyr::mutate(sheep = 1,
                DOT = 1)
sheep1_2 <- read_csv(paste0(path_HOBO_PreVF, "Red_Sheep1_day2.csv")) %>% 
  dplyr::mutate(sheep = 1,
                DOT = 2)

sheep3_1 <- read_csv(paste0(path_HOBO_PreVF, "Orange_sheep3_day1.csv")) %>% 
  dplyr::mutate(sheep = 3,
                DOT = 1)
sheep3_2 <- read_csv(paste0(path_HOBO_PreVF, "Orange_sheep3_day2.csv")) %>% 
  dplyr::mutate(sheep = 3,
                DOT = 2)

sheep5_1 <- read_csv(paste0(path_HOBO_PreVF, "Green_sheep5_day1.csv")) %>% 
  dplyr::mutate(sheep = 5,
                DOT = 1)
sheep5_2 <- read_csv(paste0(path_HOBO_PreVF, "Green_sheep5_day2.csv")) %>% 
  dplyr::mutate(sheep = 5,
                DOT = 2)

sheep2_1 <- read_csv(paste0(path_HOBO_PreVF, "Blue_Sheep2_day1.csv")) %>% 
  dplyr::mutate(sheep = 2,
                DOT = 1)
sheep2_2 <- read_csv(paste0(path_HOBO_PreVF, "Blue_Sheep2_day2.csv")) %>% 
  dplyr::mutate(sheep = 2,
                DOT = 2)
sheep6_1 <- read_csv(paste0(path_HOBO_PreVF, "Black_Sheep6_day1.csv")) %>% 
  dplyr::mutate(sheep = 6,
                DOT = 1)
sheep6_2 <- read_csv(paste0(path_HOBO_PreVF, "Black_Sheep6_day2.csv")) %>% 
  dplyr::mutate(sheep = 6,
                DOT = 2)


HOBO_PreVF <- rbind(sheep1_1,
                    sheep1_2,
                    sheep2_1,
                    sheep2_2, 
                    sheep3_1,
                    sheep3_2,
                    sheep4_1,
                    sheep4_2,
                    sheep5_1,
                    sheep5_2,
                    sheep6_1,
                    sheep6_2)
rm(
  sheep1_1,
  sheep1_2,
  sheep2_1,
  sheep2_2,
  sheep3_1,
  sheep3_2,
  sheep4_1,
  sheep4_2,
  sheep5_1,
  sheep5_2,
  sheep6_1,
  sheep6_2
)

### bring in animal logs for VF all
path_HOBO_PostVF <- "W:/VF/Optimising_VF/raw_data/Waikerie/Raw_hobo_behav/Hobo/PostVF/"

sheep1_1 <- read_csv(paste0(path_HOBO_PostVF, "Red_Sheep1_d6.csv")) %>% 
  dplyr::mutate(sheep = 1,
                DOT = 6)
sheep1_2 <- read_csv(paste0(path_HOBO_PostVF, "Red_Sheep1_d7.csv")) %>% 
  dplyr::mutate(sheep = 1,
                DOT = 7)

sheep2_1 <- read_csv(paste0(path_HOBO_PostVF, "Blue_Sheep2_d6.csv")) %>% 
  dplyr::mutate(sheep = 2,
                DOT = 6)
sheep2_2 <- read_csv(paste0(path_HOBO_PostVF, "Blue_Sheep2_d7.csv")) %>% 
  dplyr::mutate(sheep = 2,
                DOT = 7)

sheep3_1 <- read_csv(paste0(path_HOBO_PostVF, "Orange_sheep3_d6.csv")) %>% 
  dplyr::mutate(sheep = 3,
                DOT = 6)
sheep3_2 <- read_csv(paste0(path_HOBO_PostVF, "Orange_sheep3_d7.csv")) %>% 
  dplyr::mutate(sheep = 3,
                DOT = 7)


sheep4_1 <- read_csv(paste0(path_HOBO_PostVF, "Yellow_Sheep4__d6.csv")) %>% 
  dplyr::mutate(sheep = 4,
                DOT = 6)
sheep4_2 <- read_csv(paste0(path_HOBO_PostVF, "Yellow_Sheep4_d7.csv")) %>% 
  dplyr::mutate(sheep = 4,
                DOT = 7)

sheep5_1 <- read_csv(paste0(path_HOBO_PostVF, "Green_sheep5_d6.csv")) %>% 
  dplyr::mutate(sheep = 5,
                DOT = 6)
sheep5_2 <- read_csv(paste0(path_HOBO_PostVF, "Green_sheep5_d7.csv")) %>% 
  dplyr::mutate(sheep = 5,
                DOT = 7)
sheep6_1 <- read_csv(paste0(path_HOBO_PostVF, "Black_Sheep6_d6.csv")) %>% 
  dplyr::mutate(sheep = 6,
                DOT = 6)
sheep6_2 <- read_csv(paste0(path_HOBO_PostVF, "Black_Sheep6_d7.csv")) %>% 
  dplyr::mutate(sheep = 6,
                DOT = 7)

sheep7_1 <- read_csv(paste0(path_HOBO_PostVF, "Red_Sheep1_d6.csv")) %>% 
  dplyr::mutate(sheep = 7,
                DOT = 6)
sheep7_2 <- read_csv(paste0(path_HOBO_PostVF, "Red_Sheep1_d7.csv")) %>% 
  dplyr::mutate(sheep = 7,
                DOT = 7)


HOBO_PostVF <- rbind(sheep1_1,
                     sheep1_2,
                     sheep2_1,
                     sheep2_2,
                     sheep3_1,
                     sheep3_2,
                     sheep4_1,
                     sheep4_2,
                     sheep5_1,
                     sheep5_2,
                     sheep6_1,
                     sheep6_2,
                     sheep7_1,
                     sheep7_2)
rm(sheep1_1,
   sheep1_2,
   sheep2_1,
   sheep2_2,
   sheep3_1,
   sheep3_2,
   sheep4_1,
   sheep4_2,
   sheep5_1,
   sheep5_2,
   sheep6_1,
   sheep6_2,
   sheep7_1,
   sheep7_2)


### bring in animal logs for VF all
path_HOBO_VFtest <- "W:/VF/Optimising_VF/raw_data/Waikerie/Raw_hobo_behav/Hobo/VF test/"

sheep1_1 <- read_csv(paste0(path_HOBO_VFtest, "Red_Sheep1_d4.csv")) %>% 
  dplyr::mutate(sheep = 1,
                DOT = 4)
sheep1_2 <- read_csv(paste0(path_HOBO_VFtest, "Red_Sheep1_d5.csv")) %>% 
  dplyr::mutate(sheep = 1,
                DOT = 5)
sheep2_1 <- read_csv(paste0(path_HOBO_VFtest, "Blue_Sheep2_d4.csv")) %>% 
  dplyr::mutate(sheep = 2,
                DOT = 4)
sheep2_2 <- read_csv(paste0(path_HOBO_VFtest, "Blue_Sheep2_d5.csv")) %>% 
  dplyr::mutate(sheep = 2,
                DOT = 5)
sheep3_1 <- read_csv(paste0(path_HOBO_VFtest, "Orange_sheep3_d4.csv")) %>% 
  dplyr::mutate(sheep = 3,
                DOT = 4)
sheep3_2 <- read_csv(paste0(path_HOBO_VFtest, "Orange_sheep3_d5.csv")) %>% 
  dplyr::mutate(sheep = 3,
                DOT = 5)
sheep4_1 <- read_csv(paste0(path_HOBO_VFtest, "Yellow_Sheep4_d4.csv")) %>% 
  dplyr::mutate(sheep = 4,
                DOT = 4)
sheep4_2 <- read_csv(paste0(path_HOBO_VFtest, "Yellow_Sheep4_d5.csv")) %>% 
  dplyr::mutate(sheep = 4,
                DOT = 5)
sheep5_1 <- read_csv(paste0(path_HOBO_VFtest, "Green_sheep5_d4.csv")) %>% 
  dplyr::mutate(sheep = 5,
                DOT = 4)
sheep5_2 <- read_csv(paste0(path_HOBO_VFtest, "Green_sheep5_d5.csv")) %>% 
  dplyr::mutate(sheep = 5,
                DOT = 5)
sheep6_1 <- read_csv(paste0(path_HOBO_VFtest, "Black_Sheep6_d4.csv")) %>% 
  dplyr::mutate(sheep = 6,
                DOT = 4)
sheep6_2 <- read_csv(paste0(path_HOBO_VFtest, "Black_Sheep6_d5.csv")) %>% 
  dplyr::mutate(sheep = 6,
                DOT = 5)


HOBO_VFtest <- rbind(sheep1_1,
                     sheep1_2,
                     sheep2_1,
                     sheep2_2,
                     sheep3_1,
                     sheep3_2,
                     sheep4_1,
                     sheep4_2,
                     sheep5_1,
                     sheep5_2,
                     sheep6_1,
                     sheep6_2)
rm(sheep1_1,
   sheep1_2,
   sheep2_1,
   sheep2_2,
   sheep3_1,
   sheep3_2,
   sheep4_1,
   sheep4_2,
   sheep5_1,
   sheep5_2,
   sheep6_1,
   sheep6_2)

# "Blue_sheep_d3.csv"
# "Green_sheep_d3.csv"
# "Black_sheep_d3.csv"
# "Orange_sheep_d3.csv"
# "Red_sheep_d3.csv"
# "Yellow_Sheep_d3.csv"


################################################################################
####   The date time clm looks suss the year is 2017 and the trial was 2018 ####
### I am going to use the time out of this clm and assume its local time ??? ###
################################################################################

str(HOBO_VFtest)

HOBO_VFtest_1 <- HOBO_VFtest
HOBO_VFtest_1 <-  HOBO_VFtest_1 %>% separate(`Date Time`, into = c("dummy_date", "local_time"), sep=" ", remove=FALSE)
HOBO_VFtest_1 <-  HOBO_VFtest_1 %>% separate(local_time, into = c("test1", "test2", "test3"), sep=":")
str(HOBO_VFtest_1)

HOBO_VFtest_1$test3 <- as.double(HOBO_VFtest_1$test3)
HOBO_VFtest_1 <- HOBO_VFtest_1 %>% 
  mutate(local_time = paste0(test1, ":", test2,":", test3))

## add a real date

HOBO_VFtest_1 <-  HOBO_VFtest_1 %>%
  mutate(
    date = case_when(
      DOT == 1 ~ "13/03/2018",
      DOT == 2 ~ "14/03/2018",
      DOT == 3 ~ "15/03/2018",
      DOT == 4 ~ "16/03/2018",
      ))

HOBO_VFtest_1 <-  HOBO_VFtest_1 %>%
  mutate(date_time = paste0(date," ", local_time))

HOBO_VFtest_1$date_time <- as.POSIXct(HOBO_VFtest_1$date_time, format="%d/%m/%Y %H:%M:%S")

## remove dummy clm and rows with no data

str(HOBO_VFtest_1)
HOBO_VFtest_1 <- HOBO_VFtest_1 %>%  dplyr::select(-test1, 
                                                  -test2,
                                                  -test3,
                                                  -dummy_date)
HOBO_VFtest_1 <- HOBO_VFtest_1 %>% filter(!is.na(date_time))



## write out the file in CSV format
write.csv(HOBO_PreVF,row.names = FALSE,
          file = "W:/VF/Optimising_VF/Waikerie/data_prep/HOBO_PreVF.csv")
write.csv(HOBO_PostVF,row.names = FALSE,
          file = "W:/VF/Optimising_VF/Waikerie/data_prep/HOBO_PostVF.csv")
write.csv(HOBO_VFtest_1,row.names = FALSE,
          file = "W:/VF/Optimising_VF/Waikerie/data_prep/HOBO_VFtest.csv")

################################################################################
#####                      Behavioural data              #######################
###############################################################################

### bring in animal logs for VF all
path_behavioural <- "W:/VF/Optimising_VF/raw_data/Waikerie/Raw_hobo_behav/"


Behav_100 <- read_excel(paste0(path_behavioural, "VF_Behaviour.xlsx"), sheet = "100%") %>% 
  mutate(treatment = "100_percent")

Behav_33 <- read_excel(paste0(path_behavioural, "VF_Behaviour.xlsx"), sheet = "33%") %>% 
  mutate(treatment = "33_percent")

Behav_66 <- read_excel(paste0(path_behavioural, "VF_Behaviour.xlsx"), sheet = "66%") %>% 
  mutate(treatment = "66_percent") %>% 
  rename(`Time to graze(s)` = Sue)


Behav <- rbind(Behav_100, Behav_33, Behav_66)
rm(Behav_100, Behav_33, Behav_66)


### clean up the time clm the time was manually entered and is not GMT so convert to string

Behav$Time <- as.character(Behav$Time)
str(Behav)
Behav <-  Behav %>% separate(Time, into = c("dummy_date", "local_time"), sep=" ")
## add a real date

Behav <-  Behav %>%
  mutate(
    date = case_when(
      treatment == "100_percent" & Day == 1 ~ "13/03/2018",
      treatment == "100_percent" & Day == 2 ~ "14/03/2018",
      
      treatment == "33_percent" & Day == 1 ~ "15/03/2018",
      treatment == "33_percent" & Day == 2 ~ "16/03/2018",
      
      treatment == "66_percent" & Day == 1 ~ "15/03/2018",
      treatment == "66_percent" & Day == 2 ~ "16/03/2018"))

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
