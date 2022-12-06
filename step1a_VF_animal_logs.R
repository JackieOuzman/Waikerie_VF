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
path_HOBO_VFtest <- "W:/VF/Optimising_VF/raw_data/Waikerie/Raw_hobo_behav/Hobo_revised/"
treatment <- "33%d1"


sheep1_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 1clip.csv")) %>% 
  dplyr::mutate(sheep = 1,  day = 1)
sheep9_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep 9 clip.csv")) %>% 
  dplyr::mutate(sheep = 9,  day = 1)
sheep12_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep 12clip.csv")) %>% 
  dplyr::mutate(sheep = 12,  day = 1)
sheep23_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep 23clip.csv")) %>% 
  dplyr::mutate(sheep = 23,  day = 1)
sheep25_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep 25clip.csv")) %>% 
  dplyr::mutate(sheep = 25,  day = 1)
sheep31_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep 31clip.csv")) %>% 
  dplyr::mutate(sheep = 31,  day = 1)
sheep32_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep 32clip.csv")) %>% 
  dplyr::mutate(sheep = 32,  day = 1)
sheep34_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep 34clip.csv")) %>% 
  dplyr::mutate(sheep = 34,  day = 1)
sheep11_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/", "Sheep11clip.csv")) %>% 
  dplyr::mutate(sheep = 11,  day = 1)

sheep_day1_33_percent <- rbind(sheep1_1, sheep9_1, sheep12_1, sheep23_1, sheep25_1,sheep31_1, sheep32_1, sheep34_1, sheep11_1)
rm(sheep1_1, sheep9_1, sheep12_1, sheep23_1, sheep25_1,sheep31_1, sheep32_1, sheep34_1, sheep11_1)

treatment <- "33%d2"


sheep1_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 1clip.csv")) %>% 
  dplyr::mutate(sheep = 1,  day = 2)
sheep34_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 34clip.csv")) %>% 
  dplyr::mutate(sheep = 34,  day = 2)
sheep9_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 9clip.csv")) %>% 
  dplyr::mutate(sheep = 9,  day = 2)
sheep11_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 11clip.csv")) %>% 
  dplyr::mutate(sheep = 11,  day = 2)
sheep12_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 12clip.csv")) %>% 
  dplyr::mutate(sheep = 12,  day = 2)
sheep23_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 23clip.csv")) %>% 
  dplyr::mutate(sheep = 23,  day = 2)
sheep25_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 25clip.csv")) %>% 
  dplyr::mutate(sheep = 25,  day = 2)
sheep31_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 31clip.csv")) %>% 
  dplyr::mutate(sheep = 31,  day = 2)
sheep32_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 32clip.csv")) %>% 
  dplyr::mutate(sheep = 32,  day = 2)
sheep34_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep 34clip.csv")) %>% 
  dplyr::mutate(sheep = 34,  day = 2)
sheep_day2_33_percent <- rbind(sheep1_2, sheep9_2, sheep12_2, sheep23_2, sheep25_2,sheep31_2, sheep32_2, sheep34_2, sheep11_2)
rm(sheep1_2, sheep9_2, sheep12_2, sheep23_2, sheep25_2,sheep31_2, sheep32_2, sheep34_2, sheep11_2)

Sheep33percent <-rbind(sheep_day1_33_percent,sheep_day2_33_percent)
Sheep33percent <- Sheep33percent %>%  mutate(treatment = "33_percent")


treatment <- "66%d1"

sheep36_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep36clip.csv")) %>%   dplyr::mutate(sheep = 36,  day = 1)
sheep7_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep7clip.csv")) %>%     dplyr::mutate(sheep = 7,  day = 1)
sheep10_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep10clip.csv")) %>%     dplyr::mutate(sheep = 10,  day = 1)
sheep15_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep15clip.csv")) %>%     dplyr::mutate(sheep = 15,  day = 1)
sheep19_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep19clip.csv")) %>%     dplyr::mutate(sheep = 19,  day = 1)
sheep21_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep21clip.csv")) %>%     dplyr::mutate(sheep = 21,  day = 1)
sheep27_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep27clip.csv")) %>%     dplyr::mutate(sheep = 27,  day = 1)
sheep28_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep28clip.csv")) %>%     dplyr::mutate(sheep = 28,  day = 1)
sheep33_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep33clip.csv")) %>%     dplyr::mutate(sheep = 33,  day = 1)

sheep_day1_66_percent <- rbind(sheep36_1, sheep7_1, sheep10_1,sheep15_1,sheep19_1,  sheep21_1, sheep27_1,  sheep28_1, sheep33_1)
rm(sheep36_1, sheep7_1, sheep10_1,sheep15_1,sheep19_1,  sheep21_1, sheep27_1,  sheep28_1, sheep33_1)

treatment <- "66%d2"

sheep36_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep36clip.csv")) %>%   dplyr::mutate(sheep = 36,  day = 2)
sheep7_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep7clip.csv")) %>%     dplyr::mutate(sheep = 7,  day = 2)
sheep10_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep10clip.csv")) %>%     dplyr::mutate(sheep = 10,  day = 2)
sheep15_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep15clip.csv")) %>%     dplyr::mutate(sheep = 15,  day = 2)
sheep19_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep19clip.csv")) %>%     dplyr::mutate(sheep = 19,  day = 2)
sheep21_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep21clip.csv")) %>%     dplyr::mutate(sheep = 21,  day = 2)
sheep27_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep27clip.csv")) %>%     dplyr::mutate(sheep = 27,  day = 2)
sheep28_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep28clip.csv")) %>%     dplyr::mutate(sheep = 28,  day = 2)
sheep33_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep33clip.csv")) %>%     dplyr::mutate(sheep = 33,  day = 2)

sheep_day2_66_percent <- rbind(sheep36_2, sheep7_2, sheep10_2,sheep15_2,sheep19_2,  sheep21_2, sheep27_2,  sheep28_2, sheep33_2)
rm(sheep36_2, sheep7_2, sheep10_2,sheep15_2,sheep19_2,  sheep21_2, sheep27_2,  sheep28_2, sheep33_2)
Sheep66percent <-rbind(sheep_day1_66_percent, sheep_day2_66_percent)
Sheep66percent <- Sheep66percent %>%  mutate(treatment = "66_percent")

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
#sheep13_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep13clip.csv")) %>%   dplyr::mutate(sheep = 13,  day = 2)
sheep14_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep14clip.csv")) %>%   dplyr::mutate(sheep = 14,  day = 2)
sheep17_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep17clip.csv")) %>%   dplyr::mutate(sheep = 17,  day = 2)
sheep22_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep22clip.csv")) %>%   dplyr::mutate(sheep = 22,  day = 2)
sheep30_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep30clip.csv")) %>%   dplyr::mutate(sheep = 30,  day = 2)

sheep_day2_100_percent <- rbind(sheep35_2, sheep2_2, sheep3_2, sheep5_2, sheep14_2, sheep17_2, sheep22_2, sheep30_2) # missing this file sheep13_2,
rm(sheep35_1, sheep2_1, sheep3_1, sheep5_1,  sheep14_1, sheep17_1, sheep22_1, sheep30_1)

Sheep100percent <-rbind(sheep_day1_100_percent, sheep_day2_100_percent)
Sheep100percent <- Sheep100percent %>%  mutate(treatment = "100_percent")

treatment <- "Controlsd1"

sheep29_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep29clip.csv")) %>%   dplyr::mutate(sheep = 29,  day = 1)
sheep4_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep4clip.csv")) %>%   dplyr::mutate(sheep = 4,  day = 1)
sheep8_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep8clip.csv")) %>%   dplyr::mutate(sheep = 8,  day = 1)
sheep20_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep20clip.csv")) %>%   dplyr::mutate(sheep = 20,  day = 1)
sheep24_1 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep24clip.csv")) %>%   dplyr::mutate(sheep = 24,  day = 1)

sheep_day1_control <- rbind(sheep29_1, sheep4_1, sheep8_1, sheep20_1, sheep24_1) 
rm(sheep29_1, sheep4_1, sheep8_1, sheep20_1, sheep24_1)

treatment <- "Controlsd2"

sheep29_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep29clip.csv")) %>%   dplyr::mutate(sheep = 29,  day = 2)
sheep4_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep4clip.csv")) %>%   dplyr::mutate(sheep = 4,  day = 2)
sheep8_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep8clip.csv")) %>%   dplyr::mutate(sheep = 8,  day = 2)
sheep20_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep20clip.csv")) %>%   dplyr::mutate(sheep = 20,  day = 2)
sheep24_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep24clip.csv")) %>%   dplyr::mutate(sheep = 24,  day = 2)
sheep18_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep18clip.csv")) %>%   dplyr::mutate(sheep = 18,  day = 2)
sheep26_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep26clip.csv")) %>%   dplyr::mutate(sheep = 26,  day = 2)
sheep6_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep6clip.csv")) %>%   dplyr::mutate(sheep = 6,  day = 2)
sheep13_2 <- read_csv(paste0(path_HOBO_VFtest,  treatment, "/","Sheep13clip.csv")) %>%   dplyr::mutate(sheep = 13,  day = 2)

sheep_day2_control <- rbind(sheep29_2, sheep4_2, sheep8_2, sheep20_2, sheep24_2, sheep18_2, sheep26_2,sheep6_2,  sheep13_2) 
rm(sheep29_2, sheep4_2, sheep8_2, sheep20_2, sheep24_2, sheep18_2, sheep26_2,sheep6_2,  sheep13_2)


Sheep_control <-rbind(sheep_day1_control, sheep_day2_control)
Sheep_control <- Sheep_control %>%  mutate(treatment = "control")


Hobo_trial_data <- rbind(Sheep_control, Sheep100percent, Sheep66percent, Sheep33percent)
str(Hobo_trial_data)
################################################################################
####   The date time clm looks suss the year is 2017 and the trial was 2018 ####
### I am going to use the time out of this clm and assume its local time ??? ###
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
