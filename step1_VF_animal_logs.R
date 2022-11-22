library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



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
                treatment = "leader",
                DOT = 2,
                herd_postion = "follower")
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
  dplyr::select(ID:time, sheep:herd_postion)
  mutate(timeOfEvent = as.POSIXct(timeOfEvent, tz = "GMT", 
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


rm(Waikerie_all_sf,Waikerie_all )


#-----Up to here-----#

#Next steps boundaries trim location and time
## merge in other animal data
## look for weather data ? anything else??
############################################################################################
############                  bring in boundaries             ##############################
############################################################################################




Chiswick_hard_fence_bound <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final.shp")  # this is the hard fences

Chiswick_hard_fence_bound <-
  st_transform(Chiswick_hard_fence_bound, crs = 28355)


Chiswick_hard_fence_bound_buff <- st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/Chiswick_paddock_boundary_final_buff10.shp")  # this is the 

Chiswick_hard_fence_bound_buff <-
  st_transform(Chiswick_hard_fence_bound_buff, crs = 28355)


VF_paddock <-   st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/VF_paddock.shp")

VF_paddock <-  st_transform(VF_paddock, crs = 28355)

water_pt <-  st_read("W:/VF/Sheep_Chiswick_2022/spatial_boundaries/water_pt.shp")





ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "all animal logs with a buffer of 10m")



ggplot() +
  geom_sf(data = Chiswick_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = Chiswick_hard_fence_bound_buff, color = "black", fill = NA) +
  geom_sf(data = water_pt ,color ="Blue") +
  geom_sf(data = animal_GPS_data_sf_trans ,alpha = 0.05) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
facet_wrap(. ~ date)+
  labs(title = "all animal logs, dates as facet")




str(animal_GPS_data_sf_trans)




# --------------------------------------------------------------------------------------------------------------------- #




################################################################################
#### filtering out data based on times trail 28/6- 9:50 and end 2/7- at 10:10 – this is based on the times Dana gave me

# start of trial (according to sue) - keep everything after  17th 11:35 or 11:40 s above

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  filter(
  local_time >=  ymd_hms("2022-06-28 09:50:00", tz= "Australia/Sydney"))

animal_GPS_data_sf_trans <- animal_GPS_data_sf_trans %>% 
  filter(
    local_time <=  ymd_hms("2022-07-02 10:10:00", tz= "Australia/Sydney"))








# Times sheep were brought in each day for the VF Chiswick trial;
# 28/6- sheep out 9:50
# 29/6 11:21- 12:21
# 30/6 10:34- 11:36
# 1/7- 10:37- 11:20
# 2/7- Brought in at 10:10
#### each day the animals were yarded so i need to remove this data

# let divide the data per day
day_28 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-06-28")
day_29 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-06-29")
day_30 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-06-30")
day_1 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-07-01")
day_2 <- animal_GPS_data_sf_trans %>%  filter(date == "2022-07-02")

# keep everything after before yarding and after yarding

day_29_before_yarding <- day_29 %>%
  filter(local_time <=  ymd_hms("2022-06-29 11:21:00", tz = "Australia/Sydney"))
day_29_after_yarding <- day_29 %>%
  filter(local_time >=  ymd_hms("2022-06-29 12:21:00", tz = "Australia/Sydney"))
                  
day_29_clean <- rbind(day_29_before_yarding, day_29_after_yarding)
rm(day_29_before_yarding, day_29_after_yarding, day_29)


day_30_before_yarding <- day_30 %>%
  filter(local_time <=  ymd_hms("2022-06-30 10:34:00", tz = "Australia/Sydney"))
day_30_after_yarding <- day_30 %>%
  filter(local_time >=  ymd_hms("2022-06-30 11:36:00", tz = "Australia/Sydney"))

day_30_clean <- rbind(day_30_before_yarding, day_30_after_yarding)
rm(day_30_before_yarding, day_30_after_yarding, day_30)

day_1_before_yarding <- day_1 %>%
  filter(local_time <=  ymd_hms("2022-07-01 10:37:00", tz = "Australia/Sydney"))
day_1_after_yarding <- day_1 %>%
  filter(local_time >=  ymd_hms("2022-07-01 11:20:00", tz = "Australia/Sydney"))

day_1_clean <- rbind(day_1_before_yarding, day_1_after_yarding)
rm(day_1_before_yarding, day_1_after_yarding, day_1)


### put it back togther 

animals_GPS_trim_time <- rbind(day_28, day_29_clean, day_30_clean, day_1_clean, day_2)

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



