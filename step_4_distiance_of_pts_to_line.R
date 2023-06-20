### working how to cal a distance from a line.
#https://gis.stackexchange.com/questions/360675/how-to-calculate-the-distance-of-points-to-line-in-r
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)



GPS <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step3_clip.csv")

#turn into spatial data
GPS <-   st_as_sf(GPS,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")

GPS <- GPS %>% dplyr::select (ID, sheep, treatment,DOT,herd_postion,local_time, date,DOY, geometry)
names(GPS)
#GPS_sheep2 <- GPS %>%  filter(sheep == 2)
GPS_sheep_all <- GPS 

coordinates <-as.data.frame( st_coordinates(GPS_sheep_all))
GPS_sheep_all_df <- as.data.frame(GPS_sheep_all)

GPS_sheep_all_df <- GPS_sheep_all_df %>% 
  dplyr::select(-"geometry")


GPS_sheep_all_df <-   cbind(GPS_sheep_all_df,coordinates )

# write.csv(GPS_sheep_all_df,
#           paste0("W:/VF/Optimising_VF/Waikerie/data_prep/","/GPS_sheepall_df_test_dist_func.csv"),
#           row.names=FALSE)
############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

hard_fence_bound <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Boundary_25m.shp")  # this is the hard fences

VF <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/VF_zone.shp")

exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Exclusion_zone_v2.shp")
exclusion_zone <-   st_as_sf(exclusion_zone,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")


VF_line <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/VFLine28m on line.shp")

############################################################################################

### check by plotting

ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_line, color = "red", fill = NA) +
  geom_sf(data = exclusion_zone, color = "blue", fill = NA) +
  
  geom_sf(data = GPS ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")




date_13_14_keep_these <- c(2,3,5,13,14,17,22,30,35) #100% trial was run on the 13th and 14th 
date_15_16_keep_these <- c(12,23,25, 10,15,21,27,33,36)  # 33% and 66% trial was run 15th and 16th


Check_1 <- GPS %>%
  filter(date == "2018-03-13" | date == "2018-03-14")  #%>%
  filter(sheep %in% date_13_14_keep_these)

unique(Check_1$sheep)


Check_2 <-  GPS %>%  
  filter(date == "2018-03-15" |date == "2018-03-16")  %>%
  filter(sheep %in% date_15_16_keep_these)



ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_line, color = "red", fill = NA) +
  geom_sf(data = exclusion_zone, color = "blue", fill = NA) +
  
  geom_sf(data = Check_1 ,alpha = 0.03, color ="black") +
  geom_sf(data = Check_2 ,alpha = 0.03, color ="blue") +
 
  
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")


############################################################################################

GPS
VF_line

GPS <- GPS %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS, VF_line))
############################################################################################
### report if the point is in the exclusion zone

VF <- VF %>%  dplyr::select(Id, geometry)

VF_points <-  st_intersection(GPS, st_difference(VF)) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS, st_difference(exclusion_zone))%>% 
  dplyr::mutate(VF_EX = "outside_VF")

names(VF_points)
names(Exclusion_points)

GPS_all <- rbind(VF_points, Exclusion_points)

str(GPS_all)


GPS_all <- GPS_all %>% dplyr::select(ID:DOY, dist_to_VF, VF_EX, geometry)



coordinates <-as.data.frame( st_coordinates(GPS_all))
GPS_all_df <- as.data.frame(GPS_all)

GPS_all_df <- GPS_all_df %>% 
  dplyr::select(-"geometry")

names(GPS_all_df)
GPS_all_df <-   cbind(GPS_all_df,coordinates )


names(GPS_all_df)


path_output_files <- "W:/VF/Optimising_VF/Waikerie/data_prep/" 
path_output_files
write.csv(GPS_all_df, 
          paste0(path_output_files,"/step4_dist_line_VF_zone.csv"), 
          row.names=FALSE)






################################################################################
#####   The below code is just checking on the few that didnt join       ######
#### Its not an issue because its only really a few on the boundary.     ######

str(GPS_all_df)
GPS_all_df_Exc_only <- GPS_all %>% filter(VF_EX == "outside_VF")



Check_1 <- GPS_all_df_Exc_only %>%
  filter(date == "2018-03-13" | date == "2018-03-13")  %>%
  filter(sheep %in% date_13_14_keep_these)


Check_2 <-  GPS_all_df_Exc_only %>%  
  filter(date == "2018-03-15" |date == "2018-03-16")  %>%
  filter(sheep %in% date_15_16_keep_these)



ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_line, color = "red", fill = NA) +
  geom_sf(data = exclusion_zone, color = "blue", fill = NA) +
  
  geom_sf(data = Check_1 ,alpha = 0.03, color ="black") +
  geom_sf(data = Check_2 ,alpha = 0.03, color ="blue") +
  
  
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")




#we have quite a few records that are logged in the exclusion zone

str(Check_1)

count_exclusion_zone_occurance_per_animal_1 <- Check_1 %>%  group_by( sheep) %>% 
  summarise(count_records = n())
count_exclusion_zone_occurance_per_animal_1

count_exclusion_zone_occurance_per_animal_2 <- Check_2 %>%  group_by( sheep) %>% 
  summarise(count_records = n())
count_exclusion_zone_occurance_per_animal_2











# test2 <- st_join(GPS_sheep2, test)
# str(test2)
# why <-  test2 %>%  dplyr::filter(is.na(VF_EX))




# ggplot() +
#    geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
#    geom_sf(data = VF_line, color = "red", fill = NA) +
#   geom_sf(data = exclusion_zone, color = "blue", fill = NA) +
#   
#   geom_sf(data = VF_points ,alpha = 0.08, color ="red") +
#   geom_sf(data = Exclusion_points ,alpha = 0.08, color ="green") +
#   geom_sf(data = why ,alpha = 1.0, color ="black") +
#   
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title = "check")

dim(GPS_sheep2)
dim(VF_points)
dim(Exclusion_points)


