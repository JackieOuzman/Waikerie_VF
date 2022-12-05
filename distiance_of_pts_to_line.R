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



GPS <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/animal_GPS_data_step1_2_3.csv")

#turn into spatial data
GPS <-   st_as_sf(GPS,
                         coords = c("X", "Y"),
                         crs = 28354,
                         agr = "constant")

GPS <- GPS %>% dplyr::select (ID, sheep, treatment,DOT,herd_postion,local_time, date,DOY, geometry)
names(GPS)
GPS_sheep2 <- GPS %>%  filter(sheep == 2)

coordinates <-as.data.frame( st_coordinates(GPS_sheep2))
GPS_sheep2_df <- as.data.frame(GPS_sheep2)

GPS_sheep2_df <- GPS_sheep2_df %>% 
  dplyr::select(-"geometry")


GPS_sheep2_df <-   cbind(GPS_sheep2_df,coordinates )

# write.csv(GPS_sheep2_df, 
#           paste0("W:/VF/Optimising_VF/Waikerie/data_prep/","/GPS_sheep2_df_test_dist_func.csv"), 
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
  
  geom_sf(data = GPS_sheep2 ,alpha = 0.03) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "check")

############################################################################################

GPS_sheep2
VF_line

GPS_sheep2 <- GPS_sheep2 %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS_sheep2, VF_line))
############################################################################################
### report if the point is in the exclusion zone

VF <- VF %>%  dplyr::select(Id, geometry)

VF_points <-  st_intersection(GPS_sheep2, VF) %>% 
  dplyr::mutate(VF_EX = "inside_VF")

Exclusion_points <-  st_intersection(GPS_sheep2, exclusion_zone)%>% 
  dplyr::mutate(VF_EX = "outside_VF")

names(VF_points)
names(Exclusion_points)

GPS_sheep2 <- rbind(VF_points, Exclusion_points)

str(GPS_sheep2)


GPS_sheep2 <- GPS_sheep2 %>% dplyr::select(ID:DOY, dist_to_VF, VF_EX, geometry)



coordinates <-as.data.frame( st_coordinates(GPS_sheep2))
GPS_sheep2_df <- as.data.frame(GPS_sheep2)

GPS_sheep2_df <- GPS_sheep2_df %>% 
  dplyr::select(-"geometry")

names(GPS_sheep2_df)
GPS_sheep2_df <-   cbind(GPS_sheep2_df,coordinates )





path_output_files <- "W:/VF/Optimising_VF/Waikerie/data_prep/" 
path_output_files
write.csv(GPS_sheep2_df, 
          paste0(path_output_files,"/GPS_sheep2_df_dist_line_VF_zone_test.csv"), 
          row.names=FALSE)






################################################################################
#####   The below code is just checking on the few that didnt join       ######
#### Its not an issue because its only really a few on the boundary.     ######

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


