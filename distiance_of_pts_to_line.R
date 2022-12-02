### working how to calualte a distance from a line.
#https://gis.stackexchange.com/questions/360675/how-to-calculate-the-distance-of-points-to-line-in-r




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

write.csv(GPS_sheep2_df, 
          paste0("W:/VF/Optimising_VF/Waikerie/data_prep/","/GPS_sheep2_df_test_dist_func.csv"), 
          row.names=FALSE)
############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

hard_fence_bound <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Boundary_25m.shp")  # this is the hard fences

VF <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Boundary_VF_paddock.shp")

exclusion_zone <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Exclusion_zone.shp")
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

#this code sort of works but the numbers seem a bit high - nope I checked in arcmap they are correct?

GPS_sheep2
VF_line

GPS_sheep2 <- GPS_sheep2 %>% 
  dplyr::mutate(dist_to_VF = st_distance(GPS_sheep2, VF_line))
############################################################################################
### report if the point is in the exclusion zone

### --- stuck here point inside polygon and points outside polygon.
#need to make a new clm for location in paddock
dim(GPS_sheep2)
dim(VF_points)

VF <- VF %>%  dplyr::select(Id, geometry)

VF_points <-  st_intersection(GPS_sheep2, VF)
Exclusion_points <-  st_intersection(GPS_sheep2, exclusion_zone)
dim(GPS_sheep2)
dim(VF_points)
dim(VF_points)


st_contains(x, y, ...)