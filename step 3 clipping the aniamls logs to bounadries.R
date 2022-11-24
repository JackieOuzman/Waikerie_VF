## step 3 clipping the data - This has not been run yet

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

hard_fence_bound <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Boundary_25m.shp")  # this is the hard fences

VF <- st_read("W:/VF/Optimising_VF/raw_data/Waikerie/BoundaryV2/Boundary_VF_paddock.shp")

############################################################################################



################################################################
### Clip to the VF hard fences  with 10 meter buffer   #########
################################################################



step1_2 <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/animals_GPS_trim_time_step1.csv")

#turn into spatial data
step1_2_sf <-   st_as_sf(step1_2,
                       coords = c("X", "Y"),
                       crs = 28354,
                       agr = "constant")







#To the large block boundary
step1_2_sf_clip <-
  st_intersection(step1_2_sf, hard_fence_bound)

### check

ggplot() +
  geom_sf(data = hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF, color = "black", fill = NA) +
  geom_sf(data = step1_2_sf_clip ,alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )






path_output_files <- "W:/VF/Optimising_VF/Waikerie/data_prep/" 
path_output_files
write.csv(step1_2_sf_clip_df, 
          paste0(path_output_files,"/animal_GPS_data_step1_2_3.csv"), 
          row.names=FALSE)
