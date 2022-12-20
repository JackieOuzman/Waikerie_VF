
install.packages("move")
library(move)
library(dplyr)

# username:JackieOuzman
# password:tEZa!6AjKKgzCst

#myMoveObject <- move(x="C:/Users/ouz001/working_from_home_post_Sep2022/Waikerie_VF/move_bank/GPS_data.csv")
data <- read.csv("W:/VF/Optimising_VF/Waikerie/data_prep/animal_GPS_data_step1_2_3.csv")
str(data)
data <- data %>%  dplyr::select(ID , time:DOY, X, Y )
data$sheep <- as.character(data$sheep)

# data_small <- data %>% 
#   filter(sheep == "11" | sheep == "12")
data_small <- data %>% 
  filter(sheep == "11" )

## arrange the time clm 
data_small <- data_small %>% 
  arrange(local_time)
## remove duplicates
data_small <- data_small %>% distinct(local_time, .keep_all = TRUE)

#CRS("+init=epsg:28354")
sheep_move <- move(x=data_small$X, y=data_small$Y, 
              time=as.POSIXct(data_small$local_time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), #2018-03-13 10:16:45 UTC
              proj=CRS("+init=epsg:28354"), #28354
              data=data_small, animal=data_small$sheep)#, 
              #sensor=data$sensor)#not sure what this is?

sheep_move


plot(sheep_move, xlab="Longitude", ylab="Latitude", type="l", pch=16, lwd=0.5)
points(sheep_move, pch=20, cex=0.5)
timeLag_matrix <-  timeLag(sheep_move, units = "mins")

move::distance(sheep_move)
