library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)

  
GPS_Sheep <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step5_Greg_time_step_dist_travelled.csv") 
GPS_Sheep$local_time <- as.POSIXct(GPS_Sheep$local_time,  tz = "Australia/Adelaide")          
GPS_Sheep$time_step <- as.POSIXct(GPS_Sheep$time_step,  tz = "Australia/Adelaide") 

str(GPS_Sheep)
names(GPS_Sheep)

reg_time_step <- GPS_Sheep %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)

#for treatment = 100 % 
sheep_list <- c("2vs3",
               "3vs2",
               "5vs2",
               "13vs2",
               "14vs2",
               "17vs2",
               "22vs2",
               "30vs2",
               "35vs2",
               "3vs2",
               "5vs2",
               "13vs2",
               "14vs2",
               "17vs2",
               "22vs2",
               "30vs2",
               "35vs2",
               "13vs5",
               "14vs5",
               "17vs5",
               "22vs5",
               "30vs5",
               "35vs5",
               "14vs13",
               "17vs13",
               "22vs13",
               "30vs13",
               "35vs13",
               "17vs14",
               "22vs14",
               "30vs14",
               "35vs14",
               "22vs17",
               "30vs17",
               "35vs17",
               "30vs22",
               "35vs22",
               "35vs30"
)


#for treatment = 33 % 
sheep_list <- c("9vs1",
"11vs1",
"12vs1",
"23vs1",
"25vs1",
"31vs1",
"32vs1",
"34vs1",
"11vs9",
"12vs9",
"23vs9",
"25vs9",
"31vs9",
"32vs9",
"34vs9",
"12vs11",
"23vs11",
"25vs11",
"31vs11",
"32vs11",
"34vs11",
"23vs12",
"25vs12",
"31vs12",
"32vs12",
"34vs12",
"25vs23",
"31vs23",
"32vs23",
"34vs23",
"31vs25",
"32vs25",
"34vs25",
"32vs31",
"34vs31",
"34vs32")

rm(df)
df <- reg_time_step %>%  arrange(time_step)
################################################################################
#for treatment = control % 
sheep_list <- c(
"6vs4",
"8vs4",
"16vs4",
"18vs4",
"20vs4",
"24vs4",
"26vs4",
"29vs4",
"8vs6",
"16vs6",
"18vs6",
"20vs6",
"24vs6",
"26vs6",
"29vs6",
"16vs8",
"18vs8",
"20vs8",
"24vs8",
"26vs8",
"29vs8",
"18vs16",
"20vs16",
"24vs16",
"26vs16",
"29vs16",
"20vs18",
"24vs18",
"26vs18",
"29vs18",
"24vs20",
"26vs20",
"29vs20",
"26vs24",
"29vs24",
"29vs26")

rm(df)
df <- reg_time_step %>%  arrange(time_step)

###############################################################################
#for treatment = 66 % 
sheep_list <- c(
"10vs7",
"15vs7",
#"19vs7",
"21vs7",
"27vs7",
"28vs7",
"33vs7",
"36vs7",
"15vs10",
#"19vs10",
"21vs10",
"27vs10",
"28vs10",
"33vs10",
"36vs10",
#"19vs15",
"21vs15",
"27vs15",
"28vs15",
"33vs15",
"36vs15",
# "21vs19",
# "27vs19",
# "28vs19",
# "33vs19",
# "36vs19",
"27vs21",
"28vs21",
"33vs21",
"36vs21",
"28vs27",
"33vs27",
"36vs27",
"33vs28",
"36vs28",
"36vs33")
rm(df)
df <- reg_time_step %>%  arrange(time_step)

for (sheep_list in sheep_list){
  
  comparison_a_b <- as.data.frame(str_split(sheep_list, "vs"),
                               col.names = "Sheep_ID" )
  
  comparison_a <- comparison_a_b[1,1]
  comparison_b <- comparison_a_b[2,1]

  GPS_Sheep_comparsion_a <- GPS_Sheep %>% filter(sheep == comparison_a) %>% mutate(comparison_label = "a")
  GPS_Sheep_comparsion_b <- GPS_Sheep %>% filter(sheep == comparison_b) %>% mutate(comparison_label = "b")
  
  GPS_Sheep_comparsion <- rbind(GPS_Sheep_comparsion_a, GPS_Sheep_comparsion_b)
  
  GPS_Sheep_comparsion <- GPS_Sheep_comparsion %>% dplyr::select(time_step, X, Y,comparison_label)
  
  GPS_Sheep_comparsion
  
  ## make it wide
  GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion %>% 
    pivot_wider(names_from = comparison_label, values_from = c(X,Y))
    GPS_Sheep_comparsion_wide
  
    
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>% 
      dplyr::mutate(
    dist = sqrt(  ((X_a - X_b)^ 2) + (Y_a - Y_b)^ 2))
    
  
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>%  dplyr::select(time_step , dist)
    GPS_Sheep_comparsion_wide <- GPS_Sheep_comparsion_wide %>%
        rename(!!paste0(paste0("dist",comparison_a,"vs", comparison_b)) := dist)
    
    df = left_join(df, GPS_Sheep_comparsion_wide)
    df <- df %>% arrange(time_step)
    rm(
      comparison_a,
      comparison_b,
      comparison_a_b,
      GPS_Sheep_comparsion_a,
      GPS_Sheep_comparsion_b,
      GPS_Sheep_comparsion,
      GPS_Sheep_comparsion_wide
    )  
}    


#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_100percent.csv")
#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_33percent.csv")
#write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_control.csv")
write.csv(df, "W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_66.csv")


### missing number 19 not sure why?