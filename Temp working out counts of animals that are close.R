library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)


matrix_66percent <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step6_dist_between_animals_matrix_66.csv") 
matrix_66percent
rm(df)
reg_time_step <- matrix_66percent %>% 
  distinct(time_step)
df <- reg_time_step %>%  arrange(time_step)
matrix_66percent

#for treatment = 66 % for sheep7
sheep_list7 <- c(
  #"time_step",
  "dist10vs7",
  "dist15vs7",
  #"19vs7",
  "dist21vs7",
  "dist27vs7",
  "dist28vs7",
  "dist33vs7",
  "dist36vs7")


sheep7 <-   matrix_66percent %>%  dplyr::select(all_of(sheep_list7 ))


sheep7



sheep7$num <- ncol(sheep7) - rowSums(is.na(sheep7) | sheep7 == "")

sheep7_matrix <-   matrix_66percent %>%  dplyr::select(time_step)

sheep7_matrix <- cbind(sheep7_matrix, sheep7$num)
#remove the extra 1 its generated