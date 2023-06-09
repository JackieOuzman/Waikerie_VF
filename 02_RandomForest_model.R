
libs <- c("tidyverse", "dplyr",  "ggplot2", "randomForest", "Hmisc")


install.libraries <- function(lib=NULL){
  new <- lib[!(lib %in% installed.packages()[, "Package"])]
  if (length(new)){   
    install.packages(new, dependencies = TRUE)
  }
} 

load.libraries <- function(lib=NULL){
  sapply(libs, require, character.only = TRUE)
}

install.libraries(libs)
load.libraries(libs)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 



baseDir <- "W:/VF/Optimising_VF/Lameroo/data_prep/RF_model_outputs/"
outDir <- "W:/VF/Optimising_VF/Lameroo/data_prep/RF_model_outputs/"


#loading the model
RF_modelVs1 = readRDS(paste0(baseDir, "RF_model_vs1.rda"))

RF_modelVs1



#loading the data for predication 
#day 1 of the trial


RF_df_DOT1 <- read_csv(paste0(outDir, "step9_RF_df_input_DOT1.csv"))

RF_df_DOT1[ is.na(RF_df_DOT1) ] <- 0
str(RF_df_DOT1)

# This is the variable used in the RF model - so I just want these in my new data frame for prediction
# •	compliance_score
# •	mean_dist_frm_VF_inside_inclusion
# •	max_dist_frm_VF_inside_inclusion"  
# •	mean_dist_frm_VF_outside_inclusion
# •	total_dist_travel
# •	total_audio
# •	total_pulse
# •	ratioprop_resting
# •	prop_walking
# •	prop_running
# •	mean_number_animals_close


         


RF_df_DOT1_for_model <- RF_df_DOT1 %>%  dplyr::select(
  mean_dist_frm_VF_inside_inclusion,
  max_dist_frm_VF_inside_inclusion,
  mean_dist_frm_VF_outside_inclusion,
  total_dist_travel,
  total_audio,
  total_pulse,
  ratio,
  prop_resting,
  prop_walking,
  prop_running,
  mean_number_animals_close
)

str(RF_df_DOT1_for_model)

pred_test <- predict(RF_modelVs1,
                     newdata = RF_df_DOT1_for_model,
                     type = "class")
pred_test #this gives me a list of my sheep and what class they are assigned to - I hope?

Check_output_of_RF_model <- as.data.frame(pred_test)
str(RF_df_DOT1)

sheep_ID_compliance_score <- RF_df_DOT1 %>%  dplyr::select (sheep) 

sheep_ID_compliance_score_withRF_results <- cbind(sheep_ID_compliance_score, Check_output_of_RF_model)
sheep_ID_compliance_score_withRF_results <- sheep_ID_compliance_score_withRF_results %>% 
  mutate(pred_compliance_score = case_when(
    pred_test == 1 ~ "non_compliant",
    pred_test == 0 ~ "compliant"
    
  ))


write.csv(sheep_ID_compliance_score_withRF_results, 
          paste0(outDir,"/sheep_ID_compliance_score_withRF_results_DOT1.csv"), 
          row.names=FALSE)




