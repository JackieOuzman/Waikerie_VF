library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


collared_animals <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step8_all_animals_collars.csv")

collared_animals$local_time <- as.POSIXct(collared_animals$local_time,  tz = "Australia/Melbourne") #to match what was defined in early steps

str(collared_animals)

RF_df <- collared_animals %>% distinct(sheep, .keep_all = TRUE) %>% 
  dplyr::select(sheep, compliance_score)
RF_df %>% distinct(compliance_score)
RF_df <- RF_df %>%  filter(compliance_score != "NA" )

#make df with 2 behaviour stages and 2 compliance_score
RF_df <- data.frame(behaviour_stage = c("Early behaviour", "Early behaviour","Later behaviour", "Later behaviour"),
                    compliance_score = c("compliant","non_compliant", "compliant","non_compliant"))
                 



################################################################################
####    remove the time logs I don't want      ###########################
################################################################################


#nothing selected here - other trial we had training period defined


################################################################################
### Note these trials were only 2 days long - another idea is to only keep 100% trial VF collared animals
### Add DOY clm

date_13_14_keep_these <- c(2,3,5,13,14,17,22,30,35) #100% trial was run on the 13th and 14th 


### Add DOY clm
temp <- collared_animals %>% 
  filter(!is.na(DOY ))

min_DOY <- min(temp$DOY, na.rm = TRUE)
max_DOY <- max(temp$DOY, na.rm = TRUE)

collared_animals <- collared_animals %>% 
  mutate(DOT = (DOY - min_DOY)+1 )

collared_animals$DOY <- as.double(collared_animals$DOY )


rm(temp)
################################################################################
### Definition of early behaviour ### Note these trials were only 2 days long
################################################################################

str(collared_animals)

collared_animals <- collared_animals %>% 
  mutate(
    behaviour_stage = case_when(
      DOT == 1 ~ "Early behaviour",
      DOT > 1 ~ "Later behaviour"))


################################################################################
### Hours for early behaviour and hours for later behaviour
str(collared_animals)

hours_behav <- collared_animals %>% count(behaviour_stage, sheep)
hours_behav$n <- as.double(hours_behav$n)


hours_behav <-  hours_behav %>% mutate(mins = n *10) #each data point is a 10min log

hours_behav


#####################################################################################
### add in the distance between animals
######################################################################################

dist_bewteen <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step7_count_close_animals_100percent.csv")



## make a new variable hours
dist_bewteen$time_step <- as.POSIXct(dist_bewteen$time_step,  tz = "Australia/Melbourne")
dist_bewteen <- dist_bewteen %>%  dplyr::mutate(date= date(time_step))  

#### add to the collared animals df
collared_animals <- left_join(collared_animals, dist_bewteen)



#####################################################################################

#####################################################################################
### what is the average distance to the fence?
## average distance to VF when inside inclusion zone and when outside inclusion zone
## max distance to Vf 

######################################################################################

str(collared_animals)
#----create new clms distance from VF when inside inclusion zone and when outside inclusion zone
#---- if the record is not in the inclusion zone it gets an NA for dist_frm_VF_inside_inclusion and vice versa
#---- the other option would be to get a zero value which genertaed different mean and max values in the summary data


collared_animals <- collared_animals %>%
  dplyr::mutate(dist_frm_VF_inside_inclusion = case_when(VF_EX == "inside_VF" ~ dist_to_VF,
                                                         TRUE ~ NA_real_))

collared_animals <- collared_animals %>%
  dplyr::mutate(dist_frm_VF_outside_inclusion = case_when(VF_EX == "outside_VF" ~ dist_to_VF,
                                                         TRUE ~ NA_real_))



#----summaries the distance to VF variable
str(collared_animals)


dist_frm_VF_summary <- collared_animals %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(
            mean_dist_frm_VF_inside_inclusion =mean(dist_frm_VF_inside_inclusion, na.rm=TRUE),
            max_dist_frm_VF_inside_inclusion =max(dist_frm_VF_inside_inclusion, na.rm=TRUE),
            
            mean_dist_frm_VF_outside_inclusion =mean(dist_frm_VF_outside_inclusion, na.rm=TRUE), #this will give you a warning message beacuse of the heaps of zero values
            max_dist_frm_VF_outside_inclusion =max(dist_frm_VF_outside_inclusion, na.rm=TRUE)
            )

            
dist_frm_VF_summary <- as.data.frame(dist_frm_VF_summary)
dist_frm_VF_summary[ is.na(dist_frm_VF_summary) ] <- NA

str(dist_frm_VF_summary)

dist_frm_VF_summary <- dist_frm_VF_summary %>% 
  dplyr::mutate(max_dist_frm_VF_outside_inclusion =
                  case_when(max_dist_frm_VF_outside_inclusion > 0 ~ max_dist_frm_VF_outside_inclusion,
                TRUE   ~ NA_real_ ))

###--- join distance to VF variables to the RF df

RF_df <- left_join(RF_df, dist_frm_VF_summary)

rm(dist_frm_VF_summary)

#####################################################################################
### what is the total distance travelled for the length of the trial?
### could do average dist day 1 and 2
######################################################################################
str(collared_animals)

dist_taken_summary <- collared_animals %>%  group_by(sheep, behaviour_stage) %>% 
  summarise(total_dist_travel  =sum(step, na.rm=TRUE))

dist_taken_summary <- left_join(dist_taken_summary, hours_behav)
dist_taken_summary <- dist_taken_summary %>% 
  mutate(dist_travel_ratio = total_dist_travel/ mins)
dist_taken_summary

temp <- collared_animals %>% dplyr::select(behaviour_stage, compliance_score, sheep)
temp <- temp %>% distinct(behaviour_stage, compliance_score, sheep)
dist_taken_summary <- left_join(dist_taken_summary, temp)

dist_taken_summary_2 <- dist_taken_summary %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_dist_ratio  =mean(dist_travel_ratio, na.rm=TRUE))

RF_df <- left_join(RF_df, dist_taken_summary_2)
rm(dist_taken_summary, dist_taken_summary_2,temp)





#####################################################################################
### what is the total number of cues audio and pulse and ratio for the length of the trial?
### could do average  day 1 and 2
######################################################################################

str(collared_animals)
unique(collared_animals$Cue)

collared_animals <- collared_animals %>% 
  mutate(audio = 
           case_when(Cue == "A" ~ 1,
                     TRUE ~ NA_real_
           ))

collared_animals <- collared_animals %>% 
  mutate(pulse = 
           case_when(Cue == "S" ~ 1,
                     TRUE ~ NA_real_
           ))
## summary per animals
cue_summary_total <- collared_animals %>%  group_by(sheep, behaviour_stage, compliance_score) %>% 
  summarise(total_audio  =sum(audio, na.rm=TRUE),
            total_pulse  =sum(pulse, na.rm=TRUE),
            total_ratio = total_audio/(total_pulse+total_audio)*100)
cue_summary_total


## add in the mins logged data
cue_summary_total <- left_join(cue_summary_total, hours_behav)

cue_summary_total <- cue_summary_total %>% 
  mutate(total_audio_per_logged = total_audio/ mins,
         total_pulse_per_logged = total_pulse/ mins,
         total_ratio_per_logged = ((total_ratio/ mins))
  )
cue_summary_total <- cue_summary_total %>% group_by(behaviour_stage, compliance_score) %>% 
  summarise(Mean_total_audio_per_logged = mean(total_audio_per_logged, na.rm = TRUE),
            Mean_total_pulse_per_logged = mean(total_pulse_per_logged, na.rm = TRUE),
            Mean_total_ratio_per_logged = mean(total_ratio_per_logged, na.rm = TRUE))
            

RF_df <- left_join(RF_df, cue_summary_total)


### not sure this is meaningful 
cue_summary <- collared_animals %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_audio  =mean(audio, na.rm=TRUE),
            mean_pulse  =mean(pulse, na.rm=TRUE),
            mean_ratio = mean_audio/(mean_pulse+mean_audio)*100)

cue_summary
cue_summary[ is.na(cue_summary) ] <- NA

RF_df <- left_join(RF_df, cue_summary)
rm(cue_summary)




#####################################################################################
### what is the proportion of activity spent resting lying grazing? for the length of the trial?

######################################################################################
str(collared_animals)

###make new clms for resting grazing and moving so it matches the other dataset

temp <- collared_animals %>% dplyr::select(ID, resting,standing , walking, running)

temp <- temp %>% mutate(sum_activity = resting+ standing+walking+running)
temp <- temp %>% mutate(grazing = standing)
temp <- temp %>% mutate(moving = walking+ running)
temp <- temp %>% dplyr::select(ID,resting,moving, grazing )

collared_animals <- collared_animals %>% dplyr::select(-resting,-standing , -walking, -running)
collared_animals <- left_join(collared_animals,temp)

rm(temp)

beha_summary_prop <- collared_animals %>%  
  #group_by(sheep, behaviour_stage) %>%  
  dplyr::mutate(
    prop_resting = resting*100,
    prop_grazing = grazing*100,
    prop_moving = moving*100    )

beha_summary <- beha_summary_prop %>%  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_resting   =mean(prop_resting,  na.rm=TRUE),
            mean_grazing  =mean(prop_grazing , na.rm=TRUE),
            mean_moving   =mean(prop_moving,  na.rm=TRUE)  
  )                  

beha_summary

RF_df <- left_join(RF_df, beha_summary)
rm(beha_summary, beha_summary_prop)



#####################################################################################
### add in the distance between animals
######################################################################################
dist_bewteen <- collared_animals %>% 
  group_by(behaviour_stage, compliance_score) %>% 
  summarise(mean_numb_sheep_close    =mean(numb_sheep_close ,  na.rm=TRUE))

dist_bewteen


RF_df <- left_join(RF_df, dist_bewteen)
rm(dist_bewteen)
######################################################################################


#keep sheep with collars only on the days that the trials were run for them
## we have a problem the data has a heap of zero some of these relate to when the trial was run and not run


str(RF_df)

## drop a few 
RF_df <- RF_df %>% 
  dplyr::select (-max_dist_frm_VF_inside_inclusion )







###################################################################################
###                 write out df ready for the next step                      ###
###################################################################################

output_path <- "W:/VF/Optimising_VF/Waikerie/data_prep/"  

write.csv(RF_df, 
          paste0(output_path,"/step9_RF_for_plots.csv"), 
          row.names=FALSE)
###################################################################################
### condense the data so it can be simply used in RF model - one row per sheep  ###
###################################################################################