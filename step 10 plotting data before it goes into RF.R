##Plots of what the RF is using.


library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
# library(ggexport)
# install.packages("ggexport")

Summary_df_for_plots <- read_csv("W:/VF/Optimising_VF/Waikerie/data_prep/step9_RF_for_plots.csv")



names(Summary_df_for_plots)
################################################################################
######## PLOTS
################################################################################

Mean_VF_graz <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_dist_frm_VF_inside_inclusion, 
                                                 fill= compliance_score 
                                          )) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  # geom_errorbar(aes(ymin=mean_dist_frm_VF_inside_inclusion-SD_dist_frm_VF_inside_inclusion, 
  #                   ymax=mean_dist_frm_VF_inside_inclusion+SD_dist_frm_VF_inside_inclusion), width=.2,
  #               position=position_dodge(.9))+
  labs(title="Mean Distance \nfrom VF when in grazing zone", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))
   

# Max_VF_graz <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=max_dist_frm_VF_inside_inclusion, 
#                                                 fill= compliance_score 
# )) + 
#   geom_bar(stat= "identity", position=position_dodge())+
#   theme(legend.position="none")+
#   labs(title="Max Distance \nfrom VF when in grazing zone", 
#        x="", 
#        y = "Max")+
#   scale_fill_manual(values=c("#999999",  "#56B4E9"))
# 
Mean_VF_exl <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_dist_frm_VF_outside_inclusion,
                                                fill= compliance_score )) +
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  # geom_errorbar(aes(ymin=mean_dist_frm_VF_outside_inclusion-SD_dist_frm_VF_outside_inclusion,
  #                   ymax=mean_dist_frm_VF_outside_inclusion+SD_dist_frm_VF_outside_inclusion), width=.2,
  #               position=position_dodge(.9))+
  labs(title="Mean Distance \nfrom VF when in exclusion zone",
       x="",
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))


Max_VF_ex <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=max_dist_frm_VF_outside_inclusion, 
                                              fill= compliance_score )) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  labs(title="Max Distance \nfrom VF when in exclusion zone", 
       x="", 
       y = "Max")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))


mean_dist_travel <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_dist_ratio, 
                                                     fill= compliance_score 
)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  # geom_errorbar(aes(ymin=mean_dist_ratio-SD_dist_frm_VF_outside_inclusion, 
  #                   ymax=mean_dist_ratio+SD_dist_frm_VF_outside_inclusion), width=.2,
  #               position=position_dodge(.9))+
  labs(title="Mean travelled ", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "#56B4E9"))



# distance_plots <- ggarrange(Mean_VF_graz, Max_VF_graz, 
#           Mean_VF_exl , Max_VF_ex ,mean_dist_travel,
#           
#           labels = c("A", "B", "C", "D"),
#           ncol = 2, nrow = 3)

distance_plots <- ggarrange(Mean_VF_graz,Mean_VF_exl , 
                            Max_VF_ex ,mean_dist_travel,
                            
                            labels = c("A", "B", "C", "D"),
                            ncol = 2, nrow = 2)

distance_plots
################################################################################
## Plots for behaviour 
################################################################################
str(Summary_df_for_plots)

Mean_resting <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_resting, 
                                                 fill = compliance_score)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  ylim(0, 100)+
  theme(legend.position="none")+
  labs(title="Mean proportion \nof time spent resting", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "darkseagreen"))

Mean_grazing <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_grazing, 
                                                 fill = compliance_score)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  ylim(0, 100)+
  theme(legend.position="none")+
  labs(title="Mean proportion \nof time spent grazing", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "darkseagreen"))

Mean_moving <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_moving, 
                                                 fill = compliance_score)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  ylim(0, 100)+
  theme(legend.position="none")+
  labs(title="Mean proportion \nof time spent moving", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "darkseagreen"))

behaviour_plots <- ggarrange(Mean_resting, Mean_grazing, 
                             Mean_moving , 
                            
                            labels = c("A", "B", "C"),
                            ncol = 2, nrow = 2)

behaviour_plots
################################################################################
## Plots for cues 
################################################################################
str(Summary_df_for_plots)

Mean_audio <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y= Mean_total_audio_per_logged, 
                                               fill = compliance_score)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  labs(title="Mean audio cues", 
       x="", 
       y = "mean")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

Mean_pulse <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_pulse_per_logged, 
                                               fill = compliance_score)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  labs(title="Mean pulse cues", 
       x="", 
       y = "mean")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

Mean_ratio <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_ratio_per_logged, 
                                               fill = compliance_score)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  theme(legend.position="none")+
  labs(title="Mean of ratio", 
       x="", 
       y = "mean")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_manual(values=c("#999999",  "cornflowerblue"))

# Mean_total_audio_logged <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_audio_per_logged, 
#                                                             fill = compliance_score)) + 
#   geom_bar(stat= "identity", position=position_dodge())+
#   theme(legend.position="none")+
#   labs(title="Mean audio \n(per logged point)", 
#        x="", 
#        y = "mean")+
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
#   scale_fill_manual(values=c("#999999",  "cornflowerblue"))
# 
# Mean_total_pulse_logged <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_pulse_per_logged, 
#                                                             fill = compliance_score)) + 
#   geom_bar(stat= "identity", position=position_dodge())+
#   theme(legend.position="none")+
#   labs(title="Mean pulse \n(per logged point)", 
#        x="", 
#        y = "mean")+
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
#   scale_fill_manual(values=c("#999999",  "cornflowerblue"))
# 
# Mean_total_ratio_logged <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=Mean_total_ratio_per_logged, 
#                                                             fill = compliance_score)) + 
#   geom_bar(stat= "identity", position=position_dodge())+
#   theme(legend.position="none")+
#   labs(title="Mean ratio \n(per logged point)", 
#        x="", 
#        y = "mean")+
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
#   scale_fill_manual(values=c("#999999",  "cornflowerblue"))

cue_plots <- ggarrange(Mean_audio, Mean_pulse,Mean_ratio, 
                             labels = c("A", "B", "C"),
                             ncol = 2, nrow = 2)

cue_plots
################################################################################
## Plots for animals close 
################################################################################
str(Summary_df_for_plots)
Mean_animals_close <- ggplot(Summary_df_for_plots, aes(x=behaviour_stage, y=mean_numb_sheep_close, 
                                                       fill = compliance_score)) + 
  geom_bar(stat= "identity", position=position_dodge())+
  #theme(legend.position="none")+
  labs(title="Mean number \nof sheep that are close", 
       x="", 
       y = "mean")+
  scale_fill_manual(values=c("#999999",  "blue"))


################################################################################
distance_plots
behaviour_plots
cue_plots

Mean_animals_close

getwd()

ggexport(distance_plots, filename = "W:/VF/Optimising_VF/Waikerie/plots_pre_RF/test_distance_plots.png")
ggexport(behaviour_plots, filename = "W:/VF/Optimising_VF/Waikerie/plots_pre_RF/test_behaviour_plots.png")
ggexport(cue_plots, filename = "W:/VF/Optimising_VF/Waikerie/plots_pre_RF/test_cue_plots.png")
ggexport(Mean_animals_close, filename = "W:/VF/Optimising_VF/Waikerie/plots_pre_RF/test_Mean_animals_close.png")

