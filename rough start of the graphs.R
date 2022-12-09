## scrap paper

str(percent100)

# change the order 

percent100$herd_postion <- factor(percent100$herd_postion, levels = c("leader", "herd", "follower"))


percent100 %>% 
#mutate( type=ifelse(herd_postion=="leader")) %>%  
#ggplot(aes(x=herd_postion , y=dist_to_VF, color=herd_postion, fill=type)) +
ggplot(aes(x=herd_postion , y=dist_to_VF, color=herd_postion)) +
  geom_boxplot(fill= NA, alpha=0.2)+
  geom_point()+ 
  facet_wrap(.~date)+
  theme_bw()+
  labs(title = "Treatmnet 100%",
       y=  "Distance from the VF",
       x = "")


percent100 %>% 
  ggplot(aes(x=herd_postion , y=step, color=herd_postion)) +
  geom_boxplot(fill= NA, alpha=0.2)+
  geom_point()+ 
  facet_wrap(.~date)+
  theme_bw()+
  labs(title = "Treatmnet 100%",
       y=  "distance between last logged point",
       x = "")


percent100 %>% 
  ggplot(aes(x=herd_postion , y=running, color=herd_postion)) +
  geom_boxplot(fill= NA, alpha=0.2)+
  geom_point()+ 
  facet_wrap(.~date)+
  theme_bw()+
  labs(title = "Treatmnet 100%",
       y=  "time spent resting",
       x = "")



names(percent100)

percent100_count <- percent100 %>% group_by(herd_postion, date, VF_EX) %>% 
  summarise(count = n())
  
percent100_count %>% 
  ggplot(aes(x=herd_postion , y=count, group)) +
  geom_col()+
  facet_wrap(.~date)+
  theme_bw()+
  labs(title = "Treatmnet 100%",
       y=  "Distance from the VF",
       x = "")
