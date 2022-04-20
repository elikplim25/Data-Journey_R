library(tidyverse)
library(ggrepel)

head(Wpackages)

####### Visualizing work package one ##########

WP1<- Wpackages %>%
  filter(`Work Package` == "WP1")

WP11<-WP1 %>%
  gather(`Original Deadline`, 
         `Delayed Deadline`, key = "Deadlines", value = "Months")

###Ordering the x axis
WP11$Activities <- factor(WP11$Activities,
                          levels = c("Delivery of messages","Delivery of safety equipment",
                                     "Initial deployment of messages","Remote delivery channels set up",
                                     "Recording messages", "Content Development", 
                                     "Content Development workshop","Community Sensitization", "Farmer organization",
                                     "Project operational and org. planning", "PM set-up and kick-off workshop"))

### reordering the legend
WP11$Deadlines <- factor(WP11$Deadlines, levels = c("Original Deadline", "Delayed Deadline"))


ggplot(WP11, aes(Activities, Months))+
  geom_point(aes(color = Deadlines), size = 9) + 
  coord_flip() +
  theme_bw()+
  geom_line(aes(group = Activities), color = "grey", size = 2) +
   labs(title = "Original Deadlines vs Delayed Deadlines of GIZ COVID Project Activities",
        subtitle = "Work Package 1")+
  scale_color_manual(values = c("green4","red3"))+
  geom_text_repel(aes(label = Months), size = 5)+
  theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size =18, face = "bold"),
        plot.subtitle = element_text(size =15, face = "bold"))

  ##geom_text(aes(label = Months), hjust = -1, position = position_dodge(0.9))


####### Visualizing work package TWO ##########

WP2<- Wpackages %>%
  filter(`Work Package` == "WP2")

WP12<-WP2 %>%
  gather(`Original Deadline`, 
         `Delayed Deadline`, key = "Deadlines", value = "Months")

###Ordering the x axis
WP12$Activities <- factor(WP12$Activities,
                          levels = c("Field extension services", "Deployment of incentives",
                                     "TOT sessions","Design of incentive mechanism"))

### reordering the legend
WP12$Deadlines <- factor(WP12$Deadlines, levels = c("Original Deadline", "Delayed Deadline"))


ggplot(WP12, aes(Activities, Months))+
  geom_point(aes(color = Deadlines), size = 9) + 
  coord_flip() +
  theme_bw()+
  geom_line(aes(group = Activities), color = "grey", size = 2) +
  labs(title = "Original Deadlines vs Delayed Deadlines of GIZ COVID Project Activities",
       subtitle = "Work Package 2")+
  scale_color_manual(values = c("green4","red3"))+
  theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size =18, face = "bold"),
        plot.subtitle = element_text(size =15, face = "bold"))




####### Visualizing work package three ##########

WP3<- Wpackages %>%
  filter(`Work Package` == "WP3")

WP13<-WP3 %>%
  gather(`Original Deadline`, 
         `Delayed Deadline`, key = "Deadlines", value = "Months")

###Ordering the x axis
WP13$Activities <- factor(WP13$Activities,
                          levels = c("Knowledge exchange session",  "Refresher training",  "M&E of adoption practices",
                                     "TOT on monitoring SOP",   "Development of monitoring SOP"))

### reordering the legend
WP13$Deadlines <- factor(WP13$Deadlines, levels = c("Original Deadline", "Delayed Deadline"))


ggplot(WP13, aes(Activities, Months))+
  geom_point(aes(color = Deadlines), size = 9) + 
  coord_flip() +
  theme_bw()+
  geom_line(aes(group = Activities), color = "grey", size = 2) +
  labs(title = "Original Deadlines vs Delayed Deadlines of GIZ COVID Project Activities",
       subtitle = "Work Package 3")+
  scale_color_manual(values = c("green4","red3"))+
  theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
        axis.title = element_text(size = 14, face = "bold", colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size =18, face = "bold"),
        plot.subtitle = element_text(size =15, face = "bold"))

  