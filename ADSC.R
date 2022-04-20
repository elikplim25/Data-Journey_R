library(tidyverse)

##### cursory view of the data ##########
dim(ADSC21)
summary(ADSC21)
head(ADSC21)

#####Visualizing the data -- bar graph ########
ggplot(ADSC21, aes(Month, `Number of trainees`, fill = `Digital Skills`)) +
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Number of Trainees per Month")+
  scale_fill_manual(values = c("steelblue", "red", "green4", "grey"))+
  theme_bw()+ 
  geom_text(aes(label = `Number of trainees`), vjust = 2, size = 5,
            position = position_dodge(0.9))+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))

 
#### Visualize targets and delays#####

ADSC21.1 <- ADSC21 %>%
  select(Month, `Scheduled Dates`, `Delayed Dates`, `Digital Skills`) %>%
  gather(`Scheduled Dates`, `Delayed Dates`, key = "Dates", value = "Days")

  ggplot(ADSC21.1 ,aes(`Digital Skills`, Days, colour = Dates))+
  geom_point(size = 6) + 
    geom_line(aes(group = `Digital Skills` ), colour = "grey", size = 2)+
    facet_wrap(~Month)+
    coord_flip()+
    theme_bw()+
    ggtitle("Scheduled Dates vs Delayed Dates for Digital Skills Training Project")+
    theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
          axis.title = element_text(size = 15, face = "bold"),
          legend.title = element_blank(),
          legend.position = "right",
          legend.text=element_text(size=13, face = "bold")) + 
    theme(plot.title = element_text(size = 20, face = "bold"))+
    scale_color_manual(values = c("red", "green4"))+
    geom_text(aes(label = Days), vjust = -1, face = "bold", size = 5)
  

