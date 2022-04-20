library(tidyverse)
library(ggrepel)

head(GIZ_Farmers)

GIZ_Reg<-aggregate(Farmers~Region, GIZ_Farmers, sum)
GIZ_Reg %>%
  mutate(Region = fct_reorder(Region,(Farmers))) %>%
  ggplot(aes(Region, Farmers)) + 
  geom_bar(stat = "identity", fill = "green4") + 
  geom_text(aes(label = Farmers), vjust = 2, color = "white", size = 7) + 
  ggtitle("GIZ COVID-19 Response Project_Number of Farmers per Region")+
  theme_bw()+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))



GIZ_gather <- GIZ_Farmers %>%
  gather(Societies, Farmers, key = "Indicators", value = "Numbers")


GIZ_soc<- GIZ_gather %>% filter(Indicators == "Societies")

###############Visualizing the number of societies per district
   GIZ_soc%>%
     mutate(District = fct_reorder(District,(Numbers))) %>%
  ggplot(aes(District, Numbers, fill = Region)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+ 
  facet_grid(~Indicators)+
     ggtitle("GIZ COVID-19 Response Project_Number of Societies per District")+
     theme_bw()+
     theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
           axis.title = element_text(size = 15, face = "bold"),
                                     legend.position = "bottom",
                                     legend.title = element_blank(),
                        legend.text=element_text(size=13, face = "bold")) + 
     theme(plot.title = element_text(size = 20, face = "bold")) + 
     geom_text(aes(label = Numbers), hjust = 2, color = "white", size = 7)+
     scale_fill_manual(values = c("green4", "red", "gold4", "steelblue", "darkred"))

##### Visualizing the number of farmers per district
   GIZ_farm%>%
     mutate(District = fct_reorder(District,(Numbers))) %>%
     ggplot(aes(District, Numbers, fill = Region)) + 
     geom_bar(stat = "identity") + 
     coord_flip()+ 
     facet_grid(~Indicators)+
     ggtitle("GIZ COVID-19 Response Project_Number of Farmers per District")+
     theme_bw()+
     theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
           axis.title = element_text(size = 15, face = "bold"),
           legend.position = "bottom",
           legend.title = element_blank(),
           legend.text=element_text(size=13, face = "bold")) + 
     theme(plot.title = element_text(size = 20, face = "bold")) + 
     geom_text(aes(label = Numbers), hjust = 2, color = "white", size = 7)+
     scale_fill_manual(values = c("green4", "red", "gold4", "steelblue", "darkred"))   
   
   
   
   GIZ_farm <- GIZ_gather %>% filter(Indicators == "Farmers")
   
   
   #### scatter plot#############
   
   GIZ_Farmers %>%
     ggplot(aes(Societies, Farmers, colour = Region))+ 
     geom_point(size = 5) +
     scale_color_manual(values = c("green4", "red", "gold4", "steelblue", "darkred"))+ 
     theme_bw() + 
     theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
           axis.title = element_text(size = 15, face = "bold"),
           legend.position = "bottom",
           legend.title = element_blank(),
           legend.text=element_text(size=13, face = "bold")) + 
     theme(plot.title = element_text(size = 20, face = "bold")) +
     geom_text_repel(aes(label = District), size = 4, face = "bold") + 
     labs(title = "Distribution of farmers in each District_ GIZ COVID 19 Project", 
          x = "Districts")
     
     

