library(tidyverse)
library(scales)

##################################Fresh ginger analysis
world_ginger<- Fresh_ginger%>%
  filter(Partner == "World")
  
  
world_ginger%>%
  ggplot(aes(Year, `Netweight (kg)`, colour = Reporter))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_text(aes(label = `Netweight (kg)`), vjust = -0.5, size = 3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::comma)+
  ggtitle("Net weight of ginger (neither crushed nor ground) importations ", 
          subtitle = "by Nigeria's top 5 partners (2016-2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 14, face = "bold"))


###########################
Naijinger<- Fresh_ginger%>%
  filter(Partner == "Nigeria")

Naijinger%>%
  ggplot(aes(Year, `Netweight (kg)`, colour = Reporter))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_text(aes(label = `Netweight (kg)`), vjust = -0.5, size = 3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::comma)+
  ggtitle("Net weight of ginger (neither crushed nor ground) importations from Nigeria ", 
          subtitle = "by Nigeria's top 5 partners (2016-2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 17, face = "bold"))+
  theme(plot.subtitle = element_text(size = 14, face = "bold"))


###########################Processed Ginger analysis

#### world 

proworld<- Processed_Ginger%>%
  filter(Partner == "World")

proworld%>%
  ggplot(aes(Year, `Netweight (kg)`, colour = Reporter))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_text(aes(label = `Netweight (kg)`), vjust = -0.5, size = 3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::comma)+
  ggtitle("Net weight of powdered ginger importations ", 
          subtitle = "by Nigeria's top 5 partners (2016-2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 14, face = "bold"))

######Nigeria

Naijapro<- Processed_Ginger%>%
  filter(Partner == "Nigeria")

Naijapro%>%
  ggplot(aes(Year, `Netweight (kg)`, colour = Reporter))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_text(aes(label = `Netweight (kg)`), vjust = -0.5, size = 3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::comma)+
  ggtitle("Net weight of powdered ginger importations from Nigeria ", 
          subtitle = "by Nigeria's top 5 partners (2016-2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 14, face = "bold"))


########################### Turmeric

Turmworld<- Turmeric %>%
  filter(Partner == "World")%>%
  select(-`Trade Flow`, -Commodity)

#### world 

Turmworld%>%
  ggplot(aes(Year, `Netweight (kg)`, colour = Reporter))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_text(aes(label = `Netweight (kg)`), vjust = -0.5, size = 3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::comma)+
  ggtitle("Net weight of Turmeric importations ", 
          subtitle = "by Nigeria's top 4 partners (2016-2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 14, face = "bold"))


####Nigeria

Naijaturm<- Turmeric%>%
  filter(Partner == "Nigeria")%>%
  select(-`Trade Flow`, -Commodity)

Naijaturm%>%
  ggplot(aes(Year, `Netweight (kg)`, colour = Reporter))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  #geom_text(aes(label = `Netweight (kg)`), vjust = -0.5, size = 3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::comma)+
  ggtitle("Net weight of turmeric importations from Nigeria ", 
          subtitle = "by Nigeria's top 4 partners (2016-2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 14, face = "bold"))

############################### Farmer Economics#################33

##Cost

Farm_econ <- tibble(
  Crops = c("Ginger", "Turmeric", "Chili"),
  Production_Cost = c(801260, 911260, 1517260)
)

Farm_econ %>% 
  ggplot(aes(Crops, Production_Cost, fill = Crops)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("red2", "tan1", "orange"))+
  scale_y_continuous(labels =  comma_format(accuracy=1))+
  ggtitle("Farmer Economics", 
          subtitle =  "Cost of producing ginger, turmeric and chili in Nigeria (In Naira)") +
  theme_bw()+ 
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "null",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))+
  geom_text(aes(label = comma(Production_Cost)), vjust = 2, colour = "black",
            size = 7)


########Income#####

farmecon1 %>%
  ggplot(aes(Crop, Amount, fill = Crop))+
  geom_bar(stat = "identity")+
  ggtitle("Farmer Economics",
          subtitle = " Revenue - Cost = Farmer Income (In Naira)")+
  scale_y_continuous(labels = scales::comma)+
  geom_text(aes(label= comma(Amount)), vjust = -0.2, colour = "black", size = 5,
            position = position_dodge(0.9)) +
  facet_wrap(~State)+
  scale_fill_manual(values = c("red2", "tan1", "orange"))+
  theme_bw()+ 
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"), 
        legend.position = "null") + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))



###################################################################################

######PRICES#######

Sprice<- prices_ %>% gather(`2016`:`2020`, key = "Years", value = "Price/Kg($)")

##convert the Years column to numeric to make it possible to create a line graph
Sprice$Years = as.numeric(as.character(Sprice$Years ))

##visualize
ggplot(Sprice, aes(Years, `Price/Kg($)`, colour = Country))+
  geom_line(size = 1.3)+
  geom_point(size =3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::dollar)+
  ggtitle("$Price/KG of Ginger (neither crushed nor ground) on the global market", 
          subtitle = "Nigeria's top 5 partners (2016 - 2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"), 
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))

#### Turmeric visualization 

Sprice2<- Prices3 %>% gather(`2016`:`2020`, key = "Years", value = "Price/Kg($)")

##convert the Years column to numeric to make it possible to create a line graph
Sprice2$Years = as.numeric(as.character(Sprice2$Years ))

##visualize
ggplot(Sprice2, aes(Years, `Price/Kg($)`, colour = Country))+
  geom_line(size = 1.3)+
  geom_point(size =3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::dollar)+
  ggtitle("$Price/KG of Turmeric on the global market", 
          subtitle = "Nigeria's top 4 partners (2016 - 2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"), 
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))


##################################
##Visualize powdered ginger

Sprice1<- Prices_1 %>% gather(`2016`:`2020`, key = "Years", value = "Price/Kg($)")

##convert the Years column to numeric to make it possible to create a line graph
Sprice1$Years = as.numeric(as.character(Sprice1$Years ))

##visualize
ggplot(Sprice1, aes(Years, `Price/Kg($)`, colour = Country))+
  geom_line(size = 1.3)+
  geom_point(size =3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4", "orange", "brown"))+
  scale_y_continuous(labels = scales::dollar)+
  ggtitle("$Price/KG of powdered ginger on the global market", 
          subtitle = "Nigeria's top 5 partners (2016 - 2020)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"), 
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))


######################################## Percentage of Partner import
head(Parcent)

colnames(Parcent) <- c("Country","Ginger(F/D)", "Powdered_Ginger", "Turmeric")
Parcent1 <- Parcent %>%
  gather(`Ginger(F/D)`, Powdered_Ginger, Turmeric,key = "Commodity", 
         value = "Percentage")%>%
  mutate(Parcent2 == "Percentage/1000")

p1<-ggplot(Parcent1, aes(Commodity, Percentage, fill = Country))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~Country)+
  scale_fill_manual(values = c("red", "steelblue", "green4",
                               "orange","midnightblue", "brown"))+
  theme_bw()+
  geom_text(aes(label = scales::percent(Percentage), 
                y = Percentage), vjust = -0.2, size = 5,colour = "black",
            position = position_dodge(0.9))+
  ggtitle("Percentage of Partner Import Supplied", 
          subtitle = " by Nigeria 2016-20")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"), 
        legend.position = "null",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))


ggsave("simple_barplot_with_ggplot2_with_percent_values_on_y_axis.png")  



ggplot(Parcent1, aes(Commodity, Percentage, fill = Country))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = scales::percent(Percentage), 
                y = Percentage), vjust = -0.2,
            size = 5,colour = "black",
            group = "Country",
            position = position_dodge(0.9))
