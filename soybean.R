library(tidyverse)
library(scales)

view(soybeans)

#Soya beans; whether or not broken = SoyA             
#Soya beans; other than seed, whether or not broken = SoyB

###################world########################################
soybeans[soybeans == "Soya beans, whether or not broken"] <- "SoyA"
soybeans[soybeans == "Soya beans; other than seed, whether or not broken"] <- "SoyB"

soybeans1 <- soybeans%>%
  select(Year, Partner, `Netweight (kg)`, Commodity,`Trade Value (US$)`)%>%
  filter(Partner == "World")

ggplot(soybeans1, aes(Year, `Netweight (kg)`, fill = Commodity))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = comma(`Netweight (kg)`)), size = 4, vjust = 2, 
            colour = "white", position = position_dodge(0.9))+
  theme_bw()+
  scale_fill_manual(values = c("navyblue", "red4"))+
  ggtitle("Net weight(kg) of Soybeans exportations", 
          subtitle = "From Ghana to the rest of the world (2016-2019)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"), 
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))


############Countries##############

soybeans2 <- soybeans%>%
  select(Year, Partner, `Netweight (kg)`, Commodity,`Trade Value (US$)`)%>%
  filter(Partner != "World")


soybeans3<-soybeans2 %>% 
  arrange(desc(`Netweight (kg)`))%>%
  filter(Partner %in% c("Turkey", "India", "United Arab Emirates",
                        "Viet Nam","Burkina Faso" ))


ggplot(soybeans3, aes(Year, `Netweight (kg)`, colour = Partner))+
  geom_line(size = 1.2)+
  geom_point(size = 3)+
  theme_bw()+
  scale_colour_manual(values = c("red", "steelblue", "green4",
                                 "orange", "brown"))+
  ggtitle("Net weight(kg) of Soybeans importations", 
          subtitle = "By top 5 partners of Ghana (2016-2019)")+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"), 
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 15, face = "bold"))+
  facet_wrap(~Commodity)



ggplot(soybeans3, aes(Year, `Netweight (kg)`, fill = Partner))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~Commodity)
