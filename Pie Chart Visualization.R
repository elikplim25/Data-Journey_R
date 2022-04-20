library(tidyverse)
library(patchwork)
library(scales)


GIZ_cons <- Consultants%>%
  gather(BISA, Sylvanus, key = "Consultants", value =  "Figures")

####Visualizing messages per year
Messages_per_year <- Consultants %>%
  filter(Details == "Number of Messages in 1 year") %>%
  gather(BISA, Sylvanus, key = "Consultants", value = "Numbers")

##Pie Chart

MY <- ggplot(Messages_per_year, aes(x="", y=Numbers, fill=Consultants)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(label = paste0(round(Numbers))), 
            position = position_stack(vjust = 0.5), colour = "white", size = 8) + 
  scale_fill_brewer(palette="Set1")  + 
  ggtitle("No. of Voice Messages in 1 year") + 
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 12, face = "bold"))

###Visualizing message per month

Messages_per_month <- Consultants %>%
  filter(Details == "Number of Messages per month") %>%
  gather(BISA, Sylvanus, key = "Consultants", value = "Numbers")

###Pie chart
MM <- ggplot(Messages_per_month, aes(x="", y=Numbers, fill=Consultants)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(label = paste0(round(Numbers))), 
            position = position_stack(vjust = 0.5), colour = "white", size = 8) + 
  scale_fill_brewer(palette="Set1")  + 
  ggtitle("No. of Voice Messages per month") + 
  theme(legend.text = element_text(size = 12, face = "bold")) + 
  theme(legend.title = element_text(size = 14, face = "bold"))+
  theme(plot.title = element_text(size = 12, face = "bold"))


### visualizing Unit cost
Unit_cost <- Consultants %>%
  filter(Details == "Unit Cost in Cedis") %>%
  gather(BISA, Sylvanus, key = "Consultants", value = "Numbers")

### Pie Chart

UC <- ggplot(Unit_cost, aes(x="", y=Numbers, fill=Consultants)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(label = paste0(round(Numbers))), 
            position = position_stack(vjust = 0.5), colour = "white", size = 5) + 
  scale_fill_brewer(palette="Set1")  + 
  ggtitle("Unit Cost in Cedis") + 
  theme(legend.position = "none")+
  theme(plot.title = element_text(size = 12, face = "bold"))
  

### visualizing Total cost
Total_cost <- Consultants %>%
  filter(Details == "Total Cost in Cedis") %>%
  gather(BISA, Sylvanus, key = "Consultants", value = "Numbers")

### Pie chart

TC <- ggplot(Total_cost, aes(x="", y=Numbers, fill=Consultants)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(label = paste0(round(Numbers))), 
            position = position_stack(vjust = 0.5), colour = "white", size = 5) + 
  scale_fill_brewer(palette="Set1")  + 
  ggtitle("Total Cost in Cedis") + 
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 12, face = "bold"))

### Patching Visualizations together with Patchwork

Patchwork <- MY + MM + UC + TC
Patchwork + 
  plot_annotation(title = "Comparing the Cost of Voice Messaging (Bisa / Sylvanus)")

