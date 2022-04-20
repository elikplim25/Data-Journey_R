library(tidyverse)
library(scales)

Item <- c("Food costs", "Housing costs",
          "Non-food non-housing costs",
          "Additional 5%" )
Ghana_Cedis <- c(757, 198,439,  70 )
US_Dollars <- c(170, 44, 99, 16)

Ecom_data <- data.frame(Item, Ghana_Cedis, US_Dollars)
Ecom_data

##reshaping the data 

Ecom_data2 <- Ecom_data %>%
  gather(Ghana_Cedis,US_Dollars, key = "Currency", value = "Amount")

## visualizing the data
Ecom_data2%>%
  ggplot(aes(reorder(Item, -Amount), Amount, fill = Currency )) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() +
  geom_text(aes(label = Amount), size = 5, vjust = -0.05, position = position_dodge(.9))+
  theme(axis.text=element_text(size=15, face = "bold", colour = "black"),axis.title=element_text(size=15,face="bold")) + 
  theme(plot.title = element_text(size = 15, face = "bold"))+
  guides(x =  guide_axis(angle = 60))+
  facet_wrap(~Currency) +
  scale_fill_manual(values = c("red", "steelblue"), guide = FALSE)+
  labs(title = "Breakdown of Living Income benchmark for a family of 2 adults and 3 children",
       subtitle = " in rural cocoa growing regions of Ghana (Ashanti, Central, Eastern and Western Regions)", x = "Item")


##second visualization  
Household_Type <- c("Male headed households", "Female headed households
", "Large male headed households")

Average_Income_per_Month <- c(848, 650, 1893)

KIT_data <- data.frame(Household_Type, Average_Income_per_Month )

ggplot(KIT_data, aes(reorder(Household_Type, Average_Income_per_Month), 
             Average_Income_per_Month)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  coord_flip()+ 
  theme_bw()+
  geom_text(aes(label = Average_Income_per_Month), 
            size = 7, hjust = 2, colour ="white")+
  labs(x = "Household Type", y = "Average Income per Month",
       title = "Average Income per Month for KIT Classification of Households (In Ghana Cedis)")+
  theme(axis.text=element_text(size=15, face = "bold", colour = "black"),axis.title=element_text(size=15,face="bold")) + 
  theme(plot.title = element_text(size = 15, face = "bold"))
  

### Creating percentages with the Scales package
ecom_perc<- Ecom_data %>% 
  mutate(Percentage = percent((Ghana_Cedis - US_Dollars)/Ghana_Cedis*100))
