library(tidyverse)
library(lubridate)

s_store <- US_Superstore_data

dim(s_store)
summary(s_store)
head(s_store)

#Visualizing orders per date

Orders_year<- s_store %>% 
  count(`Order Date`) %>%
  separate(`Order Date`,  c("Year", "Month", "Days"), sep = "-") %>%
  count(Year) 

colnames(Orders_year) <- c("Year", "Number")
Orders_year %>%
  ggplot(aes(Year, Number))+
  geom_bar(stat = "identity", fill = "steelblue") + 
  ggtitle("Number of Orders from the Superstore per year") + 
  geom_text(aes(label = Number), vjust = 2, colour = "white", size = 7)+
  theme_bw()+ 
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))


## Visualizing orders per month

Orders_month<- s_store %>% 
  count(`Order Date`) %>%
  separate(`Order Date`,  c("Year", "Month", "Days"), sep = "-") %>%
  count(Month) %>%
  mutate(Months = c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", 
                "November", "December"))%>%
  select(!Month)

colnames(Orders_month) <- c("Number" , "Month")

Orders_month %>%
  mutate(Month = fct_reorder(Month,(Number))) %>%
  ggplot(aes(Month,Number))+
  geom_bar(stat = "identity", fill = "steelblue") + 
  ggtitle("Number of Orders from the Superstore per month") + 
  geom_text(aes(label = Number), hjust = 2, colour = "white", size = 7)+
  theme_bw()+ 
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  coord_flip()

## Visualizing order delays with lubridate

order_delays<-interval( s_store$`Order Date`, s_store$`Ship Date`)%/% days()
order_count <- as.data.frame(order_delays)
class(order_count)
dim(order_count)

order_count2<- order_count%>%
  count(order_delays)

colnames(order_count2) <- c("Days", "Number_of_Orders")
order_count3 <- order_count2 %>% mutate(Delivery_Day = c("Same Day", "One Day", "Two Days", "Three Days",
                                            "Four Days", "Five Days", "Six Days", 
                                            "Seven Days"))%>%
   select(!Days)

order_count3 %>%
mutate(Delivery_Day = fct_reorder(Delivery_Day,(Number_of_Orders))) %>%
ggplot(aes(Delivery_Day, Number_of_Orders))+
  geom_bar(stat = "identity", fill = "red4") + 
  coord_flip() + 
  ggtitle("Most frequent Delivery Days") + 
  geom_text(aes(label = Number_of_Orders), hjust = 2, colour = "white", size = 7)+
  theme_bw()+ 
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))+
  labs(x = "Days", y = "Number of Orders")
  

