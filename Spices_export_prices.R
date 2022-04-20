library(tidyverse)
library(scales)

head(Naijinger)
head(Sprice)

##################################################################
gingerA <- Naijinger%>%
  select(Year, Reporter,`Netweight (kg)`)

colnames(gingerA) <- c("Years", "Country", "Netweight_(kg)")

gingerA2 <- merge(gingerA, Sprice, all = TRUE)

colnames(gingerA2) <- c("Years", "Country", "Netweight", "Price")

gingerA2[gingerA2 == "United Arab Emirates"] <- "UAE"

gingerA3 <- na.omit(gingerA2)
##############################################################################

UAE_price<- Sprice %>% filter(Country == "UAE")
gingerA[gingerA == "United Arab Emirates"] <- "UAE"
UAE_export<- gingerA %>% filter(Country == "UAE")

## merge the data
UAE_merge<-merge(UAE_export, UAE_price, all = TRUE)

colnames(UAE_merge)<- c("Years", "Country", "Netweight", "Price")

gingerA4 <- merge(gingerA3, UAE_merge, all = TRUE)


  ggplot(gingerA4, aes(Netweight,Price, colour = Country))+
  geom_point(size = 3)+
  scale_y_continuous(labels = scales::dollar)+
  scale_x_continuous(labels = scales::comma)+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_colour_manual(values = c("red", "steelblue", "green4",
                                  "orange", "brown"))+
  theme_bw()+
    ggtitle("Price vs Netweight of Ginger (neither crushed nor ground)", 
            subtitle = "In Nigeria's top 5 partner Countries (2016 - 2020)")+
    labs(x = "Netweight(KG)")+
    theme(axis.text = element_text(size = 12, colour = "black", face = "bold"),
          axis.title = element_text(size = 15, face = "bold"), 
          legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=13, face = "bold")) + 
    theme(plot.title = element_text(size = 20, face = "bold"))+
    theme(plot.subtitle = element_text(size = 15, face = "bold"))+
    facet_wrap(~Years)

  
  