library(tidyverse)
library(ggthemes)

Names <- c("Joseph",
           "Abraham",
           "Jacob",
           "The_Israelites",
           "Jesus")
Number_of_Years <- c(20,
                     25,
                     14,
                     40,
                     30)
Bible_reference <- c("Genesis 37_Genesis 41",
                     "Genesis 12",
                     "Genesis 29", 
                     "Numbers 14:34",
                     "Luke 3:23")
Waiters <- data.frame(Names, Number_of_Years,Bible_reference)

Waiters %>%
  ggplot(aes(reorder(Names,Number_of_Years), Number_of_Years)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(title = "Number of years some Bible Characters waited on God", 
       x = "Names", 
       y = "Number of Years")+
  geom_text(aes(label = Number_of_Years), 
            vjust = 1, size = 7, colour = "white") +
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))
