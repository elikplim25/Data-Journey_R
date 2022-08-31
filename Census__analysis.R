###### Bible Data Stories ###################

library(tidyverse)
library(scales)
library(showtext)


### adding font and turn on showtext
font_add_google(name = "Ultra", family = "Ultra")
showtext_auto()

font <- "Ultra"

#### calculate break values for diverging bars####
breaks_values <- pretty(Cen3$Pop)

### Changing text in the "Censuses" column"
Cen3[Cen3 == "First_census"]<- "First Census"
Cen3[Cen3 == "Second_census"]<- "Second Census"


####Visualize the data (divergent bar graph) ####
ggplot(Cen3, 
       aes(x = factor(Tribes, 
                      level = c('Manasseh','Ephraim','Benjamin',
                                        'Asher','Gad', 'Naphtali',
                                        'Dan','Zebulun','Issachar',
                                        'Judah', 'Simeon','Reuben')), 
           y = Pop, 
           fill = Censuses)) +
  geom_col() +
  geom_text(aes(label = scales::comma(Popu)),
            position = position_stack(vjust = 0.5), 
            size = 5.5,
            color = "white",
            fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  scale_y_discrete(breaks = breaks_values,
                       labels = abs(breaks_values)) +
  labs(title = "God instructed Moses to conduct 2 Censuses",
       subtitle = "Moses counted male warriors in each of the 12 tribes of Israel who were 20+ years old",
       x = "Tribes",
       y = "Population",
       fill = NULL, 
       caption = "Data Source: Numbers 1 and 26") +
  theme_minimal() +
  scale_fill_manual(values = c("#EF5B0C", "#003865"))+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(family = font,size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 8))



### size of viz when exporting####
#width =930, height = 770

################## analyzing the difference in the censuses####

head(census)

Census_diff<-mutate(census, Difference = Second_census - First_census) %>%
  mutate(Diff_Perc = Difference/First_census*100)%>%
  mutate(Diff_rating = ifelse(Census_diff$Diff_Perc >0, "positive", "negative"))



#### changing decimal points in "Diff_Perc" column to 0"
is.num <- sapply(Census_diff, is.numeric)
Census_diff[is.num] <- lapply(Census_diff[is.num], round, 0)

#### adding the percentage sign to the "Diff_Perc" column###
Census_diff$Diff_Perc[] <- paste0(as.data.frame(Census_diff$Diff_Perc), '%')

#### calculate break values####
breaks_values2 <- pretty(Census_diff$Diff_Perc)

  

######## diverging bars with ggplot2#######################

  ggplot(Census_diff, aes(x = factor(Tribes, 
                                     level = c('Manasseh','Ephraim','Benjamin',
                                               'Asher','Gad', 'Naphtali',
                                               'Dan','Zebulun','Issachar',
                                               'Judah', 'Simeon','Reuben')),
                          Diff_Perc2, fill = Diff_rating))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()+
  geom_text(aes(label = scales::percent(Diff_Perc2)), hjust = -0.1,size = 5, colour = "black"
            ,face = "bold") +
  scale_fill_manual(values = c("gold", "firebrick"))+
  labs(title = "Percentage Change after the 2nd Censuses",
       subtitle = "%increase or %decrease of the number of warriors in each of the 12 tribes of Israel",
       x = "Tribes", 
       y = "Percentage Difference",
       caption = "Data Source: Numbers 1 and 26") + 
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=13, face = "bold")) + 
  theme(plot.title = element_text(family = font,size = 20, face = "bold"))+
  theme(plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 8))
 













