library(tidyverse)
library(scales)
library(showtext)

##### Exploring the data and reshaping it for further use ###########

Census<-tibble(Tribes = c("Asher","Benjamin", "Dan", "Ephraim", "Gad","Issachar","Judah",  
                  "Manasseh","Naphtali","Reuben","Simeon","Zebulun"),
       First_Census = c(41500,35400, 62700,40500,45650,
                        54400, 74600, 32200, 53400, 46500,
                        59300,57400),
       Second_Census = c(53400, 45600, 64400,32500, 40500,
                         64300, 76500, 52700, 45400, 43730 ,
                         22200, 60500))

head(Census)


### Preparing data for divergent bars ####
Cen3<-Census %>% 
  gather("First_Census", 
         "Second_Census", 
         key = "Censuses", 
         value = "Popu") %>%
  mutate(Pop = Popu) %>%
  mutate(Pop = ifelse(Censuses == "Second_Census",
                      Pop,
                      -1*Pop))

head(Cen3)


### adding font and turn on showtext
font_add_google(name = "Ultra", family = "Ultra")
showtext_auto()

font <- "Ultra"

#### calculate break values for diverging bars####
breaks_values <- pretty(Cen3$Pop)

### Changing text in the "Censuses" column"
Cen3[Cen3 == "First_Census"]<- "First Census"
Cen3[Cen3 == "Second_Census"]<- "Second Census"


####Visualize the data (divergent bar graph) First census vs sec. census######
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

##### data wrangling in preparation for visualization ### 

head(Census)

Census_diff<-mutate(Census, Difference = Second_Census - First_Census) %>%
  mutate(Diff_Perc = Difference/First_Census*100)%>%
  mutate(Diff_rating = ifelse(Census_diff$Diff_Perc >0, "positive", "negative"))%>%
  mutate(Diff_Perc2 = Diff_Perc/100)



#### changing decimal points in "Diff_Perc" column to 2"
Census_diff$Diff_Perc2<-round(Census_diff$Diff_Perc2 ,digit=2)

#### calculate break values####
breaks_values2 <- pretty(Census_diff$Diff_Perc)

  

######## diverging bars with ggplot2 (Percentage change)#######################

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








         