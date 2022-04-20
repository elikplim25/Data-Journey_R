library(tidyverse)



Miracles3<-Miracles%>%
  gather( Matthew, Mark, Luke,   John, 
         key = "Gospels", value = "Verses" )%>%
  filter(Verses !="NA")

########unique miracles

Miracles4<-Miracles3 %>% 
  count(Miracle)%>%
  filter(n==1)%>%
  mutate("Reference" = paste0(c("Luke", "Luke", "Mark", "Mark", "John", "Matthew", "Luke", 
                               "Luke", "Luke", "John", "John", "Matthew", "Luke", "John", 
                               "John","Matthew" ,"John")))%>%
  count(Reference)

Miracles_unique2 <- tibble(
  Gospel = c("Matthew", "Mark", "Luke", "John"),
  Number = c(3, 2,6,6)
)



Miracles5<-Miracles3 %>% 
  count(Miracle)%>%
 # filter(n==1)%>%
  mutate("Reference" = paste0(c("John",
                                "John",
                                "Mark_Luke",
                                "Matthew_Mark_Luke",
                                "Matthew_Mark_Luke",
                                "Luke",
                                "Matthew_Mark_Luke",
                                "Matthew_Luke",
                                "Matthew_Mark_Luke",
                                "Matthew_Mark_Luke",
                                "Luke",
                                "Matthew_Mark_Luke",
                                "Matthew_Mark_Luke",
                                "Matthew_Mark_Luke",
                                "Matthew_Mark_Luke",
                                "Matthew",
                                "Matthew",
                                "John",
                                "Matthew_Mark_John_Luke",
                                "Matthew_Mark_John",
                                "Matthew_Mark",
                                "Matthew_Mark",
                                "Mark",
                                "Matthew_Mark",
                                "Mark",
                                "John",
                                "Matthew_Mark_Luke",
                                "Matthew",
                                "Luke",
                                "Luke",
                                "Luke",
                                "Luke",
                                "John",
                                "Matthew_Mark_Luke",
                                "Matthew_Mark",
                                "Luke",
                                "John"
  )))%>%mutate("Kind_of_Miracle" = paste0(c("Miraculous act",
                                            "Healing",
                                            "Casting out demons",
                                            "Healing",
                                            "Healing",
                                            "Miraculous act",
                                            "Healing",
                                            "Healing",
                                            "Healing",
                                            "Healing",
                                            "Raising the dead",
                                            "Miraculous act",
                                            "Casting out demons",
                                            "Healing",
                                            "Raising the dead",
                                            "Healing",
                                            "Healing",
                                            "Healing",
                                            "Feeding thousands",
                                            "Miraculous act",
                                            "Healing",
                                            "Healing",
                                            "Healing",
                                            "Feeding thousands",
                                            "Healing",
                                            "Healing",
                                            "Healing",
                                            "Miraculous act",
                                            "Healing",
                                            "Healing",
                                            "Healing",
                                            "Healing",
                                            "Raising the dead",
                                            "Healing",
                                            "Miraculous act",
                                            "Healing",
                                            "Miraculous act")))

###########################################################

Matt_healing<-miracles_revised%>%
  filter(Kind_of_miracle == "Healing")%>%
  filter(grepl("Matthew", Gospels))%>%
  count()
Mark_MA <- miracles_revised%>%
  filter(Kind_of_miracle == "Miraculous Act")%>%
  filter(grepl("Mark", Gospels))%>%
  count()
Luke_MA <- miracles_revised%>%
  filter(Kind_of_miracle == "Miraculous Act")%>%
  filter(grepl("Luke", Gospels))%>%
  count()
John_MA <- miracles_revised%>%
  filter(Kind_of_miracle == "Miraculous Act")%>%
  filter(grepl("John", Gospels))%>%
  count()
## Healing: Matt = 13, Mark = 12, Luke = 14, John =13
###### Miraculous Acts: Matthew = 4, Mark = 3, Luke = 2, John = 3
###casting out demons: Matthew = 1, Mark =1, Luke = 2, John = 0
### Raising the dead: Matthew = 1, Mark = 1, Luke = 2, John = 1
#### Feeding thousands: Matthew = 2, Mark = 2, Luke = 1, John = 1


kINDS_PER_GOSPELS<- tibble(
  Gospels1 = c("Matthew", "Mark", "Luke", "John"), 
  Healing = c(13, 12, 14, 13), 
  Gospels2 = c("Matthew", "Mark", "Luke", "John"),
  Miraculous_Act = c(4,3,2,3), 
  Gospels3 = c("Matthew", "Mark", "Luke", "John"),
  Casting_out_demons = c(1,1,2,0), 
  Gospels4 = c("Matthew", "Mark", "Luke", "John"),
  Raising_the_dead = c(1,1,2,1), 
  Gospels5 = c("Matthew", "Mark", "Luke", "John"),
  Feeding_thousands = c(2,2,1,1)
)

kINDS_PER_GOSPELS2<-kINDS_PER_GOSPELS %>%
  gather(Healing, Miraculous_Act, Casting_out_demons, 
         Raising_the_dead,Feeding_thousands, key = "Miracle", value = "Number")%>%
  select(Gospels1, Miracle, Number)

colnames(kINDS_PER_GOSPELS2) <- c("Gospels", "Miracle", "Number")

ggplot(kINDS_PER_GOSPELS2, aes(Gospels, Number, fill = Miracle))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = Number), vjust = -0.2, position = position_dodge(0.9))+
  facet_wrap(~Miracle)

