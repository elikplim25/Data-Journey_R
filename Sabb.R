library(tidyverse)

Sabina <- Sabina1
summary(Sabina)

head(Sabina)

Sabina2 <- Sabina %>% 
  select(Community, `Advice 1`, `Advice 2`, `Advice 3`)

Sabina3<- Sabina2 %>% 
  gather(`Advice 1`,`Advice 2`,`Advice 3`, key = "Advice", value = "count")

colnames(Sabina3) <- c("Community", "Advice", "Number")

Sabina4<-Sabina3 %>% group_by(Community)%>%
  count(Number)

colnames(Sabina4) <- c("Community", "Advice", "Number")

ggplot(Sabina5, aes(Advice, Number, fill = Advice)) + 
  geom_bar(stat = "identity", show.legend = FALSE)+
  facet_wrap(~Community) + 
  coord_flip()

Sabina5<-na.omit(Sabina4)

dim(Sabina1)

### Visualizing

Akwadum <- Sabina5%>%
  filter(Community == "Akwadum")

Akwadum%>%
  mutate(Advice = fct_reorder(Advice,(Number))) %>%
  ggplot(aes(Advice, Number)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Number), hjust = 1.5, color = "white", size = 7) + 
  ggtitle("Akwadum") + 
  theme_bw()+ 
  coord_flip()+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))



Amantra <- Sabina5 %>%
  filter(Community == "Amantra")

Amantra%>%
  mutate(Advice = fct_reorder(Advice,(Number))) %>%
  ggplot(aes(Advice, Number)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Number), vjust = 1.5, color = "white", size = 7) + 
  ggtitle("Amantra") + 
  theme_bw()+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))


Krobo <- Sabina5 %>%
  filter(Community == "Krobo")

Krobo%>%
  mutate(Advice = fct_reorder(Advice,(Number))) %>%
  ggplot(aes(Advice, Number)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Number), vjust = 1.5, color = "white", size = 7) + 
  ggtitle("Krobo") + 
  theme_bw()+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))


Kyiremfaso <- Sabina5 %>%
  filter(Community == "Kyiremfaso")

Kyiremfaso%>%
  mutate(Advice = fct_reorder(Advice,(Number))) %>%
  ggplot(aes(Advice, Number)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Number), vjust = 1.5, color = "white", size = 7) + 
  ggtitle("Kyiremfaso") + 
  theme_bw()+
  theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
        axis.title = element_text(size = 15, face = "bold")) + 
  theme(plot.title = element_text(size = 20, face = "bold"))
