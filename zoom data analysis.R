library(tidyverse)
library(reshape2)

head(zoom_data1)

MAW <- zoom_data1 %>%
  select(!Name) %>%
           select(!Email)

##renaming column names

colnames(MAW) <- c("Date",                                                         
                   "Goal clearly communicated",
                   "Duration Too Long",
                   "Facilitator Encouraged Participation",     
                   "Easy to Understand",                       
                  "Re-onboarding session is Excellent")

#Reshaping the data into a machine-readable format
MAW2 <- MAW %>%
  gather("Goal clearly communicated",
         "Duration Too Long",
         "Facilitator Encouraged Participation",     
         "Easy to Understand",                       
         "Re-onboarding session is Excellent", key = "Questions", value = "Responses")


## Visualizing the data for Feb 22

MAW3 <- MAW2 %>%
  group_by(Date,Questions) %>%
  count(Responses)

View(MAW3)

##Renaming column names for MAW3
colnames(MAW3) <- c("Date", "Questions", "Responses", "Number")

Maw3_22feb <- MAW3 %>%
  filter(Date < "2021-02-24")

ggplot(Maw3_22feb, aes(Questions, Number, fill = Responses))+
  geom_bar(stat = "identity") + 
  coord_flip() + 
  geom_text(aes(label = Number), colour = "white", hjust = 2, position = "stack", size = 7) + 
  ggtitle("Re-onboarding Poll Responses _Ahanta West Online Training") + 
  labs(y = "Number of Participants") + 
  theme(plot.title = element_text(size =25, face = "bold"))+
  theme_bw()+
  theme(axis.text=element_text(size=15, face = "bold", colour = "black"),axis.title=element_text(size=20,face="bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.text = element_text(size = 13,
                                   face = "bold"),
        legend.title = element_text(size = 14,
                                    face = "bold"))

## Visualizing the data for Feb 24 
Maw3_24feb <-zoom_data2

Maw3_24feb <- Maw3_24feb %>%
  select(!Name) %>%
  select(!Email)
colnames(Maw3_24feb) <- c( "Date",                                                                  
                           "Goal clearly communicated",
                           "Duration Too Long",                       
                           "Overall Ratinf of session is Excellent ",     
                           "Goal of Session was achieved",      
                           "Easy to Understand Facilitator",                      
                           "Excited to use hands-on approach in teaching",            
                           "Facilitator encouraged participation")
Maw3_24feb <- Maw3_24feb %>%
  gather("Goal clearly communicated",
         "Duration Too Long",                       
         "Overall Ratinf of session is Excellent ",     
         "Goal of Session was achieved",      
         "Easy to Understand Facilitator",                      
         "Excited to use hands-on approach in teaching",            
         "Facilitator encouraged participation", key = "Questions", value = "Responses")

Maw3_24feb


Maw3_24feb <- Maw3_24feb %>%
  group_by(Date,Questions) %>%
  count(Responses)
colnames(Maw3_24feb) <- c("Date", "Questions", "Responses", "Number")

ggplot(Maw3_24feb, aes(Questions, Number, fill = Responses))+
  geom_bar(stat = "identity") + 
  coord_flip() + 
  geom_text(aes(label = Number), colour = "white", hjust = 2, position = "stack", size = 7) + 
  ggtitle("Design Challenge Poll Reponses_Ahanta West Online Training") + 
  labs(y = "Number of Participants") + 
  theme(plot.title = element_text(size =25, face = "bold"))+
  theme_bw()+
  theme(axis.text=element_text(size=15, face = "bold", colour = "black"),axis.title=element_text(size=20,face="bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.text = element_text(size = 13,
                                   face = "bold"),
        legend.title = element_text(size = 14,
                                    face = "bold"))

## Visualizing the data for 1st March

Maw3_1Mar <-zoom_data3

Maw3_1Mar <- Maw3_1Mar %>%
  select(!Name) %>%
  select(!Email)
colnames(Maw3_1Mar) <- c( "Date",                                                                  
                          "Facilitator encouraged participation",
                           "Enjoyed doing assignment",                       
                           "Difficult uploading assignment to Google Classroom",     
                           "Learnt a lot from completing assignment",      
                           "Difficult filming assignments",                      
                           "Difficult setting up materials for assignment",            
                           "Objective for assignment was clearly communicated", 
                          "Excited to use hands-on approach in teaching")
Maw3_1Mar <- Maw3_1Mar %>%
  gather("Facilitator encouraged participation",
         "Enjoyed doing assignment",                       
         "Difficult uploading assignment to Google Classroom",     
         "Learnt a lot from completing assignment",      
         "Difficult filming assignments",                      
         "Difficult setting up materials for assignment",            
         "Objective for assignment was clearly communicated", 
         "Excited to use hands-on approach in teaching", key = "Questions", value = "Responses")

Maw3_1Mar


Maw3_1Mar <- Maw3_1Mar %>%
  group_by(Date,Questions) %>%
  count(Responses)
colnames(Maw3_1Mar) <- c("Date", "Questions", "Responses", "Number")

ggplot(Maw3_1Mar, aes(Questions, Number, fill = Responses))+
  geom_bar(stat = "identity") + 
  coord_flip() + 
  geom_text(aes(label = Number), colour = "white", hjust = 2, position = "stack", size = 7) + 
  ggtitle("Discussion Session 1_Poll Responses_Ahanta West Online Training") + 
  labs(y = "Number of Participants") + 
  theme(plot.title = element_text(size =25, face = "bold"))+
  theme_bw()+
  theme(axis.text=element_text(size=15, face = "bold", colour = "black"),axis.title=element_text(size=20,face="bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.text = element_text(size = 13,
                                   face = "bold"),
        legend.title = element_text(size = 14,
                                    face = "bold"))

### Visualizing data for 5th March

Maw3_5Mar <-zoom_data4

Maw3_5Mar <- Maw3_5Mar %>%
  select(!Name)

colnames(Maw3_5Mar) <- c( "Date", 
                          "Facilitator encouraged participation",
                          "Difficult uploading assignment to Google Classroom",
                          "Difficult filming assignments",
                          "Difficult setting up materials for assignment",
                          "Enjoyed doing assignment",      
                          "Objective for assignment was clearly communicated",
                          "Excited to use hands-on approach in teaching",
                          "Learnt a lot from completing assignment",      
                          "Difficult sourcing materials for assignments")
Maw3_5Mar <- Maw3_5Mar %>%
  gather("Facilitator encouraged participation",
         "Difficult uploading assignment to Google Classroom",
         "Difficult filming assignments",
         "Difficult setting up materials for assignment",
         "Enjoyed doing assignment",      
         "Objective for assignment was clearly communicated",
         "Excited to use hands-on approach in teaching",
         "Learnt a lot from completing assignment",      
         "Difficult sourcing materials for assignments", key = "Questions", value = "Responses")

Maw3_5Mar


Maw3_5Mar <- Maw3_5Mar %>%
  group_by(Date,Questions) %>%
  count(Responses)

colnames(Maw3_5Mar) <- c("Date", "Questions", "Responses", "Number")

ggplot(Maw3_5Mar, aes(Questions, Number, fill = Responses))+
  geom_bar(stat = "identity") + 
  coord_flip() + 
  geom_text(aes(label = Number), colour = "white", hjust = 2, position = "stack", size = 7) + 
  labs(y = "Number of Participants") + 
  ggtitle("Discussion Session 2_Poll Responses_Ahanta West Online Training ") + 
  theme(plot.title = element_text(size =25, face = "bold"))+
  theme_bw()+
  theme(axis.text=element_text(size=15, face = "bold", colour = "black"),axis.title=element_text(size=20,face="bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.text = element_text(size = 13,
                                   face = "bold"),
        legend.title = element_text(size = 14,
                                    face = "bold"))


## Visualizing data for 12th March

Maw3_12Mar <-zoom_data5

Maw3_12Mar1 <- Maw3_12Mar %>%
  select(!Name)%>%
select(!Email)
colnames(Maw3_12Mar1) <- c( "Date",                                                                  
                           "Enjoyed doing assignment",
                           "Difficult uploading assignment to Google Classroom",
                           "Facilitator encouraged participation",
                           "Learnt a lot from completing assignment",
                           "Difficult filming assignments",
                           "Difficult setting up materials for assignment",
                           "Overall rating of discussion was Excellent",
                           "Objective for assignment was clearly communicated",
                           "Excited to use hands-on approach in teaching",
                           "Difficult sourcing materials for assignments")
Maw3_12Mar2 <- Maw3_12Mar1 %>%
  gather("Enjoyed doing assignment",
         "Difficult uploading assignment to Google Classroom",
         "Facilitator encouraged participation",
         "Learnt a lot from completing assignment",
         "Difficult filming assignments",
         "Difficult setting up materials for assignment",
         "Overall rating of discussion was Excellent",
         "Objective for assignment was clearly communicated",
         "Excited to use hands-on approach in teaching",
         "Difficult sourcing materials for assignments", key = "Questions", value = "Responses")

Maw3_12Mar2


Maw3_12Mar3 <- Maw3_12Mar2 %>%
  group_by(Date, Questions) %>%
  count(Responses)

colnames(Maw3_12Mar3) <- c("Date", "Questions", "Responses", "Number")
Maw3_12Mar4 <-Maw3_12Mar3 %>% group_by(Questions,Responses) %>% count(Responses)
colnames(Maw3_12Mar4) <- c("Questions", "Responses", "Number")


ggplot(Maw3_12Mar4, aes(Questions, Number, fill = Responses))+
  geom_bar(stat = "identity") + 
  coord_flip() +
  geom_text(aes(label = Number), colour = "white", hjust = 2, position = "stack", size = 7) + 
  ggtitle("Final Discussion Session_Poll Responses_Ahanta West Online Training") + 
  labs(y = "Number of Participants") + 
  theme(plot.title = element_text(size =25, face = "bold"))+
  theme_bw()+
  theme(axis.text=element_text(size=15, face = "bold", colour = "black"),axis.title=element_text(size=20,face="bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.text = element_text(size = 13,
                                   face = "bold"),
        legend.title = element_text(size = 14,
                                    face = "bold"))
