library(tidyverse)

head(COVID)
colnames(COVID)

COVID_Dates <- COVID %>% 
  select(date, new_cases, new_deaths)

##removing scientific numbers
options(scipen=999)

###Comparison between Number OF COVID-19 Cases and Deaths Recorded

New_dates_covid<- gather(COVID_Dates, "new_cases", "new_deaths", key = "Recordings", value = "Number")
COVID_Dates <- new_covid %>% select(date, new_cases, new_deaths)
ggplot(loc_data_covid, aes(new_deaths, new_cases, colour = continent)) + 
  geom_point() + ##facet_wrap(~continent) + 
  theme_bw() + 
  labs(title = "Comparison between Number OF COVID-19 Cases and Deaths Recorded in Countries per Continent (24th Feb, 2020 - 8th Feb, 2021)")

###impact of COVID continentally

continent_total_covid <- aggregate(cbind(new_cases, new_deaths) ~continent, COVID, sum)

Continents1 <- continent_total_covid %>%
  gather("new_cases", "new_deaths", key = "Specifics", value = "Number")

ggplot(Continents1, aes(reorder(continent, Number), Number, fill = Specifics)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  geom_text(aes(label = Number), vjust = -0.5, position = position_dodge(.9), size = 4.5) + 
  labs(title = "Total Number COVID-19 Cases and Deaths per Continent (24th Feb, 2020 - 8th Feb, 2021)", 
       x = "Continent", y = "Number") + 
  theme(axis.text=element_text(size=10, face = "bold", colour = "black"),axis.title=element_text(size=20,face="bold")) +
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  facet_wrap(~Specifics) +
  scale_fill_manual(values = c("Steelblue", "red"))
  


continent_total_covid %>% mutate(recovered_cases = new_cases - new_deaths)

ggplot(loc_data_covid, aes(new_deaths, new_cases, colour = continent)) + 
  geom_point() + 
  facet_wrap(~continent) + 
  theme_bw()

