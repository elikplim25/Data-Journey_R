##############################################################################


library(tidyverse)
library(Lahman)
head(Teams)

### runs per game vs home runs ######

Teams %>%
  filter(yearID %in% 1961:2001)%>%
  ggplot(aes(R, HR))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm")+
  theme_bw()

#### runs per game vs at bats #####

Teams %>%
  filter(yearID %in% 1961:2001)%>%
  ggplot(aes(R, AB))+
  geom_point(alpha = 0.5)+
  #geom_smooth(method = "lm")+
  theme_bw()

###win rate (number of wins per game) versus number of fielding errors (E)###

Teams %>%
  filter(yearID %in% 1961:2001)%>%
  ggplot(aes(X3B, X2B))+
  geom_point(alpha = 0.5)+
  theme_bw()+
  geom_smooth(method = "lm")

### correlation #####

wins <- Teams_filtered$W
errors <- Teams_filtered$E
cor(wins, errors)

doubles <- Teams_filtered$X2B
triples <- Teams_filtered$X3B
cor(doubles, triples)

##############################################################################

#### Galton's inception of regression and correlation ###############

library(HistData)

head(GaltonFamilies)
dim(GaltonFamilies)

##### extracting data for analysis ########

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
view(galton_heights)

###### visualizing data #########

galton_heights%>%  
ggplot(aes(father, son))+
  geom_point()+
  geom_smooth(method = "lm")



