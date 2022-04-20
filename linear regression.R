
head(cars)


## regression analysis visualized in a scatter plot
ggplot(cars, aes(speed, dist))+
  geom_point()+
  geom_smooth(method = lm)

## Box plot visualization
ggplot(cars, aes(speed, dist))+ 
  geom_boxplot()

## Density plots for speed and distance
ggplot(cars, aes(dist))+ 
  geom_density(fill = "red4")

ggplot(cars, aes(speed))+ 
  geom_density(fill = "steelblue")

## Correlation
cor(cars$speed, cars$dist)

linearmod <- lm(dist ~ speed, cars)

summary(linearmod)
coefficients(linearmod)
coef(linearmod)
errorCondition(linearmod)









theme(strip.text = element_textbox)