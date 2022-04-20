Census_data

mean(Census_data$`First Census`)
mean(Census_data$`Second Census`)

Cen_diff <- Census_data %>%
  mutate(Difference = (`Second Census` - `First Census`))
 
  ggplot(Cen_diff, aes(Tribes, Difference)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
    coord_flip() + 
    geom_text(aes(label = Difference), hjust = -0.05, colour = "red") + 
    theme_bw()
    
