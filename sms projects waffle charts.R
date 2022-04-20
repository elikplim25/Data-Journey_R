install.packages("waffle")
library(waffle)
library(tidyverse)

expenses <- c(`Infants: <1(16467) `=16467, `Children: <11(30098) `=30098,
              `Teens: 12-17(20354)`=20354, `Adults:18+(12456) `=12456,
              `Elderly: 65+(12456) `=12456)

vecgiz2 <- c(`Ashanti` = 2120, `Bono Region` = 528, `Central` = 380, 
             `Eastern` = 1794, `Western North` = 524)
expenses
waffle(expenses/1000, rows=5, size=0.6,
       colors=c("#44D2AC", "#E48B8B", "#B67093",
                "#3A9ABD", "#CFE252"),
       title="Age Groups bifurcation",
       xlab="1 square = 1000 persons")

waffle(vecgiz2/10, rows = 20, size = 0.6,
       colors=c("#44D2AC", "#E48B8B", "#B67093",
                "#3A9ABD", "#CFE252"), title = "xxx", 
       xlab="Districts")


vecgiz <- as.vector(GIZ_Reg)




#Developp progress analysis

devPPP <- DEGprogress %>%
        gather( `Current Progress`, Target, key = "Indicators", 
               value = "Numbers")

ggplot(devPPP, aes(fill = Indicators, values = Numbers))+
        geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) 

devPPP %>%
        ggplot(aes(fill = Indicators, values = Numbers)) + 
        expand_limits(x=c(0,0), y=c(0,0))+
        coord_equal() +
        labs(fill = NULL, colour = NULL) +
        theme_enhance_waffle() -> buf
        
buf + geom_waffle(
        color = "white", size = 2, n_rows = 20
) +
        theme(strip.text.x = element_text(hjust = 0.5))





ggplot(devPPP, aes(fill = Indicators, values =Numbers)) +
        geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
        facet_wrap(~Activity, nrow = 1, strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                           expand = c(0,0)) +
        scale_fill_manual(values = c("red", "grey")) +
        coord_equal() +
        labs(
                title = "Developpp_ Current progress vs Dry Season Target",
                subtitle = "progress as at Sept 20, 2021",
                x = "Target",
                y = "Numbers"
        ) +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))+
        theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
              axis.title = element_text(size = 15, face = "bold"),
              legend.position = "top",
              legend.title = element_blank(),
              legend.text=element_text(size=13, face = "bold")) + 
        theme(plot.title = element_text(size = 20, face = "bold"))

### Developp Progress analysis

DEGp[DEGp == "Total number of farmers registered"] <- "Registered Farmers"
DEGp[DEGp == "Field trainers identified and trained"] <- "FTs Identified/Trained"
DEGp[DEGp == "Farming household identified and trained in GAPS"] <- 
        "Farming Households Trained in GAPs"
DEGp[DEGp == "Linkages made with exporter"] <- "Exporters"

DEGp<- DEGp %>% filter(Activity %in% c("Registered Farmers", "FTs Identified/Trained", 
                              "Farming Households Trained in GAPs", "Exporters"))
DEGp <- DEGp %>% select(Activity,`Annual Target`,`Dry Season Target`)



DEGp1<- DEGp %>% gather(`Annual Target`,`Dry Season Target`, key = "Indicators", value = "Numbers")



###Visualizing the data as a waffle chart

ggplot(DEGp1, aes(fill = Indicators, values =Numbers)) +
        geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
        facet_wrap(~Activity, nrow = 1, strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                           expand = c(0,0)) +
        scale_fill_manual(values = c("green4", "grey")) +
        coord_equal() +
        labs(
                title = "DEG_ Current progress vs Dry Season Target",
                subtitle = "progress as at Sept 20, 2021",
                x = "Target",
                y = "Numbers"
        ) +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))+
        theme(axis.text = element_text(size = 15, colour = "black", face = "bold"),
              axis.title = element_text(size = 15, face = "bold"),
              legend.position = "top",
              legend.title = element_blank(),
              legend.text=element_text(size=13, face = "bold")) + 
        theme(plot.title = element_text(size = 20, face = "bold"))
