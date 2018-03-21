library(tidyverse)
library(plotly)

graf <- ggplot(diamonds, aes(carat, price, color = color)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 20000, by = 5000),
                     limits = c(0, 20000)) +
  labs(x = "Karáty", y = "Cena", color = "Barva", 
       title = "Porovnání ceny diamantů podle karátů a barvy") +
  theme_bw()

ggplotly(graf)
