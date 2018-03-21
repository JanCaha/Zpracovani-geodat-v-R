#---------------------------------------
# instalace a načtení balíků

# instalace balíku Tidyverse
install.packages("tidyverse")

# instalace balíku s daty 
install.packages("nycflights13")

# načtení balíku Tidyverse
library(tidyverse)



#---------------------------------------
# ukázka práce s daty s využitím dplyr

# načtení datové sady diamonds z balíku ggplot2 (součást Tidyverse)
# do aktuálního prostředí
data("diamonds")

# ukázka filtrace a sumarizace bez použití pipeline
diamonds_filtered <- filter(diamonds, price > 9000)
diamonds_grouped <- group_by(diamonds_filtered, cut, color)
diamonds_summarise <- summarise(diamonds_grouped, 
                                count = n(),
                                price_min = min(price),
                                price_max = max(price),
                                carat = median(carat))

# zobrazení sumarizovaných dat
View(diamonds_summarise)

# ukázka filtrace a sumarizace dat
diamonds_summarise <- diamonds %>% 
  filter(price > 9000) %>% 
  group_by(cut, color) %>% 
  summarise(count = n(),
            price_min = min(price),
            price_max = max(price),
            carat = median(carat))

# zobrazení sumarizovaných dat
View(diamonds_summarise)

# zobrazení tabulky seřazené podle karátů sestupně
View(diamonds_summarise %>% arrange(desc(carat)))

# ukázka joinů
library("nycflights13")
data(flights, airports)

View(flights)
View(airports)

# propojení tabulek
joined_data <- flights %>% 
  left_join(airports, by = c("origin" = "faa"))

View(joined_data)

# další příklady na webu - http://dplyr.tidyverse.org/



#---------------------------------------
# vizualizace pomocí balíků ggplot2

# vizualizace dat
ggplot(diamonds, aes(carat, price)) +
  geom_point()

# vizualizace dat s přidáním barvy
ggplot(diamonds, aes(carat, price, color = color)) +
  geom_point()

# vizualizace dat doplněně od nezbytná nastavení 
ggplot(diamonds, aes(carat, price, color = color)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 20000, by = 5000),
                     limits = c(0, 20000)) +
  labs(x = "Karáty", y = "Cena", color = "Barva", 
       title = "Porovnání ceny diamantů podle karátů a barvy") +
  theme_bw()

# uložení aktuální grafu
ggsave(here::here("graficke_vystupy", "diamanty_karáty_barvy.pdf"))



#---------------------------------------
# uložení dat

# uložení dat jako CSV
diamonds_summarise %>% 
  write_csv("data_sumarziované.csv")

# balík pro zapisování do Excelu
install.packages("writexl")
library(writexl)

# zápis do excelu s pipeline
diamonds_summarise %>% 
  write_xlsx("data_sumarziované.xlsx")

# identický zápis bez pipeline
write_xlsx(diamonds_summarise, "data_sumarziované.xlsx")
