#---------------------------------------
# instalace a načtení balíků
install.packages(c("sf", "tmap", "readxl"))

library(tidyverse)
library(sf)
library(tmap)
library(readxl)

#---------------------------------------
# operace s daty

# načtení dat s geometrií
okresy <- st_read(here::here("data", "okresy.gpkg"), stringsAsFactors = FALSE)

# tabulková data
okresy_data <- read_xlsx(here::here("data", "socioekonomicka_data.xlsx"), sheet = 1)
okresy_kody <- read_xlsx(here::here("data", "socioekonomicka_data.xlsx"), sheet = 2)

# propojení tabulkových dat
okresy_data <- okresy_data %>% left_join(okresy_kody)

# odstranění nepotřebných dat z paměti
rm(okresy_kody)

# join tabulky na geometrická data
okresy <- okresy %>% left_join(okresy_data, by = c("KOD_OKRES" = "kod_okresu"))

rm(okresy_data)

# vytvoření nového atributu
okresy <- okresy %>% 
  mutate(pracovni_mista_na_obyvatele = pracovni_mista_v_evidenci / obyvatel)

#---------------------------------------
# vizualizace pomocí balíku tmap

## ukázka palet
# tmaptools::palette_explorer() 

# základní verze
tm_shape(okresy) +
  tm_polygons(col = "nezamestnani", n = 5 , style = "quantile", palette = "OrRd")

# s větším množstvím nastavení a uložení do proměnné
mapa <- 
tm_shape(okresy) +
  tm_polygons(col = "nezamestnani", n = 5 , style = "quantile",
              palette = "OrRd", title = "Nezaměstnanost (%)") +
tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 50, 100), size = 0.75) + 
tm_layout(frame = FALSE,
          legend.title.size = 1.3,
          legend.text.size = 1.0, 
          legend.format = list(text.separator = "-"),
          main.title = "Nezaměstnanost v okresech ČR k 31. 12. 2016",
          main.title.position = "center")

# zobrazení mapy z proměnné
mapa

# uložení mapy ve formátu PDF
save_tmap(mapa, here::here("graficke_vystupy", "nezamestnanost.svg"))

# interaktivní zobrazení
tmap_mode("view")
mapa

# přepnutí zpět na klasické vykreslování
tmap_mode("plot")

# #---------------------------------------
# vizualizace pomocí ggplot2

# instalace vývojové verze ggplot2
devtools::install_github("tidyverse/ggplot2")

# základní verze
ggplot() + 
  geom_sf(data = okresy, aes(fill = nezamestnani))

# další nastavení dle grafů v ggplot2

#---------------------------------------
# vytvoření geometrie krajů

# nastavení přesnosti geometrie
# předchází problémům se zbytkovými geometriemi
st_precision(okresy) <- 100

kraje <- okresy %>% 
  group_by(kraj) %>% 
  summarise(obyvatel = sum(obyvatel),
            pracovni_mista_v_evidenci = sum(pracovni_mista_v_evidenci))

## někdy nastane po některých operacích problém s geometrií
## funkce hlásí, že nemohou najít sloupec s geometrií
## řešením je sloupec vyjmout a znovu vložit do datové sady
## viz následující 4 řádky kódu
#
# sfc <- st_geometry(kraje)
# st_geometry(kraje) <- NULL
# st_geometry(kraje) <- sfc
# rm(sfc)

tm_shape(kraje) +
  tm_polygons(col = "obyvatel")

# uložení vrstvy krajů
st_write(kraje, here::here("data", "kraje.gpkg"))



#---------------------------------------
# vizualizace více vrstev pomocí balíku tmap

tm_shape(okresy) +
  tm_polygons(col = "pracovni_mista_na_obyvatele", n = 4 , style = "quantile",
              palette = "Reds", title = "Pracovních míst \n na obyvatele") +
tm_shape(kraje) +
  tm_borders(col = "black", lwd = 2) +
tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 50, 100), size = 0.75) + 
tm_layout(frame = FALSE,
          legend.title.size = 1.3,
          legend.text.size = 1.0, 
          legend.format = list(text.separator = "-"),
          main.title = "Počet pracovních míst v evidenci ÚP na obyvatele 
          okresech ČR k 31. 12. 2016",
          main.title.position = "center")