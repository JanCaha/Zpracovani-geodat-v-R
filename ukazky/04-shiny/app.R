#---------------------------------------------
# statiscká část aplikace, která bude přístupná dále

library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(readxl)

okresy <- st_read(here::here("data", "okresy.gpkg"), stringsAsFactors = FALSE)
okresy_data <- read_xlsx(here::here("data", "socioekonomicka_data.xlsx"), sheet = 1)
okresy_kody <- read_xlsx(here::here("data", "socioekonomicka_data.xlsx"), sheet = 2)
okresy_data <- okresy_data %>% left_join(okresy_kody)
rm(okresy_kody)
okresy <- okresy %>% left_join(okresy_data, by = c("KOD_OKRES" = "kod_okresu"))
rm(okresy_data)

promenne <- c("nezamestnani", "nezamestnani_muzi", "nezamestnani_zeny", "pracovni_mista_v_evidenci")

titulky <- c("Nezaměstnanost", "Nezaměstnanost mužů", "Nezaměstnanost žen", "Počet pracovních míst v evidenci")
popisky_legendy <- c("Nezaměstnanost (%)", "Nezaměstnanost (%)", "Nezaměstnanost (%)", "Počet míst \n v evidenci ÚP")


#---------------------------------------------

# UI část aplikace
ui <- fluidPage(
   
   # Titulek
   titlePanel("Vizualizace dat o okresech"),
   
   # Sidebar s dvěmi nastaveními
   sidebarLayout(
      sidebarPanel(
         sliderInput("intervals",
                     "Number of interals:",
                     min = 1,
                     max = 9,
                     value = 4),
         selectInput("variables",
                     label = "Proměnné:",
                     selected = promenne[1],
                     choices = promenne)
      ),
      
      # Shlavní panel, kde jsou dva definované výstupy
      mainPanel(
         plotOutput("map"),
         plotOutput("legend")
      )
   )
)

# serverová část
server <- function(input, output) {
  
  # reaktivní prvek
  pozice <- reactive({
    which(promenne == input$variables)
  })
  
  # reaktivní prvek
  mapa <- reactive({
    tm_shape(okresy) +
      tm_polygons(col = input$variables, n = input$intervals , style = "quantile",
                  palette = "OrRd", title = popisky_legendy[pozice()])
    })
  
  # výstup - mapa
   output$map <- renderPlot({
     mapa() + 
      tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 50, 100), size = 0.75) + 
      tm_layout(frame = FALSE,
                legend.title.size = 1.3,
                legend.text.size = 1.0, 
                legend.format = list(text.separator = "-"),
                main.title = paste0(titulky[pozice()], " v okresech ČR k 31. 12. 2016"),
                main.title.position = "center", 
                legend.show = FALSE)
   })
   
   # výstup - legenda
   output$legend <- renderPlot({
     mapa() + 
       tm_layout(frame = FALSE,
                 legend.title.size = 1.3,
                 legend.text.size = 1.0, 
                 legend.format = list(text.separator = "-"),
                 legend.only = TRUE)
   })
}

# kód pro spuštění aplikace
shinyApp(ui = ui, server = server)
