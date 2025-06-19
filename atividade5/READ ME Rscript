### ATIVIDADE 5

install.packages ("leaflet")
library(tidyverse)
library(rgbif)
library (leaflet)

# ocorrencias
corvus_gbif <- occ_data(scientificName = "Corvus", 
                        hasCoordinate = TRUE,
                        hasGeospatialIssue = FALSE)
# checar issues
issues_gbif <- corvus_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

# selecionar variaveis
corvus <- corvus_gbif$data %>%
  dplyr::select(scientificName, decimalLatitude, decimalLongitude) %>% 
  distinct()

library(leaflet)

# conferir no mapa
corvus %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~decimalLongitude,
             ~decimalLatitude)

pal <- colorFactor(palette = "viridis", domain = unique(corvus$scientificName))

corvus %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~decimalLongitude,
                   ~decimalLatitude,
                   radius = 5,
                   label = ~as.character(scientificName),
                   color = ~pal(corvus$scientificName),
                   stroke = FALSE, fillOpacity = 0.5) %>% 
  addLegend('bottomright', 
            colors = unique(pal(corvus$scientificName)), 
            labels = unique(corvus$scientificName),
            title = 'Espécie',
            opacity = 0.5)

corvus %>% 
  ggplot(aes(y = decimalLatitude, fill = scientificName)) +
  geom_histogram() +
  theme_classic() +
  labs(y = "latitude", x = 'ocorrências') +
  ylim(-50, 70)

corvus %>% 
  ggplot(aes(y = decimalLatitude, fill = scientificName)) +
  geom_histogram() +
  theme_classic() +
  labs(y = "latitude", x = 'ocorrências') +
  ylim(35, 65)

install.packages("plotly")
library(plotly)

cc <- corvus %>% 
  ggplot(aes(y = decimalLatitude, fill = scientificName)) +
  geom_histogram() +
  theme_classic() +
  labs(y = "latitude", x = 'ocorrências') +
  ylim(-50, 70)

ggplotly(cc)
