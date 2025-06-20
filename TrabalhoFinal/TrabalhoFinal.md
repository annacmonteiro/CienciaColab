---
title: "Trabalho Final"
author: "Anna Monteiro"
date: "2025-06-20"
output: html_document
---
# Peixe-papagaio brasileiro (Sparisoma amplum)

```{r}
knitr::include_graphics("/Users/annamonteiro/Documents/UENF/Ciencia_Aberta/TrabalhoFinal/sparisoma-amplum.png")
```

Os peixes papagaio são espécies chave em ecossistemas recifais rasos, uma vez que os mesmos possuem uma adaptação única do aparato bocal em formato de "bico", a qual proporciona mordidas em substratos duros. Tais organismos se alimentam majoritariamente de microalgas endolíticas que residem dentro de organismos holobiontes coralíneos (corais duros), ocupando um nicho ecológico único e por sua vez proporcionando uma importante ciclagem de carbono inorgânico do carbonato de cálcio (CaCO3) proporcionado pelos esqueletos de corais duros.

A espécie de peixe-papagaio Sparisoma amplum é endêmica do Brasil e possui cores exuberantes em sua fase terminal, mesclando diferentes tons de azul e verde com detalhes em vermelho. 


```{r, message=FALSE}
# pacotes necessarios
library("tidyverse")
library("dplyr")
library("CoordinateCleaner")
library("ggplot2")
library("ggmap")
library("maps")
library("mapdata")
library("rgbif")
library("robis")
library("leaflet")
```


```{r setup, include=FALSE}

# baixar ocorrencias
s.amplum_gbif <- occ_data(scientificName = "Sparisoma amplum", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(s.amplum_gbif)
``` 


Checando nomes das variáveis disponíveis relacionadas a S. amplum:
``` {r}
s.amplum_gbif$data %>% names
```

Problemas reportados: coluna 'issues'

```{r}
issues_gbif <- s.amplum_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)
```

Selecionando variáveis que serão úteis para a validação dos dados:
```{r}
s.amplum_gbif1 <- s.amplum_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
         issues, basisOfRecord, occurrenceStatus, rightsHolder, 
         datasetName, recordedBy, depth, locality) 
```

Para vermos quantas ocorrências são únicas, aplicamos a função distinct do pacote dplyr.
```{r}
s.amplum_gbif1 <- s.amplum_gbif1 %>% 
  distinct()
```

Para identificar todos os valores únicos presentes nos dados, vamos aplicar a função unique a cada coluna com um loop na função lapply:
```{r}
# checar niveis dos fatores
lapply(s.amplum_gbif1, unique)
```

# Problemas não reportados: checagem de dados refinada
Para checar coordenadas válidas:
```{r}
library(bdc)
library(CoordinateCleaner)

check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = s.amplum_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")
```
Nenhum problema com as coordenadas aparentemente. Uma coluna foi adicionada, agora temos 13 variáveis.

Para checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países):
```{r}
cl <- s.amplum_gbif1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                 "zeros"))
```

# Ocorrências por regiões:
```{r}
s.amplum_gbif1 %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~decimalLongitude,
             ~decimalLatitude)
```

# Ocorrências por localidade:
Note que as localidades fazem parte das regiões marcadas no mapa acima em escala maior, provenientes de ambientes recifais da costa brasileira e das ilhas oceânicas brasileiras, pois S. amplum é uma espécie endêmica do Brasil.

```{r}
s.amplum_gbif1 %>%
  count(locality) %>%
  mutate(locality = forcats::fct_reorder(locality, n)) %>%
  ggplot(aes(y = locality, x = n, fill = locality)) +
  geom_col() +
  theme_classic() +
  labs(y = "Localidade", x = "Número de Ocorrências") +
  theme(legend.position = "none")
```
# Ocorrências por profundidade:
Segundo a base de dados do FishBase, Sparisoma amplum é uma espécie de recifes rasos e todas as ocorrências foram reportadas até ~18 metros de profundidade.
```{r, message=FALSE, warning=FALSE}
s.amplum_gbif1 %>% 
  ggplot(aes(x = depth, fill = locality)) +
    geom_histogram() 
```

