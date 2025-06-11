install.packages("rgbif")
install.packages ("tibble")
library("tidyverse")
library("rgbif")
library(tibble)

# ler sobre as funções do pacote
?rgbif 

# checar funcoes
?occ_data

# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Paracanthurus hepatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(dori_gbif)
dim(dori_gbif$data)

# checar campos
dori_gbif$data %>% names

# checar problemas reportados
issues_gbif <- dori_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

#### A maioria dos problemas reportados é relacionado com discrepancias entre informações indicadas pelos autores e as levantadas pelo algoritmo de checagem, mas nenhum parece invalidar as ocorrências, por enquanto.
#### Prosseguimos selecionando algumas variáveis que serão úteis para a validação dos dados e futuras análises, como coordenadas, profundidade, nome da base de dados etc.

dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 

#### Note que temos 500 ocorrências, no entanto, vamos ver quantas são únicas aplicando a função distinct do pacote dplyr

dori_gbif1 <- dori_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(dori_gbif1, unique)

#### Agora iniciamos o processo de checagem mais fina que não é realizada pelo algoritmo, por apresentar especificidades que vão além de sua programação. Inicialmente, podemos verificar se as coordenadas são válidas (e.g., latitudes > 90 ou longitude > 180) utilizando funções dos pacotes CoordinateCleaner e bcd. Clicando nos links dos pacotes vocês podem checar diversas outras funcionalidades oferecidas.

install.packages("bdc")
install.packages ("CoordinateCleaner")

library(bdc)
library(CoordinateCleaner)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dori_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- dori_gbif1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                "zeros"))
# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
library("ggplot2")
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cap`)) +
  coord_quickmap() +
  theme_classic()
1

# pontos no mar
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.sea`)) +
  coord_quickmap() +
  theme_classic()

# investigar niveis suspeitos
library("dplyr")
dori_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()

# waterBody
dori_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 

# fonte das regioes erradas após checar no FISHBASE que não há ocorrência no Atlântico.
dori_gbif1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)

dori_gbif1 <- dori_gbif1 %>% 
  filter(!waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire"))

# 14 ocorrencias
dori_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science")) %>% 
  data.frame()

# filtrar todas do dataset suspeito
dori_gbif_noDiveboard <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))

# Sem o Atlântico, ainda existem algumas suspeitas no Mediterrâneo e Mar do Norte (a Dori é tropical!); além do norte do Japão.

dori_gbif_noDiveboard %>% 
  filter(decimalLatitude > 25) %>% 
  arrange(-decimalLatitude) %>% 
  data.frame()

dori_gbif_ok <- dori_gbif_noDiveboard %>% 
  filter(decimalLatitude < 31) 

install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))

# Podemos usar a profundidade como outro critério, pois esta espécie é associada apenas a recifes rasos segundo o FishBase. E parece tudo ok.
# checar profundidade
dori_gbif_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

install.packages("robis")
library("robis")

## OBIS
dori_obis <- robis::occurrence("Paracanthurus hepatus")

# checar dados
names(dori_obis)

dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
library("dplyr")
dori_obis1 %>% 
  distinct(flags)

# check NA em datasetName
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         is.na(datasetName)) %>% 
  distinct(waterBody)


# depth ok
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique")) %>% 
  lapply(., unique)

# aplicar filtros
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique"))


library(ggplot2)
library(maps)  # pode ser necessário para map_data()

world <- map_data("world")

# plot
ggplot() +
  geom_polygon(data =world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))

# Ainda temos ocorrências no Atlântico, então vamos limpar isso e plotar o mapa novamente.

dori_obis_final <- dori_obis_ok %>% 
  filter(decimalLongitude > 0 | decimalLongitude < -100)

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_final, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))


#### unir GBIF e OBIS
# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis_ok))
setdiff(names(dori_obis_final), names(dori_obis_final))

library(tidyr)
library("dplyr")

all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis_final %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Paracanthurus hepatus") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))

dir.create("data")
write.csv(all_data, "data/occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)

install.packages("CoordinateCleaner")
install.packages("biogeo")

# funcao para classificar ocorrencias suspeitas
flag_outlier <- function(df, species){
  
  # funcao para classificar ocorrencias suspeitas
  # baseada no calculo do centroide de todas ocorrencias
  # indica como 'check' as ocorrencias que tem distancias até o centroide
  # acima do 90th quantil (default) das distancias calculadas
  
  dados <- df %>% 
    dplyr::filter(scientificName == species); 
  
  dados2 <- geosphere::distVincentyEllipsoid(
    dados %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    dados %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(dados) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.90), "OK",
                         ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK"))))
  
  # mutate(flag = ifelse(dist_centroid > quantile(dist_centroid, probs = prob), "check", "OK"))
  
  print(dados2)
  
}
library(dplyr)


# classificar ocorrências
marcados <- dori_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., species = "Paracanthurus hepatus (Linnaeus, 1766)")

### mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))

library(CoordinateCleaner)
flags <-
  clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas")
  )

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = datasetName)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))
