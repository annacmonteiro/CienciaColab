# Primeiro passo é carregar o diretório de trabalho e instalar os pacotes necessários
setwd("/Users/annamonteiro/Documents/UENF/Ciencia_Aberta")

# Para instalar o pacote usar install.packages. Após instalação, sempre lembrar de abrir/puxar o mesmo, usando a função library.
install.packages("validate")
install.packages("taxadb")
library("tidyverse")
library("validate")
library("vegan")
library ("taxadb")
library("dplyr")

# Indicar ao R qual o seu objeto de trabalho com <- e seu respectivo arquivo ao lado direito da seta. Toda vez que quiser citar a planilha, só indicar o nome do objeto criado.
# Heather é o cabeçalho da planilha, indicar se tem ou não tem com (T)rue or (F)alse.
iris <- read.csv("iris_spp.csv", header = T)

# A função lapply te mostra os números inteiros e categóricos da sua tabela. Isso é ótimo para percebermos erros categóricos em gráficos, e.g. corrigir erro de mais spp do que deveria: existiam caracteres diferentes para a mesma espécie: iris sertosa e Iris sertosa.
lapply(iris, unique)


# Passo 2:

iris %>%
  select(Species, Sepal.Lenght:Petal.Width) %>% 
  pivot_longer(cols = -Species, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(x = valores, fill = Species)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (cm)") +
  scale_fill_discrete(
    expression(bold("Species:")),
    labels = c(expression(italic("Iris setosa")), 
               expression(italic("Iris versicolor")), 
               expression(italic("Iris virginica"))))
 #### Passo 3:
rules <- validator(in_range(latitude, min = -90, max = 90),
                   in_range(longitude, min = -180, max = 180),
                   is.character(site),
                   is.numeric(data),
                   all_complete(iris))

out   <- confront(iris, rules)
summary(out)

# Passo 4:
plot(out)

# Passo 5: check taxa
species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  c("Iris setosa", .) %>% # inserimos uma espécie fictícia para teste
  filter_name(., provider = "itis") %>% 
  data.frame() %>% 
  bind_cols(Species = iris %>% 
              distinct(Species) %>% 
              pull())

# É necessário carregar o seguinte pacote para o próximo passo
library("vegan")

?iris

iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, acceptedNameUsageID, scientificName)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

## Planilha 1: create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

## Planilha 2: create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, acceptedNameUsageID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

## Planilha 3: create measurementsOrFacts
library("dplyr")

eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))

dir.create("DwC_Files")
write.csv(eventCore, "DwC_Files/eventCore.csv", row.names = FALSE)
write.csv(occurrences, "DwC_Files/occurrences.csv", row.names = FALSE)
write.csv(eMOF, "DwC_Files/eMOF.csv", row.names = FALSE)

# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(eventCore$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)

# check NA values
eMOF %>%
  filter(is.na(eventID))

occurrences %>%
  filter(is.na(eventID))

# Ultimo passo:Escrevendo as matrizes como arquivos de texto
rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}
