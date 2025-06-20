---
title: "atividade4"
output: html_document
date: "2025-06-11"
---

# ACESSO A BANCOS DE DADOS ABERTOS: Repositório de dados de ocorrências de espécies.

Atividade: acessar os repositórios do GBIF e OBIS, inspecionar os dados baixados, avaliar sua qualidade e fazer mapas de ocorrências da espécie selecionada.

Obs. GBIF engloba espécies terrestres e aquáticas, enquanto OBIS engloba apenas espécies marinhas. 

Espécie marinha selecionada: Paracanthus hepatus.

# Pacotes necessários
tidyverse
dplyr
CoordinateCleaner
bcd
ggplot2
ggmap
maps
mapdata
rgbif
robis

```{r, message = FALSE}
library("tidyverse")
library("dplyr")
library("CoordinateCleaner")
library("ggplot2")
library("ggmap")
library("maps")
library(mapdata)
library(rgbif)
library(robis)
```


# Baixar ocorrências (dados) e checar dimensões dos mesmos:
```{r}
dori_gbif <- occ_data(scientificName = "Paracanthurus hepatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)
                      
dim(dori_gbif)
dim(dori_gbif$data)
```


# Checar campos das variáveis disponíveis relacionadas a P. hepatus:
As variáveis podem ser utilizadas para filtrar as ocorrências de acordo com o objetivo, além de fornecerem diversos dados a respeito das ocorrências.
```{r}

dori_gbif$data %>% names
```


# Problemas reportados: coluna 'issues'
A coluna indica os problemas já identificados pelo validador automático do repositório.
Para checar os 'issues' indicados na base baixada, é necessário utilizar a função strsplit para individualizar os issues e poder conferí-los, pois algumas ocorrências possuem múltiplos problemas. 

Para checar problemas reportados:
```{r}
issues_gbif <- dori_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)
```

Nenhum issue parece invalidar as ocorrências, por enquanto.

Prosseguimos selecionando algumas variáveis que serão úteis para a validação dos dados e futuras análises, como: coordenadas, profundidade, nome da base de dados etc.
```{r}
dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
         issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
         datasetName, recordedBy, depth, locality, habitat) 
```

Para vermos quantas ocorrências são únicas, aplicamos a função distinct do pacote dplyr.
```{r}
dori_gbif1 <- dori_gbif1 %>% 
  distinct() 
```

Algumas ocorrências foram tiradas por conta de diferenças em colunas.

Para identificar todos os valores únicos presentes nos dados, vamos aplicar a função unique a cada coluna com um loop na função lapply.
```{r}
# checar niveis dos fatores
lapply(dori_gbif1, unique)
```

# Problemas não reportados
Processo de checagem mais fina que não é realizada pelo algoritmo devido sua limitação de programação. 

Inicialmente, podemos verificar se as coordenadas são válidas (e.g., latitudes > 90 ou longitude > 180) utilizando funções dos pacotes CoordinateCleaner e bcd. Clicando nos links dos pacotes vocês podem checar diversas outras funcionalidades oferecidas.

Para checar coordenadas válidas:
```{r}
library(bdc)
library(CoordinateCleaner)

check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dori_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")
```

Para checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países):
```{r}
cl <- dori_gbif1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                 "zeros"))
```


# Verificar visualmente coordenadas com flags:
O próximo passo é para gerar gráficos com as coordenadas associadas a espécie alvo  para conferir os alertas - flags - indicados pelas funções. 


1) Capitais (padrão é um raio de 10km): 
Não tivemos nenhuma coordenada inválida, mas algumas ocorrências parecem estar muito próximas a capitais. No entanto, todas as capitais estão em terra e, nesse caso, temos que investigar se as ocorrências estão em terra (lembre-se a Dori vive no mar!) ou apenas próximas a países insulares.
```{r}

ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cap`)) +
  coord_quickmap() +
  theme_classic()
```

2) Pontos no mar
```{r}
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.sea`)) +
  coord_quickmap() +
  theme_classic()
```

# Checar distribuição das ocorrências
Excluir dados em terra: Checar a distribuição das ocorrências em relação às regiões oceanográficas indicadas nos dados (waterBody). Isso vale apenas para o OBIS.

Investigar níveis suspeitos:
```{r}
dori_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()
```

Função waterBody
```{r}
dori_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
    geom_bar(stat = 'identity') 
```

Aparentemente esta espécie tem sido reportada no mundo todo, mas podemos acessar bancos de dados especializados para checar estas informações:No caso de peixes (Osteichthyes e Chondrichthyes) o 'FishBase' é a fonte mais atualizada de informações deste grupo. 

Depois desta confirmação, podemos suspeitar das ocorrências indicadas no Atlântico e, o tratamento mais rigoroso é a exclusão de qualquer ocorrência suspeita.

Fontes das regiões erradas
```{r}
dori_gbif1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)
```

Alguma característica destas ocorrências do Atlântico podem dar pistas de como continuar filtrando os resultados. Neste caso, abaixo podemos ver que, ao investigarmos um programa de ciência de identificação realizada por mergulhadores amadores, notamos que este concentra a maior parte das suspeitas. Assim, é melhor ser conservador e remover todas as ocorrências associadas a tal programa.
```{r}
dori_gbif1 <- dori_gbif1 %>% 
  filter(!waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire"))
```

14 ocorrencias suspeitas
```{r}
dori_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science")) %>% 
  data.frame()
```

Filtrar todas do dataset suspeito
```{r}
dori_gbif_noDiveboard <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))
```

Agora não temos mais nenhuma ocorrência no Atlântico, mas ainda existem algumas suspeitas no Mediterrâneo, Mar do Norte e um ponto ao norte no Japão, pois P. hepatus é uma espécie tropical!

Excluindo coordenadas mais boreais:
```{r}
dori_gbif_noDiveboard %>% 
  filter(decimalLatitude > 25) %>% 
  arrange(-decimalLatitude) %>% 
  data.frame()
```

Como o registro do Japão é de um espécie preservado, indica que possivelmente foi coletado nesta latitude mesmo, então este vamos manter.

```{r}
dori_gbif_ok <- dori_gbif_noDiveboard %>% 
  filter(decimalLatitude < 31) 
```

Mapa final das ocorrências filtradas:
```{r}
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
```

# Profundidade como critério extra
Podemos usar a profundidade como outro critério, pois esta espécie é associada apenas a recifes rasos segundo o FishBase. 

Checar profundidade:
```{r}
dori_gbif_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
    geom_histogram() 
```

Parece tudo ok.

# OBIS
Agora vamos fazer os mesmos procedimentos com os dados do OBIS, utilizando o pacote robis e a função occurrence deste pacote.

1) Baixar as ocorrências
```{r}
## OBIS
dori_obis <- robis::occurrence("Paracanthurus hepatus")
```

2) Checar os dados
```{r}
names(dori_obis)
```

Temos variáveis com os mesmos nomes, pois ambos usam o sistema DwC, mas os problemas reportados neste caso são indicados na coluna flags.

```{r}
dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
         flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
         datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()
```

Checar problemas reportados (flags)
```{r}
dori_obis1 %>% 
  distinct(flags)
```

Checar NA em datasetName
```{r}
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         is.na(datasetName)) %>% 
  distinct(waterBody)
```

Aqui usamos as flags para filtrar ocorrências em terra, além de remover dados sem nome de dataset, filtrar ocorrências no Atlântico e verificar a profundidade reportada. 

Podemos usar um mapa para verificar melhor as ocorrências:
```{r}
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
    geom_histogram() 
```

Checar níveis
```{r}
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique")) %>% 
  lapply(., unique)
```

Aplicar filtros
```{r}
# aplicar filtros
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique"))
```

Gráfico
```{r}
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))
```

Ainda temos ocorrências no Atlântico, então vamos limpar isso e plotar o mapa novamente.
```{r}
dori_obis_final <- dori_obis_ok %>% 
  filter(decimalLongitude > 0 | decimalLongitude < -100)

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_final, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))
```

Parece tudo ok, e chegamos a 253 ocorrências potenciais.

Por fim, vamos unir todas as ocorrências, checar se existem duplicatas e plotar o resultado final.

# Unir GBIF e OBIS

Ver diferencas entre os dois repositórios:
```{r}
setdiff(names(dori_gbif_ok), names(dori_obis_ok))

setdiff(names(dori_obis_final), names(dori_obis_final))

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
```

Mapear ocorrências
```{r}
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))
```

O último passo é guardarmos os dados baixados e tratados para economizar tempo no próximo uso, mas o mais importante já está registrado, o passo-a-passo de como chegamos até os dados usados nas análises.
```{r}
dir.create("data")
write.csv(all_data, "data/occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)
```

# EXTRA: Classificação automática de pontos

Função ‘caseira’

Podemos usar outras ferramentas mais refinadas para nos ajudar a detectar ocorrências suspeitas, como as encontradas nos pacotes CoordinateCleaner, obistools, scrubr e biogeo. Além disso, podemos criar nossas próprias funções para auxiliar nessa tarefa.

Abaixo, vamos utilizar os dados baixados do GBIF antes da limpeza já realizada acima.

Aqui vou começar a exemplificar com uma função simples criada por mim. 
Esta função utiliza as coordenadas para calcular o centróide (ponto médio de todas as ocorrências) e, a partir dele, a distância de cada ponto até o centróide. Esse princípio se baseia em propriedades de conectividade de populações contíguas, então quanto mais distantes (neste caso as muito distantes) maior a chance de termos uma ocorrência suspeita da mesma espécie. 

Atenção: isso é apenas uma ferramenta para classificar as ocorrências! A decisão de filtrar ou não os pontos suspeitos vai depender do seu conhecimento ou da literatura a respeito dos habitats e regiões de ocorrência da espécie-alvo.

Inicialmente, vamos carregar a função flag_outlier. E, em seguida, aplicaremos a função e vamos plotar um mapa para avaliar as ocorrências com flag de outlier.

Função para classificar ocorrencias suspeitas
```{r}
flag_outlier <- function(df, species){
 
 
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
```

Classificar ocorrências
```{r}
marcados <- dori_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., species = "Paracanthurus hepatus (Linnaeus, 1766)")
```  

Mapa
```{r}
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
```

Podemos notar no mapa acima que as ocorrencias acima do 90ésimo quantil são muito similares às já filtradas acima com base no waterBody, mas se já não tivéssemos a informação da ocorrência restrita da espécie ao Indo-Pacífico, já poderíamos desconfiar destas ocorrências tão longe, os outliers. Investigando o datasetName destas ocorrências com flags também chegaríamos a mesma conclusão de excluir os dados associados ao Diveboard - Scuba diving citizen science e sem valor de datasetName.

Por fim, vamos testar o pacote CoordinateCleaner. Nele devemos especificar os campos correspondentes na função clean_coordinates.
```{r}
flags <-
  clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas")
  )
``` 

Verificação de outliers:
```{r}
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
```
