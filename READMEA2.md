# Dados formatados em Darwin Core – Atividade 2 (Ciência Aberta)

Este repositório contém os dados gerados na Atividade 2 da disciplina de Ciência Aberta, utilizando o padrão **Darwin Core (DwC)** para estruturação de dados de biodiversidade.

## Arquivos incluídos

### eventCore.csv
Contém informações sobre os eventos de coleta (amostragem), incluindo:
- eventID: identificador único do evento (local + data)
- eventDate: data da coleta
- decimalLatitude, decimalLongitude: coordenadas geográficas
- samplingProtocol, locality: descrição do local e método
- Outros campos de georreferência (datum, sistema, protocolo)

### occurrences.csv
Contém as ocorrências biológicas observadas em cada evento:
- occurrenceID: identificador único da ocorrência
- eventID: vínculo com o evento correspondente
- scientificName, taxonRank: identificação taxonômica
- recordedBy, basisOfRecord: metadados do registro
- acceptedNameUsageID: nome 

### eMOF.csv
Planilha de extensão no formato **MeasurementOrFact**, contendo as medidas associadas às ocorrências:
- occurrenceID: referência à ocorrência
- measurementType: tipo de medida (ex: sepal length)
- measurementValue: valor da medida
- measurementUnit: unidade cm
- recordedBy: quem coletou a informação

## Dados utilizados

Os dados são baseados no dataset `iris` (modificado para fins didáticos), contendo registros fictícios de plantas do gênero *Iris*, com campos adicionados para simular um cenário real de coleta.

## Pacotes e ferramentas utilizados:

- vegan; tidyverse; dplyr
- validate; taxadb -> para controle de qualidade e validação taxonômica
- RStudio como ambiente de desenvolvimento

---

Todos os arquivos estão organizados na pasta `DwC_Files/` gerada durante o processamento no R.
