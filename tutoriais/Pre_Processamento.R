rm(list = ls())
# Pre processing to create data and poligons of Brazil
library(readxl)  # le excel
library(dplyr)   # manipulacao
library(rgdal)   # shp

# Prepara Base do BNDES ---------------------------------------------------
BNDES <- read_excel("tutoriais/dados/BNDES.xlsx", skip = 3) 

# Agrupando por UF, Ano e Grande grupo da CNAE
BNDES <- BNDES %>%
  mutate(ano = lubridate::year(`Data da Contratação`)) %>% 
  group_by(UF, ano, CNAE = `Setor CNAE`) %>% 
  summarise(n_contrato = n(),
            val_tot = sum(`Valor Contratado  R$`),
            val_med = val_tot / n_contrato)

cod_uf <- read.table("tutoriais/Cod_UF.txt", sep = "\t", header = T)
BNDES <- left_join(BNDES, cod_uf)  

# Prepara shapefile -------------------------------------------------------
# Poligons in geoJson -----------------------------------------------------
BR <- "https://raw.githubusercontent.com/fititnt/gis-dataset-brasil/master/uf/geojson/uf.json"
states <- geojsonio::geojson_read(BR, what = "sp")

states@data$NOME_UF <- as.character(states@data$NOME_UF) 
states@data$NOME_UF <- ifelse(states@data$NOME_UF == "DF", "Distrito Federal", 
                              states@data$NOME_UF)
# Salva workspace ---------------------------------------------------------
rm(cod_uf)
save.image("tutoriais/dados/Dados_Shiny.RData")


