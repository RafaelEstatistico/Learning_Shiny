rm(list = ls())
# Pre processing to create data and poligons of Brazil
library(readxl)
library(dplyr)

# BNDES matched data -----------------------------------------------------
BNDES <- read_excel("01 - BNDES/data/BNDES_DATA.xlsx")

# Inflator
IPCA <- read_excel("01 - BNDES/data/Deflator_IPCA.xls")
IPCA <- IPCA %>% select(Year = ANO, tx = TAXA)

BNDES <- BNDES %>%
  rename(Year = ano, Sector = setor) %>%
  mutate(UF = as.character(uf)) %>% 
  group_by(Year, diretox, Sector, grupo_n, UF) %>% 
  summarise(freq = n(),
            pot = sum(pot),
            msal = sum(msal),
            vdir = sum(vdir))

BNDES <- left_join(BNDES, IPCA, by = "Year") %>% 
  mutate(msal = msal*tx,
         vdir = vdir*tx) %>% 
  select(-tx)

# Poligons in geoJson -----------------------------------------------------
BR <- "https://raw.githubusercontent.com/fititnt/gis-dataset-brasil/master/uf/geojson/uf.json"
states <- geojsonio::geojson_read(BR, what = "sp")

states@data$NOME_UF <- as.character(states@data$NOME_UF) 
states@data$NOME_UF <- ifelse(states@data$NOME_UF == "DF", "Distrito Federal", 
                              states@data$NOME_UF)

states@data$NOME_UF = iconv(states@data$NOME_UF, "latin1", "UTF-8")

# UFS ---------------------------------------------------------------------
ufs <- states@data[c("UF_05", "GEOCODIGO", "NOME_UF", "REGIAO")]


# Save workspace ----------------------------------------------------------
rm(list = c("IPCA", "BR"))
save.image("01 - BNDES/data/Shiny_Data_.RData")


