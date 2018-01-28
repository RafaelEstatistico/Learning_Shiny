# Pre processing to create data and poligons of Brazil

# Download and prepare BNDES data  ----------------------------------------
url <- "https://www.bndes.gov.br/wps/wcm/connect/site/f14610e3-b3f9-41f8-92b5-f8ec02f41ca4/BASE_CONTRATA%C3%87%C3%95ES_VSITE+-+20171228.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-f14610e3-b3f9-41f8-92b5-f8ec02f41ca4-lMrQW39"
file <- "./data/BNDES.xlsx"
curl::curl_download(url, file)
BNDES <- readxl::read_excel(file, skip = 3)


library(dplyr)
BNDES <- BNDES %>% mutate(firm = substr(CNPJ, 1, 8)) %>%
         group_by(Sector = `Setor CNAE`, UF, firm, 
                  Year = substr(BNDES$`Data da Contratação`,1,4)) %>% 
         summarise(freq  = n(),
                   value = sum(`Valor Contratado  R$`)) %>%
         filter(UF != "IE")

# Poligons in geoJson -----------------------------------------------------
BR <- "https://raw.githubusercontent.com/fititnt/gis-dataset-brasil/master/uf/geojson/uf.json"
states <- geojsonio::geojson_read(BR, what = "sp")

# UFS ---------------------------------------------------------------------
ufs <- states@data[c("UF_05", "GEOCODIGO", "NOME_UF", "REGIAO")]
ufs[,3] <- as.character(ufs[,3])
ufs[,3] <- ifelse(ufs[,3]=="DF", "Distrito Federal", ufs[,3])


# Save workspace ----------------------------------------------------------
rm(list = c("BR", "file", "url"))
save.image("./data/Shiny_Data.RData")
