

load("./data/Shiny_Data.RData")

BNDE <- BNDES %>% group_by(UF, Sector, Year) %>% summarise(freq  = sum(freq),
                                                           value = sum(value)/1000000)
#         
# unique(BNDE$Setor)
# class(BNDE$Setor)
