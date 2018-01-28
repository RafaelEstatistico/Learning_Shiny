library(shiny)
library(shinydashboard)
library(leaflet)                 # Cartograms
library(DT)                      # Data Tables
library(dplyr)
library(rgdal)
library(shinythemes)             # Custumize Shiny 
library(plotly)                  # Interactive graphs 
library(highcharter)             # Chart wiht drilldown


setwd("C:/Users/rafal/Google Drive/GitHub/Learning_Shiny")


ui <- fluidPage( theme = shinytheme("flatly"),
                 h1("Exploring Shiny - BNDES", 
                    style = "font-family: 'Source Sans Pro';
                    text-align: center"),
                 
                 fluidRow(
                   column(12, offset = 0,
                          h4("A Shiny App on open data from Brazilian Development Bank - BNDES", 
                             style = "font-family: 'Source Sans Pro';
                             text-align: center")
                          )
                   ),
                 
                 navbarPage(title = "Navigation Bar",
                            
# Tab 01 - Cartogram ------------------------------------------------------
               tabPanel("Cartogram", icon = icon("map"),
                        fluidRow(
                          column(width = 4, offset = 0,
                                 wellPanel(width=NULL, 
                                           uiOutput("yearsel"),
                                           uiOutput("sectorsel"),
                                           radioButtons("meas", "Measure", 
                                                        c("Frequency" = "freq",
                                                          "Total Value" = "value")) )
                          ) 
                          , 
                          column(width = 8, offset = 0,
                                 box(width = NULL , solidHeader = TRUE,
                                     leafletOutput("mapa", height=400)
                                 )
                          )
                          , 
                          column(width = 12, offset = 0,
                                 box(width=NULL ,
                                     dataTableOutput("datatable")
                                 )
                          )
                        )
               ),
# Tab 02 - Time Series ----------------------------------------------------
         tabPanel("Time Series", icon = icon("line-chart"),
                  h1("he he")
         ),
   
# Tab 03 - Pie Chart ------------------------------------------------------
                            tabPanel("Pie Chart", icon = icon("pie-chart"),
                                     
                                     fluidRow(
                                       column(width = 4, offset = 0,
                                              wellPanel(width=NULL, 
                                                        uiOutput("anosel2"),
                                                        uiOutput("uf"),
                                                        radioButtons("medida2", "Medida", 
                                                                     c("Coeficiente Importa" = "coef_imp",
                                                                       "Coeficiente Exporta" = "coef_exp",
                                                                       "Coeficiente Comercio" = "coef_comercio")) )
                                       ) 
                                       , 
                                       column(width = 8, offset = 0,
                                              box(width = NULL , solidHeader = TRUE,
                                                  plotlyOutput("plot") 
                                              )
                                       )
                                       
                                       , column(width = 12, offset = 0,
                                                box(width=NULL ,
                                                    dataTableOutput("datatable2")
                                                )
                                       )
                                     )
                                     
                            ),
                            
                            
                            
                            
                            
                            
                            tabPanel("Bar Charts", icon = icon("bar-chart"),
                                     
                                     fluidRow(h5("Em Breve...."),
                                              column(width = 12, offset = 0,
                                                     box(width=NULL ,
                                                         highchartOutput("highchart")
                                                     )
                                              )
                                     ) )
                            
                            
                 ),
                 
# Incone ao final da barra -> mudar endereco de link             
HTML("<script>var parent = document.getElementsByClassName('navbar-nav'); parent[0].insertAdjacentHTML(
     'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"\"><a href=\"www.linkedin.com/in/RafaelLimaMorais/\"><i class=\"fa fa-linkedin\" aria-hidden=\"true\"></i></a></li><li class=\"\"><a href=\"https://github.com/RafaelEstatistico/Learning_Shiny/\"><i class=\"fa fa-github\" aria-hidden=\"true\"></i></a></li></ul>' );</script>")


#                   )
                 # navbarMenu("More",
                 #            tabPanel("Sub-Component A"),
                 #            tabPanel("Sub-Component B"))
                 
                 
                 
                 )


server <- function(input, output){
  options(warn =-1) # No warnings!
  # load("./data/Shiny_Data.RData")
  
  # Year inputs
  output$yearsel <- renderUI({
    year <- c("All", sort(unique(BNDE$Year)))
    selectInput("Year", "Year", choices = year, selected = year[1])
  })
  
  output$yearsel2 <- renderUI({
    year <- c("All", sort(unique(BNDE$Year)))
    selectInput("Year2", "Year", choices = year, selected = year[1])
  })
  
  # CNAE subdivision button
  output$sectorsel <- renderUI({
    sectors <- c("All", unique(BNDE$Sector))
    selectInput("Sector", " CNAE subdivision", choices = sectors, selected = sectors[1])
  })
  
  # UF
  output$uf <- renderUI({
    uf <- sort(ufs[,2])
    selectInput("uf", "Unidade Federativa", choices = uf, selected=uf[1])
  })
  
  
  
  # Filter -----------------------------------------------------------------
  data_sel <- reactive({
    validate(
      need(input$Year, " "),
      need(input$Sector, " ")
    )
    # Year filter
    if(input$Year == "All"){
      data <- BNDE %>% group_by(UF, Sector) %>% 
              summarise(freq = sum(freq, na.rm = T),
                        value = sum(value, na.rm = T) )
    } else data <- BNDE %>% filter(Year == input$Year)
    
    # Sector
    if(input$Sector == "All"){
      data <- data %>% group_by(UF) %>% summarise(freq = sum(freq, na.rm = T),
                                                  value = sum(value, na.rm = T) )
    } else data <- data %>% filter(Sector == input$Sector) %>% group_by(UF) 
    # %>% 
    #     summarise(coef_exp = mean(coef_exp, na.rm = T),
    #               coef_imp = mean(coef_imp, na.rm = T),
    #               coef_comercio = mean(coef_comercio, na.rm = T))
    
    data <- data %>% data.frame()
    data["meas"] <- round(data[input$meas][,1], 2)
    
    # Merge with poligons for map
    data <-  merge(states, data, by.y = "UF", by.x = "UF_05")
    
    data$meas[is.na(data$meas)] <- 0
    
    data
  })
  
  
  # Mapa --------------------------------------------------------------------
  output$mapa <-  renderLeaflet({
    measu <- sort(c("Frequency: ", "Value: R$"), decreasing = input$meas != "freq")
    measu2 <- sort(c("", "<strong> Milions</strong>"), decreasing = input$meas != "freq")
    
    # Atualiza dado
    data <- data_sel()
    
    # estados$ESTADO <- toupper(estados$NOME_UF)
    pal <- colorNumeric("Blues", data$meas)
    
    # Caixa de Dialogo                                      ------------------------ Qual deve ser o popup ?????
    labels <- sprintf(paste0("<strong>Estado: </strong>",
                             data$NOME_UF,
                             paste0("<br><strong>", measu[1], "</strong>"),
                             data$meas, measu2[1])) %>% lapply(htmltools::HTML)
    # Mapa
    m <- leaflet(data = data) %>% setView(-54, -16, zoom = 3) %>%
      addTiles(options = providerTileOptions(minZoom = 3, maxZoom = 8)) %>%
      addPolygons(data = states, weight = 2, opacity = 1,
                  color = "white", fillColor = ~pal(data$meas),
                  dashArray = "3", fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5, color = "#666",
                    dashArray = "", fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend("bottomright", pal = pal, values = ~data$meas, 
                opacity = 0.7, title = NULL)
    
    m
  })
  
  # data table  -------------------------------------------------------------
  output$datatable <- renderDataTable(datatable({
    dataSet <- dado_sel()@data
    dataSet <- dataSet[c("NOME_UF", "coef_exp", "coef_imp", "coef_comercio")] 
    dataSet[,2:4] <- round(dataSet[,2:4], 3)
    names(dataSet)<-c("Estados", "Exporta", "Importa", "Comercio") # --------------------- Quais Nomes ???
    dataSet
  }, rownames = F, 
  options = list(lengthMenu = c(5, 10, 27), pageLength = 5, searching = F) )
  )
  
  
  # Filtros 2 ---------------------------------------------------------------
  dado_sel2 <- reactive({
    validate(
      need(input$Year2, " "),
      need(input$uf, " ")
    )
    # Filtros para segunda aba
    if(input$Year2 == "Todos"){
      dado2 <- cade %>% group_by(CO_UF, cnae, divisao) %>% 
        summarise(coef_exp = sum(coef_exp),
                  coef_imp = sum(coef_imp),
                  coef_comercio = sum(coef_comercio))
    } else dado2 <- cade %>% filter(CO_ANO == input$Year2)
    
    # Unidade Feerativa
    UF <- ufs[which(ufs$NOME_UF == input$uf), 1]
    
    dado2 <- dado2 %>% filter(CO_UF == UF) %>% group_by(divisao) %>%
      summarise(coef_exp = sum(coef_exp),
                coef_imp = sum(coef_imp),
                coef_comercio = sum(coef_comercio)) %>% 
      as.data.frame()
    
    dado2["medida"] <- dado2[input$medida2][,1]
    dado2
  })
  
  # Grafico de setor --------------------------------------------------------
  output$plot <- renderPlotly({
    dado2 <- dado_sel2()
    if (nrow(dado2) > 0) {
      p <- plot_ly(dado2, labels = ~divisao, values = ~medida, type = 'pie') 
      # %>%
      #   layout(title = paste0('Composi\u00E7\u00E3o de', input$medida2, ' para ', input$uf)) informa\u00E7\u00F5es
      p
    }  else {
      stop(safeError(paste(input$uf, 'n\u00E3o possui dados') )) }
    
  })
  
  
  # Data table 2 ------------------------------------------------------------
  output$datatable2 <- renderDataTable(datatable({
    dataSet<-dado_sel2()
    # dataSet<-dataSet@data[c("divisao", "coef_exp", "coef_imp", 
    #                         "coef_comercio")] # Just get name and value columns
    names(dataSet)<-c("Divisao", "Exporta", "Importa", "Comercio") 
    dataSet
  },
  options = list(lengthMenu = c(3), pageLength = 3) )
  )  
  
  # HighChart - DrillDown ---------------------------------------------------
  output$highchart <- renderHighchart({
    
    data <- merge(dado[,1:2], ufs, by.x = "CO_UF", by.y = "GEOCODIGO")
    
    
    data.r <- data %>% select(-CO_UF, -NOME_UF) %>% rename(name = REGIAO) %>%
      group_by(name) %>% summarise(y = round(sum(coef_exp, na.rm = T), 2)) %>%
      mutate(drilldown = name)
    
    data.uf <- data %>% select(-CO_UF) %>% mutate(id = REGIAO, 
                                                  y = round(coef_exp, 2),
                                                  name = NOME_UF) %>%
      select(id, name, y)
    
    aa <- list()
    j = 1
    for(i in unique(data.uf$id)){
      aa[[j+1]] <-  list(id = i, data = list.parse2(data.uf[which(data.uf$id==i),-1]))
      j = j+1
    }
    
    data.l <- list.parse3(data.r)
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Brincando") %>%
      hc_xAxis(type = "category") %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_add_series(
        name = "Coef. de Exporta.",
        colorByPoint = TRUE,
        data = data.l
      ) %>% 
      hc_drilldown(
        series = aa
      )
    
    hc
    
  }) 
  
}


shinyApp(ui, server)
