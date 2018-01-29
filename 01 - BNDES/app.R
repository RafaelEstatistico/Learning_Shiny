library(shiny)
library(shinydashboard)
library(leaflet)                 # Cartograms
library(DT)                      # Data Tables
library(dplyr)
library(rgdal)
library(shinythemes)             # Custumize Shiny 
library(plotly)                  # Interactive graphs 
library(highcharter)             # Chart wiht drilldown
library(tidyr)                   # For Spread


setwd("C:/Users/rafal/Google Drive/GitHub/Learning_Shiny/01 - BNDES")


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
                                                          "Total Value" = "value",
                                                          "Mean Value" = "mean_val")) )
                          ) 
                          , 
                          column(width = 8, offset = 0,
                                 box(width = NULL , solidHeader = TRUE,
                                     leafletOutput("map", height=400)
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
                  fluidRow(
                    column(width = 4, offset = 0,
                           wellPanel(width=NULL, 
                                     uiOutput("uf1"),
                                     radioButtons("meas1", "Measure", 
                                                  c("Frequency" = "freq",
                                                    "Total Value" = "value",
                                                    "Mean Value" = "mean_val")) )
                    ) 
                    , 
                    column(width = 8, offset = 0,
                           box(width = NULL , solidHeader = TRUE,
                               plotlyOutput("plotTS")
                           )
                    ),
                    column(width = 12, offset = 0,
                           box(width=NULL ,
                               dataTableOutput("datatable1")
                           )
                    )
                    
         )
         ),
   
# Tab 03 - Pie Chart ------------------------------------------------------
                            tabPanel("Pie Chart", icon = icon("pie-chart"),
                                     
                                     fluidRow(
                                       column(width = 4, offset = 0,
                                              wellPanel(width=NULL, 
                                                        uiOutput("yearsel2"),
                                                        uiOutput("uf"),
                                                        radioButtons("meas2", "Measure", 
                                                                     c("Frequency" = "freq",
                                                                       "Total Value" = "value",
                                                                       "Mean Value" = "mean_val")) )
                                              
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
                                     ) ),
                            tabPanel("About", icon = icon("question-circle")
                                     
                                     # fluidRow(h5("Em Breve...."),
                                     #          column(width = 12, offset = 0,
                                     #                 box(width=NULL #,
                                     #                     # highchartOutput("highchart")
                                     #                 )
                                     #          )
                                     # )
                                     )
                            
                            
                 ),
                 
# Incone ao final da barra -> mudar endereco de link             
HTML("<script>var parent = document.getElementsByClassName('navbar-nav'); parent[0].insertAdjacentHTML(
     'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"\"><a href=\"https://linkedin.com/in/RafaelLimaMorais/\"><i class=\"fa fa-linkedin\" aria-hidden=\"true\"></i></a></li><li class=\"\"><a href=\"https://github.com/RafaelEstatistico/Learning_Shiny/\"><i class=\"fa fa-github\" aria-hidden=\"true\"></i></a></li></ul>' );</script>")


#                   )
                 # navbarMenu("More",
                 #            tabPanel("Sub-Component A"),
                 #            tabPanel("Sub-Component B"))
                 
                 
                 
                 )


server <- function(input, output){
  options(warn =-1) # No warnings!

  load("./data/Shiny_Data.RData")
  BNDE <- BNDES %>% group_by(UF, Sector, Year) %>% 
          summarise(freq  = sum(freq, na.rm = T),
                    value = sum(value, na.rm = T)/1000000,
                    mean_val = mean(value, na.rm = T)/1000000)

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
  output$uf1 <- renderUI({
    uf <- c("All", sort(ufs[,3]))
    selectInput("uf1", "Unidade Federativa", choices = uf, selected=uf[1])
  })
  
  output$uf <- renderUI({
    uf <- c("All", sort(ufs[,3]))
    selectInput("uf", "Unidade Federativa", choices = uf, selected=uf[1])
  })

  # Map Filter --------------------------------------------------------------
  data_sel <- reactive({
    validate(
      need(input$Year, " "),
      need(input$Sector, " ")
    )
    # Year filter
    if(input$Year == "All"){
      data <- BNDE %>% group_by(UF, Sector) %>% 
              summarise(freq = sum(freq, na.rm = T),
                        value = sum(value, na.rm = T),
                        mean_val = sum(value/freq, na.rm = T))
    } else data <- BNDE %>% filter(Year == input$Year)
    
    # Sector
    if(input$Sector == "All"){
      data <- data %>% group_by(UF) %>% summarise(freq = sum(freq, na.rm = T),
                                                  value = sum(value, na.rm = T),
                                                  mean_val = sum(value/freq, na.rm = T))
    } else data <- data %>% filter(Sector == input$Sector) %>% group_by(UF) 

    data <- data %>% data.frame()
    data["meas"] <- round(data[input$meas][,1], 2)
    
    # Merge with poligons for map
    data <-  merge(states, data, by.y = "UF", by.x = "UF_05")
    
    data$meas[is.na(data$meas)] <- 0
    
    data
  })

  # Map ---------------------------------------------------------------------
  output$map <-  renderLeaflet({
    measu <- sort(c("Frequency: ", "Value: R$"), decreasing = input$meas != "freq")
    measu2 <- sort(c("", "<strong> Milions</strong>"), decreasing = input$meas != "freq")
    
    data <- data_sel()
    
    pal <- colorNumeric("Blues", data$meas)
    labels <- sprintf(paste0("<strong>Estado: </strong>",
                             data$NOME_UF,
                             paste0("<br><strong>", measu[1], "</strong>"),
                             data$meas, measu2[1])) %>% lapply(htmltools::HTML)
    # map
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

  # data table map ----------------------------------------------------------
  output$datatable <- renderDataTable(datatable({
    dataSet <- data_sel()@data
    dataSet <- dataSet[c("NOME_UF", "freq", "value", "mean_val")] 
    dataSet[,2:4] <- round(dataSet[,2:4], 3)
    names(dataSet) <- c("State", "Frequency", "Total Value (Milions R$)",
                        "Mean Value (Milions R$)")
    dataSet
  }, rownames = F, 
  options = list(lengthMenu = c(5, 10, 27), pageLength = 5, searching = F) )
  )


  # Filter Time Series ------------------------------------------------------
  data_sel1 <- reactive({
    validate(
      need(input$uf1, " ")
    )
    # UF
    UF1 <- ufs[which(ufs$NOME_UF == input$uf1), 1]
    
    if(input$uf1 == "All"){
      data1 <- BNDES %>% ungroup() %>% group_by(Year, Sector) %>%
              summarise(freq = sum(freq, na.rm = T),
                        value = sum(value, na.rm = T),
                        mean_val = sum(value/freq, na.rm = T)) %>%
              select(Year, Sector, input$meas1) %>% 
              spread(Sector, input$meas1) %>% as.data.frame()
    } else  data1 <- BNDES %>% ungroup() %>% 
                    group_by(UF, Year, Sector) %>%
                    summarise(freq = sum(freq, na.rm = T),
                              value = sum(value, na.rm = T),
                              mean_val = sum(value/freq, na.rm = T)) %>%
                    select(UF, Year, Sector, input$meas1) %>% 
                    spread(Sector, input$meas1) %>% filter(UF == UF1) %>% 
                    as.data.frame()
    
    data1[is.na(data1)] <- 0
    
    data1
  })

  # Time Series -------------------------------------------------------------
  output$plotTS <- renderPlotly({

    data1 <- data_sel1()
      p <- plot_ly(data1, x = ~Year, y =~`AGROPECUÁRIA E PESCA`, 
                   name = "Agropecu\u00E1ria e Pesca",
                   type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = ~`COMERCIO E SERVICOS`, 
                  name = "Com\u00E9rcio e Servi\u00E7os") %>%
        add_trace(y = ~`INDUSTRIA DE TRANSFORMAÇÃO`, 
                  name = "Ind\u00FAstria de Transforma\u00E7\u00E3o") %>%
        add_trace(y = ~`INDUSTRIA EXTRATIVA`, 
                  name = "Ind\u00FAstria Extrativa") %>%
        layout( legend = list(orientation = 'h'), xaxis = list(title = ""),
                yaxis = list(title = ""), hovermode = "compare")
      p
  })
  
  # Data Table Time Series --------------------------------------------------
  output$datatable1 <- renderDataTable(datatable({
    dataSet <- data_sel1()[c("Year", "AGROPECUÁRIA E PESCA", "COMERCIO E SERVICOS",
                             "INDUSTRIA DE TRANSFORMAÇÃO", "INDUSTRIA EXTRATIVA")]
    dataSet[, 2:5] <- round(dataSet[ ,2:5], 3)
    names(dataSet) <- c("Ano", "Agropecu\u00E1ria e Pesca", 
                        "Com\u00E9rcio e Servi\u00E7os",
                        "Ind\u00FAstria de Transforma\u00E7\u00E3o",
                        "Ind\u00FAstria Extrativa")
    dataSet
  }, rownames = F,
  options = list( dom = "t", pageLength = 16) )
  )  
  



  # Filter Pie Chart --------------------------------------------------------
  data_sel2 <- reactive({
    validate(
      need(input$Year2, " "),
      need(input$uf, " ")
    )
    # Year filter
    if(input$Year2 == "All"){
      data2 <- BNDE %>% group_by(UF, Sector) %>% 
               summarise(freq = sum(freq, na.rm = T),
                         value = sum(value, na.rm = T),
                         mean_val = sum(value/freq, na.rm = T))
    } else data2 <- BNDE %>% filter(Year == input$Year2)
    
    # UF
    UF <- ufs[which(ufs$NOME_UF == input$uf), 1]
    if(input$uf == "All"){
      data2 <- data2 %>% group_by(Sector) %>% summarise(freq = sum(freq, na.rm = T),
                                                        value = sum(value, na.rm = T),
                                                        mean_val = sum(value/freq, na.rm = T))
    } else data2 <- data2 %>% filter(UF == UF) %>% group_by(Sector) %>%
                    summarise(freq = sum(freq, na.rm = T),
                              value = sum(value, na.rm = T),
                              mean_val = sum(value/freq, na.rm = T)) 
    
    data2 <- data2 %>% data.frame()
    data2["meas"] <- data2[input$meas2][,1]
    data2
  })
  
  # Grafico de setor --------------------------------------------------------
  output$plot <- renderPlotly({
    data2 <- data_sel2()
    if (nrow(data2) > 0) {
      p <- plot_ly(data2, labels = ~Sector, values = ~meas, type = 'pie') %>%
        layout(showlegend = T)
      p
    }  else {
      stop(safeError(paste(input$uf, 'n\u00E3o possui datas') )) }
    
  })
  
  
  # Data table 2 ------------------------------------------------------------
  output$datatable2 <- renderDataTable(datatable({
    dataSet <- data_sel2()[,1:4]
    dataSet[,2:4] <- round(dataSet[,2:4], 3)
    names(dataSet) <- c("Sector CNAE", "Frequency", "Total Value (Milions R$)",
                        "Mean Value (Milions R$)")
    dataSet
  }, rownames = F,
  options = list(lengthMenu = c(5, 10, 27), pageLength = 5, searching = F) )
  )  

  # HighChart - DrillDown ---------------------------------------------------
  output$highchart <- renderHighchart({
    
    data <- merge(data[,1:2], ufs, by.x = "CO_UF", by.y = "GEOCODIGO")
    
    
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
