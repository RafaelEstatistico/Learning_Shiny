library(shiny)
library(shinydashboard)
library(leaflet)                 # Cartograms
library(DT)                      # Data Tables
library(dplyr)                   # Data manipulation
library(rgdal)                   # Provides bindings to the 'Geospatial' 
library(shinythemes)             # Custumize Shiny 
library(plotly)                  # Interactive graphs 
library(highcharter)             # Chart wiht drilldown
library(tidyr)                   # For Spread

# list.packages <- c("shiny", "shinydashboard", "leaflet", "DT", "dplyr", "rgdal", "shinythemes", "plotly",
#                       "highcharter", "tidyr", "rmarkdown", "htmltools")
# new.packages <- list.packages[!(list.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

ui <- fluidPage( theme = shinytheme("flatly"),
                 h1("BNDES - Matched Data", 
                    style = "font-family: 'Source Sans Pro';
                    text-align: center"),
                 
                 fluidRow(
                   column(12, offset = 0,
                          h4("An Exploratory study on matched data driven by BNDES and RAIS databases", 
                             style = "font-family: 'Source Sans Pro';
                             text-align: center")
                          )
                   ),
                 
                 navbarPage(title = "Navigation Bar",
                            
                            # Tab 01 - Cartogram ------------------------------------------------------
                            tabPanel("Cartogram", icon = icon("map"),
                                     fluidRow(
h5(strong(p(style="color:black",
       "These data are for firms who didn't access BNDES financial and were paired with firms who did.")))
,                                          style = "font-family: 'Source Sans Pro';
                                          text-align: center"),
                                       column(width = 4, offset = 0,
                                              wellPanel(width=NULL, 
                                                        uiOutput("yearsel"),
                                                        uiOutput("sectorsel"),
                                                        radioButtons("meas", "Measure", 
                                                                     c("Number of Firms" = "freq",
                                                                       "Employees" = "pot",
                                                                       "Average Wage" = "rend_med"))
                                                        ),
h5(span(style="color:red", "*Click on some State for more information."))
                                      ) 
                                       , 
                                       column(width = 8, offset = 0,
                                              box(width = NULL , solidHeader = TRUE,
                                                  leafletOutput("map", height=400)
                                              )
                                       ),
                                       column(width = 12, offset = 0,
                                              box(width=NULL ,
                                                  dataTableOutput("datatable")
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
                                         c("Number of Firms" = "freq",
                                           "Employees" = "pot",
                                           "Average Wage" = "rend_med",
                                           "Financied Value (Milions R$)" = "vdir")),
                            radioButtons("dir1", "Group", 
                                         c("Non Financed Firms" = "0",
                                           "Financed Firms" = "1"))
                  )
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
                                         c("Number of Firms" = "freq",
                                           "Employees" = "pot",
                                           "Average Wage" = "rend_med",
                                           "Financied Value (Milions R$)" = "vdir")),
                            radioButtons("dir2", "Group", 
                                         c("Non Financed Firms" = "0",
                                           "Financed Firms" = "1")) 
                            )       
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

# HighChart ---------------------------------------------------------------


tabPanel("Bar Charts", icon = icon("bar-chart"),
         fluidRow(
           column(width = 4, offset = 0,
                  wellPanel(uiOutput("yearsel3")
                  )
           ),
           column(width = 4, offset = 0,
                  wellPanel(
                    radioButtons("dir3", "Group",  inline = T,
                        c("Non Financed Firms" = "0",
                          "Financed Firms" = "1"))
                    )
                  ),
           column(width = 4, offset = 0,
                  wellPanel(
                    radioButtons("meas3", "Measure", inline = T,
                                 c("Number of Firms" = "freq",
                                   "Employees" = "pot",
                                   "Average Wage" = "rend_med",
                                   "Financied Value (Milions R$)" = "vdir"))  )
                  )
           ),
           column(width = 12,
                  highchartOutput("highchart")
                  
           )
         ) ,


tabPanel("About", icon = icon("question-circle"),
         column(width = 11, offset = 1,
                wellPanel(
                  
                  
                  HTML("
                       <img src='Rafael.jpg'; width='110' height='120' style='padding: 5px; float: left'>
                       <p><strong>Statistician</strong>
                       <br>E-mail:
                       <a href='mailto:anajulia.padula@gmail.com'>
                       rafaelestatistico@gmail.com</a></p>
                       <p>
                       Graduated in Statistics at University of Brasília (2017). Undertaking a Master's 
                       degree in Statistics in Finance and Quantitative Methods at University of Brasília. 
                       Currently is a fellow researcher at the Institute for Applied Economic Research. 
                       Has experience in classification and regression models, data manipulation of large 
                       datasets and interactive interfaces (Shiny). Has interests in machine learning, 
                       finance, statistical modeling and probability.
                       </p>"
                  )
                  
                  )
                )
                )

                ),


HTML("<script>var parent = document.getElementsByClassName('navbar-nav'); parent[0].insertAdjacentHTML(
     'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"\"><a href=\"https://linkedin.com/in/RafaelLimaMorais/\"><i class=\"fa fa-linkedin\" aria-hidden=\"true\"></i></a></li><li class=\"\"><a href=\"https://github.com/RafaelEstatistico/Learning_Shiny/\"><i class=\"fa fa-github\" aria-hidden=\"true\"></i></a></li></ul>' );</script>")



                )



server <- function(input, output){
  options(warn =-1) # No warnings!
  
  load("data/Shiny_Data_.RData")
  BNDE <- BNDES %>% group_by(UF, diretox, grupo_n, Year) %>% 
    summarise(freq  = sum(freq, na.rm = T),
            pot = sum(pot, na.rm = T),
            msal = sum(msal),
            rend_med = sum(msal)/(sum(pot)*12)) # msal esta anual)
  
  
  # Year inputs
  output$yearsel <- renderUI({
    year <- c("All", sort(unique(BNDE$Year)))
    selectInput("Year", "Year", choices = year, selected = year[1])
  })
  
  output$yearsel2 <- renderUI({
    year <- c("All", sort(unique(BNDE$Year)))
    selectInput("Year2", "Year", choices = year, selected = year[1])
  })
  
  output$yearsel3 <- renderUI({
    year <- c("All", sort(unique(BNDE$Year)))
    selectInput("Year3", "Year", choices = year, selected = year[1])
  })
  
  # CNAE subdivision button
  output$sectorsel <- renderUI({
    sectors <- c("All", unique(BNDE$grupo_n))
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
      data <- BNDE %>% filter(diretox == 0) %>% 
        group_by(UF, grupo_n) %>% 
        summarise(freq  = sum(freq, na.rm = T),
                  pot = sum(pot, na.rm = T),
                  msal = sum(msal),
                  rend_med = sum(msal)/(sum(pot)*12)) # msal esta anual)
    } else data <- BNDE %>% filter(Year == input$Year & diretox == 0)

    DT_data <- left_join(data, ufs, by = c("UF"="GEOCODIGO")) # Para Data Table
    
    # Sector
    if(input$Sector == "All"){
      data <- data %>% group_by(UF) %>% 
        summarise(freq  = sum(freq, na.rm = T),
                  pot = sum(pot, na.rm = T),
                  msal = sum(msal),
                  rend_med = sum(msal)/(sum(pot)*12)) # msal esta anual)
      
    } else{
      data <- data %>% filter(grupo_n == input$Sector) %>% group_by(UF) 
      DT_data <- DT_data %>% filter(grupo_n == input$Sector) %>% group_by(UF) 
      }

    DT_data$freq <- round(DT_data$freq)
    DT_data$pot <- round(DT_data$pot)
    DT_data$rend_med <- round(DT_data$rend_med,2)
    
    data <- data %>% data.frame()
    data["meas"] <- round(data[input$meas][,1], 2)
    
    data$freq <- round(data$freq)
    data$pot <- round(data$pot)
    data$rend_med <- round(data$rend_med,2)

    
    # Merge with poligons for map
    data <- merge(states, data, by.y = "UF", by.x = "GEOCODIGO")
    
    data$meas[is.na(data$meas)] <- 0
    
    list(data, DT_data)
  })
  
  # Map ---------------------------------------------------------------------
  output$map <-  renderLeaflet({
    measu <- c("Number of Firms: ", "Employees: ", "Average Wage: R$")

    data <- data_sel()[[1]]
    
    pal <- colorNumeric("Blues", data$meas)
    labels <- sprintf(paste0("<strong>State: </strong>",
                             data$NOME_UF,
                             paste0("<br><strong>", measu[1],"</strong>"),
                             data$freq, 
                             paste0("<br><strong>", measu[2],"</strong>"),
                             data$pot, 
                             paste0("<br><strong>", measu[3],"</strong>"),
                             data$rend_med)) %>% 
                        lapply(htmltools::HTML)
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
                  layerId = data@data[,"NOME_UF"] ,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend("bottomright", pal = pal, values = ~data$meas, 
                opacity = 0.7, title = NULL)
    
    m
  })
  
  
  clique <- reactive({
    validate( need(input$map_shape_click, " ") )
    input$map_shape_click$id  
  })
  
  # data table map ----------------------------------------------------------
  output$datatable <- renderDataTable(datatable({
    dataSet <- data_sel()[[2]]
    dataSet <- dataSet[c("NOME_UF", "grupo_n", "freq", "pot", "rend_med")] 
    
    if(length(unique(dataSet$grupo_n))>1){
      dataSet = dataSet[dataSet$NOME_UF==clique(),]  
    }

    names(dataSet) <- c("State", "Economic Activity", "Number of Firms", 
                        "Employees", "Average Wage")
    dataSet
  }, rownames = F )
  )
  
  # Filter Time Series ------------------------------------------------------
  data_sel1 <- reactive({
    validate(
      need(input$uf1, " "),
      need(input$dir1, " ")
    )
    # UF
    UF1 <- ufs[which(ufs$NOME_UF == input$uf1), 2]
    
    if(input$uf1 == "All"){
      data1 <- BNDES %>% filter(diretox == input$dir1) %>% 
        ungroup() %>% group_by(Year, Sector) %>%
        summarise(freq  = sum(freq, na.rm = T),
                  pot = sum(pot, na.rm = T),
                  msal = sum(msal),
                  rend_med = sum(msal)/(sum(pot)*12),
                  vdir = sum(vdir)/1000000)  %>%
        select(Year, Sector, input$meas1) %>% 
        spread(Sector, input$meas1) %>% as.data.frame()
    } else  data1 <- BNDES %>% ungroup() %>% 
      group_by(UF, Year, Sector) %>%
      summarise(freq  = sum(freq, na.rm = T),
                pot = sum(pot, na.rm = T),
                msal = sum(msal),
                rend_med = sum(msal)/(sum(pot)*12),
                vdir = sum(vdir)/1000000)  %>%
    select(UF, Year, Sector, input$meas1) %>% 
      spread(Sector, input$meas1) %>% filter(UF == UF1) %>% 
      as.data.frame()
    
    data1[is.na(data1)] <- 0
    
    data1
  })
  
  # Time Series -------------------------------------------------------------
  output$plotTS <- renderPlotly({
    
    data1 <- data_sel1()
    p <- plot_ly(data1, x = ~Year, y =~`Serviços`, 
                 name = "Servi\u00E7os",
                 type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~`Comércio`, 
                name = "Com\u00E9rcio e Servi\u00E7os") %>%
      add_trace(y = ~`Indústria da Construção`, 
                name = "Ind\u00FAstria da Constru\u00E7\u00E3o") %>%
      add_trace(y = ~`Indústria de Transformação`, 
                name = "Ind\u00FAstria da Transforma\u00E7\u00E3o") %>%
      add_trace(y = ~`Indústria Extrativa`, 
                name = "Ind\u00FAstria Extrativa") %>%
      layout( legend = list(orientation = 'h'), xaxis = list(title = ""),
              yaxis = list(title = ""), hovermode = "compare")
    p
  })
  
  
  
  # Data Table Time Series --------------------------------------------------
  output$datatable1 <- renderDataTable(datatable({
    dataSet <- data_sel1()[,-1]

    dataSet <- round(dataSet, digits = 2)
    dataSet
  }, rownames = F,
  options = list( dom = "t", pageLength = 16) )
  )  
  
  
  
  # Filter Pie Chart --------------------------------------------------------
  data_sel2 <- reactive({
    validate(
      need(input$Year2, " "),
      need(input$uf, " "),
      need(input$dir2, " ")
    )
    # Year filter
    if(input$Year2 == "All"){
      data2 <- BNDES %>% filter(diretox == input$dir2) %>% 
        group_by(UF, Sector) %>% 
        summarise(freq  = sum(freq, na.rm = T),
                  pot = sum(pot, na.rm = T),
                  msal = sum(msal),
                  rend_med = sum(msal)/(sum(pot)*12),
                  vdir = sum(vdir)/1000000)
    } else data2 <- BNDES %>% filter(Year == input$Year2)
    
    # UF
    UF_ <- ufs[which(ufs$NOME_UF == input$uf), 2]
    if(input$uf == "All"){
      data2 <- data2 %>% group_by(Sector) %>% 
        summarise(freq  = sum(freq, na.rm = T),
                  pot = sum(pot, na.rm = T),
                  rend_med = sum(msal)/(sum(pot)*12),
                  vdir = sum(vdir)/1000000)
    } else data2 <- data2 %>% filter(UF == UF_) %>% group_by(Sector) %>%
      summarise(freq  = sum(freq, na.rm = T),
                pot = sum(pot, na.rm = T),
                rend_med = sum(msal)/(sum(pot)*12),
                vdir = sum(vdir)/1000000 )
      
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
    dataSet[,-1] <- round(dataSet[,-1], 2)
    names(dataSet) <- c("Economic Activity", "Number of Firms", "Employees",
                        "Average Wage (R$)")
    dataSet
  }, rownames = F,
  options = list(lengthMenu = c(5, 10, 27), pageLength = 5, searching = F) )
  ) 
  
  
  
  # Filter HighCHart --------------------------------------------------------
  data_sel3 <- reactive({
    validate(
      need(input$meas3, " "),
      need(input$Year3, " "),
      need(input$dir3, " ")
    )
    BNDE1 <- BNDES %>% filter(diretox == input$dir3) %>% 
      group_by(UF, grupo_n, Year) %>% 
      summarise(freq  = sum(freq, na.rm = T),
                pot = sum(pot, na.rm = T),
                msal = sum(msal),
                rend_med = sum(msal)/(sum(pot)*12),
                vdir = sum(vdir)/1000000)
    
    datahc <-  merge(BNDE1, ufs, by.x = "UF", by.y = "GEOCODIGO") 
    # Year Filter
    if(input$Year3 !="All") {
      datahc <- filter(datahc, Year == input$Year3)
    }
    datahc["meas3"] <- datahc[input$meas3][,1]
    
    datahc
  })
  
  
  
  # HighChart - DrillDown ---------------------------------------------------
  output$highchart <- renderHighchart({
    
    datahc <- data_sel3()
    
    # First Layer
    if(input$meas3 != "rend_med"){
      data_1 <- datahc %>% select(REGIAO, meas3) %>% rename(name = REGIAO) %>% 
        group_by(name) %>% summarise(y = round(sum(meas3, na.rm = T), 2)) %>%
        mutate(drilldown = name)
    } else{
      data_1 <- datahc %>% select(REGIAO, msal, pot) %>% 
        rename(name = REGIAO) %>% 
        group_by(name) %>% 
        summarise(y = round(sum(msal)/(sum(pot)*12), 2)) %>%
        mutate(drilldown = name)
      
    }
    
    # Second Layer
    if(input$meas3 != "rend_med"){
      data_2 <- datahc %>% select(REGIAO, NOME_UF, meas3) %>% 
        group_by(id = REGIAO, name = NOME_UF) %>%
        summarise(y = round(sum(meas3), 2))  %>% mutate(drilldown = name)
    } else{
      data_2 <- datahc %>% select(REGIAO, NOME_UF, msal, pot) %>% 
        group_by(id = REGIAO, name = NOME_UF) %>%
        summarise(y = round(sum(msal)/(sum(pot)*12), 2))  %>% 
        mutate(drilldown = name)
      
    }
      
    dat_2 <- list()
    j = 1
    for(i in unique(data_2$id)){
      dat_2[[j]] <- list(id = i, data=list.parse3(data_2[which(data_2$id == i), -1]))
      j = j+1
    }
    
    # Third Layer
    if(input$meas3 != "rend_med"){
      data_3 <- datahc %>% select(REGIAO, NOME_UF, grupo_n, meas3) %>% 
        group_by(id = NOME_UF, name = grupo_n) %>%
        summarise(y = round(sum(meas3), 2))
    } else{
      data_3 <- datahc %>% select(REGIAO, NOME_UF, grupo_n, msal, pot) %>% 
        group_by(id = NOME_UF, name = grupo_n) %>%
        summarise(y = round(sum(msal)/(sum(pot)*12), 2))
    }
    
    dat_3 <- list()
    j = 1
    for(i in unique(data_3$id)){
      dat_3[[j]] <- list(id = i, data = list.parse2(data_3[which(data_3$id == i), -1]))
      j = j+1
    }
    
    # HighChart
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Total ") %>%
      hc_xAxis(type = "category") %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_add_series(
        name = input$meas3,
        colorByPoint = TRUE,
        data = data_1
      ) %>% 
      hc_drilldown(
        series =  c(dat_2, dat_3)
      )
    
    hc
    
  }) 

  
}


shinyApp(ui, server)

  
  


