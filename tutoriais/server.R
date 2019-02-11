shinyServer(function(input, output, session) {
  
  # server <- function(input, output){
    options(warn =-1) # No warnings!

    
    # Cria botoes dos anos
    output$yearsel <- renderUI({
      anos <- c("Todos", sort(unique(BNDES$ano))) # possiveis escolhas de anos
      selectInput("Year", "Ano", choices = anos, selected = anos[1])
    })
    
    # CNAE subdivision button
    output$sectorsel <- renderUI({
      sectors <- c("Todos", unique(BNDES$CNAE))
      selectInput("Sector", " Divisao CNAE", choices = sectors, 
                  selected = sectors[1])
    })

# Criando Base usada no Cartograma ----------------------------------------
# A graca do Shiny e ter outputs interativos, isto e, dinamicos. Para isso,
# os objetos que variam de acordo com os botoes sao "reativos".
# Abaixo temos uma funcao que usara os botoes escolhidos para gerar a base
# que usaremos no Shiny.
    data_sel <- reactive({
      shiny::validate(
        need(input$Year, " "),   # So roda se tiver valor de ano
        need(input$Sector, " ")  # idem. isso preve msgs de erro...
      )
      # Filtro de Ano
      if(input$Year == "Todos"){
        data <- BNDES %>% 
          group_by(UF,Cod_UF, CNAE,U_F) %>% 
          summarise(n_contrato  = sum(n_contrato, na.rm = T),
                    val_tot = sum(val_tot, na.rm = T),
                    val_med = val_tot / n_contrato)
      } else data <- BNDES %>% filter(ano == input$Year)
      
      # DT_data <- left_join(data, ufs, by = c("UF"="GEOCODIGO")) # Para Data Table
      
      # Sector
      if(input$Sector == "Todos"){
        data <- data %>% group_by(UF,Cod_UF,U_F) %>% 
          summarise(n_contrato  = sum(n_contrato, na.rm = T),
                    val_tot = sum(val_tot, na.rm = T),
                    val_med = val_tot / n_contrato)
        } else{
          data <- data %>% 
            filter(CNAE == input$Sector)
          # DT_data <- data %>% filter(grupo_n == input$Sector)
      }
      
      # DT_data$n_contrato <- round(DT_data$n_contrato)
      # DT_data$val_tot <- round(DT_data$val_tot,2)
      # DT_data$val_med <- round(DT_data$val_tot,2)
      # 
      data <- data %>% data.frame()
      data["meas"] <- round(data[input$meas][,1], 2)
      # data["meas"] <- round(data["n_contrato"][,1], 2)
      data$n_contrato <- round(data$n_contrato)
      data$val_tot <- round(data$val_tot,2)
      data$val_med <- round(data$val_med,2)
      
      DT_data <- data
      # Finalmente mergea a malha com os dados filtrados
      data <- merge(states, data, by.x = "GEOCODIGO", by.y = "Cod_UF")
      
      data$meas[is.na(data$meas)] <- 0
      
      list(data, DT_data)
    })
    
# Mapa --------------------------------------------------------------------
    output$map <-  renderLeaflet({
      measu <- c("Número de Contratos: ", "Valor Total Contratado: R$",
                 "Valor Contratado Médio: R$")
      
      data <- data_sel()[[1]] # pega o primeiro elemento da lista criada
      
      pal <- colorNumeric("Blues", data$meas) # escala de cor
      # esses labels são justamente os pop-up que vao aparecer
      labels <- sprintf(paste0("<strong>Estado: </strong>",
                               data$U_F,
                               paste0("<br><strong>", measu[1],"</strong>"),
                               data$n_contrato, 
                               paste0("<br><strong>", measu[2],"</strong>"),
                               data$val_tot, 
                               paste0("<br><strong>", measu[3],"</strong>"),
                               data$val_med)) %>% 
        lapply(htmltools::HTML)
      # map
      m <- leaflet(data = data) %>% 
        setView(-54, -16, zoom = 3) %>% # fixa onde o mapa inicia
        addTiles(options = providerTileOptions(minZoom = 3, maxZoom = 8)) %>% # fixa o maximo e min de zoom
        addPolygons(weight = 2, opacity = 1, 
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
      # dataSet <- data_sel()[[2]]
      dataSet <- BNDES
      dataSet <- dataSet[c("ano", "U_F", "CNAE", "n_contrato","val_tot","val_med")]

      names(dataSet) <- c("Ano", "Estado", "Setor CNAE", "Total de Contratos",
                          "Valor Total", "Valor Médio")
      dataSet
    }, rownames = F, 
    options = list(search = list(regex = TRUE, caseInsensitive = FALSE)))
    )
    
  })