foo <- function() {
  message("one")
  message("two")
}

runApp(shinyApp(
  ui = fluidPage(
    actionButton("btn","Click me"),
    textOutput("text")
  ),
  server = function(input,output, session) {
    observeEvent(input$btn, {
      withCallingHandlers(
        foo(),
        message = function(m) output$text <- renderPrint(m$message)
      )
    })
  }
))
list.of.packages <- c("pacman","maptools","dplyr","data.table","reshape2","ggplot2","plyr","rgdal","rgeos","shinyjs","scales","DT","readxl")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)