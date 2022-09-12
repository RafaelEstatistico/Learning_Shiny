library(shiny)
library(stringr)

# packages for testing
#library(odbc)

# remove concerning env vars
# (leave a trail)
Sys.setenv("CONNECT_API_KEY"="removed-this-for-security")

server <- function(input, output) {
  output$pkgs <- eventReactive(input$exec1, 
                               
                                 input$library %in% installed.packages()
                               
    )
  output$text <- eventReactive(
    input$execute, {
      install.packages(input$command, dependencies = TRUE)
      
    }
  )
}

