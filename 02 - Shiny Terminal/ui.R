
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny Shell!"),
  
  sidebarLayout(
    sidebarPanel = NULL,
    mainPanel = mainPanel(
      "Check if a library is installed"
      , textInput('library', label='Enter library')
      , actionButton('exec1', label='Run!')
      , verbatimTextOutput("pkgs")
      , textInput('command', label='Enter Command')
      , actionButton('execute', label='Run!')
      , verbatimTextOutput("text")
    )
  )
)
