
ui <- fluidPage( 
  theme = shinytheme("flatly"), # Tema azuzinho bonitinho
  h1("BNDES - Exemplo de Shiny", # Titulo
     style = "font-family:'Source Sans Pro';text-align: center"), # Estilo do Titulo
  fluidRow( # fluidRow cria uma area onde iremos inserir os paineis e butoes da pagina
    column(width = 12, offset = 0, # width representa a largura dessa coluna
#   OBS. para o Shiny imagine que a pagina web tem 12 unidades de largura, logo
# quando eu digo que a coluna tem 12 de largura (width) entenda que ela vai 
# pegar toda a pagina, isso com 0 de recuo (offset).
# Uma outra possivilidade seria eu criar duas colunas cada uma com 6 de largura
# (ou 8 e 4, 9 e 3, etc. O importante é somar 12!).
          h4("Um ponto de partida para um Shiny com Cartograma",
             style = "font-family: 'Source Sans Pro';text-align: center")
          )
          ),
  navbarPage(title = "Navigation Bar", # cada navbar corresponde a uma aba
# Tab 01 - Cartogram ------------------------------------------------------
            tabPanel("Cartogram", icon = icon("map"), # nome do painel com um incone bonitinho
                     fluidRow(
                       h5(strong(p(style="color:black",
                                   "Você pode escrever um subtitulo aqui"))),
                          style = "font-family: 'Source Sans Pro';text-align: center"),
# Cria a coluna de largura 4 onde crio o wellPanel (tipo um caixa) para colocar meus botoes
                     column(width = 4, offset = 0,
                            wellPanel(width=NULL, 
                                      uiOutput("yearsel"),   # botao de ano
                                      uiOutput("sectorsel"), # botao de setor
                                      radioButtons("meas", "Measure", # botaode medida
                                                   c("Total de Contratos" = "n_contrato",
                                                     "Valor Total" = "val_tot",
                                                     "Valor Médio" = "val_med"))
                                      )
                              ),
                      column(width = 8, offset = 0, # repara no width. 4+8=12 :)
# coluna onde crio o box para receber o mapa
                             box(width = NULL, solidHeader = TRUE,
                                 leafletOutput("map", height=400) # objeto que vem do server
                                 )
                              ),
                      column(width = 12, offset = 0, # coluna que pega toda a pag
                             box(width=NULL ,
                                 dataTableOutput("datatable") # tabela dinamica
                                 )
                              )
                          ),
# Tab 02 - Time Series ----------------------------------------------------
            tabPanel("Outros Paineis...", icon = icon("line-chart")
                     ) ,

            tabPanel("Sobre", icon = icon("question-circle"),
                     column(width = 11, offset = 1,
                            wellPanel(
HTML("<img src='Rafael.jpg'; width='110' height='120' style='padding: 5px; float: left'>
<p><strong>Statistician</strong>
<br>E-mail:<a href='mailto:anajulia.padula@gmail.com'>rafaelestatistico@gmail.com</a></p>
<p>Graduated in Statistics at University of Brasília (2017). Undertaking a Master's 
degree in Statistics in Finance and Quantitative Methods at University of Brasília.
Currently is a fellow researcher at the Institute for Applied Economic Research. 
Has experience in classification and regression models, data manipulation of large 
datasets and interactive interfaces (Shiny). Has interests in machine learning, 
finance, statistical modeling and probability.
</p>" )
                                        )
                              )
                      )
        ),
HTML("<script>var parent = document.getElementsByClassName('navbar-nav'); parent[0].insertAdjacentHTML(
     'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"\"><a href=\"https://linkedin.com/in/RafaelLimaMorais/\"><i class=\"fa fa-linkedin\" aria-hidden=\"true\"></i></a></li><li class=\"\"><a href=\"https://github.com/RafaelEstatistico/Learning_Shiny/\"><i class=\"fa fa-github\" aria-hidden=\"true\"></i></a></li></ul>' );</script>")

)






