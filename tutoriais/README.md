Olá Bianca
===

Aqui eu pretendo deixar mais claro colocando um passo-a-passo de como criar um Shiny com cartograma e tabela dinâmica. Um exemplo pode ser visto [AQUI](https://rafa-est.shinyapps.io/01-BNDES/).

## Pacotes Necessários:
+ shiny 		(claro né)
+ shinydashboard 	(facilita para criar o template da página)
+ leaflet		(Será usado para criar os Cartograms)
+ DT			(Permite criar tabelas dinâmicas: Data Tables)
+ dplyr 		(Manipulação de Dados)
+ tidyr 		(Manipulação de Dados)
+ rgdal			(Manipulação de Dados Geoespaciais)
+ shinythemes		(Mais personalização para o Shiny éééé)

Para checar se tem tudo instalado você pode rodar :


```
list.packages <- c("shiny", "shinydashboard", "leaflet", "DT", "dplyr", "rgdal", "shinythemes", "tidyr","rmapshaper")
new.packages <- list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

Se você tiver os pacotes pode tentar rodar esse Shiny usando o comando:
```
shiny::runGitHub(repo = "Learning_Shiny", username = "RafaelEstatistico", 
                 subdir = "tutorial")
```

## Dados e Programas
Estou aproveitando uma base de dados que extrai do portal transparência do BNDES.

Para o mapa baixamos o arquivo no formato geoJason. Vou deixar o github de onde é possivel pra baixar vários shapefiles de geoJson:
+ https://github.com/fititnt/gis-dataset-brasil

Vou deixar os programas do Shiny organizados em 3:
+ global.R	(em geral, serve para o pré-processamento: carregar pacotes, bases, etc...)
+ ui.R		("User Interface", aqui é onde você desenha a página, isto é, layout, botões e etc.)
+ server.R	(O server é responsável por tornar a página interativa, ele vai responder de acordo com o uso da página web.)

No mais é isso... Vou tentar deixar o código bem organizado e comentado! 
Espero que isso ajude e qualquer coisa é só me chamar :)
-----
