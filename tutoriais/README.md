Ol� Bianca
===

Aqui eu pretendo deixar mais claro colocando um passo-a-passo de como criar um Shiny com cartograma e tabela din�mica. Um exemplo pode ser visto [AQUI](https://rafa-est.shinyapps.io/01-BNDES/).

## Pacotes Necess�rios:
+ shiny 		(claro n�)
+ shinydashboard 	(facilita para criar o template da p�gina)
+ leaflet		(Ser� usado para criar os Cartograms)
+ DT			(Permite criar tabelas din�micas: Data Tables)
+ dplyr 		(Manipula��o de Dados)
+ tidyr 		(Manipula��o de Dados)
+ rgdal			(Manipula��o de Dados Geoespaciais)
+ shinythemes		(Mais personaliza��o para o Shiny ����)

Para checar se tem tudo instalado voc� pode rodar 
���
list.packages <- c("shiny", "shinydashboard", "leaflet", "DT", "dplyr", "rgdal", "shinythemes", "tidyr", "geojsonio")
new.packages <- list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
���

===

## Dados e Programas
Estou aproveitando uma base de dados que extrai do portal transpar�ncia do BNDES.

Para o mapa baixamos o arquivo no formato geoJason. Vou deixar o github de onde � possivel pra baixar v�rios shapefiles de geoJson:
+ https://github.com/fititnt/gis-dataset-brasil

Vou deixar os programas do Shiny organizados em 3:
+ global.R	(em geral, serve para o pr�-processamento: carregar pacotes, bases, etc...)
+ ui.R		("User Interface", aqui � onde voc� desenha a p�gina, isto �, layout, bot�es e etc.)
+ server.R	(O server � respons�vel por tornar a p�gina interativa, ele vai responder de acordo com o uso da p�gina web.)

No mais � isso... Vou tentar deixar o c�digo bem organizado e comentado! 
Espero que isso ajude e qualquer coisa � s� me chamar :)
-----
