01 - BNDES
===

Shiny study based on [open data](https://www.bndes.gov.br/wps/portal/site/home/transparencia/centraldedownloads) 
from Brazilian Develop Bank database by operation

## Libraries Needed
For check and install all packages used you can run the script bellow:

```
list.packages <- c("shiny", "shinydashboard", "leaflet", "DT", "dplyr", "rgdal", "shinythemes",
                   "plotly", "highcharter", "tidyr", "rmarkdown", "htmltools")
new.packages <- list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

### Task List
- [x] Cartogram;
- [x] Time Series;
- [x] Pie Chart;
- [x] Bar Chart with drilldown;
- [x] Datatables;
- [x] Report in html;
- [ ] About and disclaimer.
-----
