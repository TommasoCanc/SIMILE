# Shiny Server Side -------
server <- function(input, output, session) {

  source("./SERVER/01_dashData_SERVER_v3.R", local = T) 
  source("./SERVER/02_condSelection_SERVER.R", local = T) 
  source("./SERVER/03_condPlot_SERVER.R", local = T) 
  
}
