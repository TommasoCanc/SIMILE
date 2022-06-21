if (!require("shiny"))
  install.packages("shiny")
require(shiny) # <- Shiny interface

if (!require("shinydashboard"))
  install.packages("shinydashboard")
require(shinydashboard)

if (!require("shinyWidgets"))
  install.packages("shinyWidgets")
require(shinyWidgets)

if (!require("fontawesome"))
  install.packages("fontawesome")
require(fontawesome)

if (!require("DT"))
  install.packages("DT")
require(DT) # <- Shiny interface (Table)

if (!require("lubridate"))
  install.packages("lubridate")
require(lubridate)

if (!require("stringr"))
  install.packages("stringr")
require(stringr)

if (!require("suncalc"))
  install.packages("suncalc")
require(suncalc)

if (!require("ggplot2"))
  install.packages("ggplot2")
require(ggplot2)

if (!require("outliers"))
  install.packages("outliers")
require(outliers)

if (!require("dplyr"))
  install.packages("dplyr")
require(dplyr)

if (!require("gridExtra"))
  install.packages("gridExtra")
require(gridExtra)

# Functions
source("./Functions/Conditions_v2.R")
source("./Functions/Functions_v2.R")

load("~/workspace/VREFolders/Limnodata/Simile/Pallanza/archivio/2022-03")

ui <- dashboardPage(skin = "green",
  # Header
  dashboardHeader(title = "LT Env Data Analysis"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashHome", icon = icon("cog", lib = "glyphicon")),
      menuItem("Load Data", tabName = "dashData", icon = icon("cog", lib = "glyphicon")),
      menuItem("Check Data", tabName = "dashCheck", icon = icon("cog", lib = "glyphicon"),
               menuSubItem("Select Conditions", tabName = "condSelection"),
               menuSubItem("Plot Condition", tabName = "condPlot")),
      menuItem("Info", tabName = "dashInfo", icon = icon("cog", lib = "glyphicon")),
      menuItem("Team", tabName = "dashTeam", icon = icon("cog", lib = "glyphicon"))
    )
  ),
  
  # Body ---------------------------------------------------------------------------------------------
  dashboardBody(
    tabItems(
      # Home ----
      tabItem(tabName = "dashHome",
              fluidRow(HTML("Hello Wold!"),
                hr(),
                img(src='Simile.png'))
      ),
      
      # Load Data ----
      tabItem(tabName = "dashData", source("./UI/01_dashData_UI.R")$value),
      
      # Check Data ---- Condition Selection
      tabItem(tabName = "condSelection", source("./UI/02_condSelection_UI.R")$value),
      
      # Check Data ---- Plot Conditions
      tabItem(tabName = "condPlot", source("./UI/03_condPlot_UI.R")$value),
      
      # Info ----
      tabItem(tabName = "dashInfo",
              h2("Widgets tab content")
      ),
      # Team ----
      tabItem(tabName = "dashTeam",
              h2("Widgets tab content")
      )
    )
  )
)