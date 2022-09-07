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

if (!require("xfun"))
  install.packages("xfun")
require(xfun)

if (!require("ggpubr"))
  install.packages("ggpubr")
require(ggpubr) # <- Plot interface

if (!require("rLakeAnalyzer"))
  install.packages("rLakeAnalyzer")
require(rLakeAnalyzer) # <- Lake analyzer functions

# Functions
source("./Functions/Conditions_v2.R")
source("./Functions/Functions_v2.R")

ui <- dashboardPage(skin = "green",
  # Header
  dashboardHeader(title = "LT Env Data Analysis"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashHome", icon = icon("home", lib = "glyphicon")),
      menuItem("Load Data", tabName = "dashData", icon = icon("upload", lib = "glyphicon")),
      menuItem("Check Data", tabName = "dashCheck", icon = icon("check", lib = "glyphicon"),
               menuSubItem("Select Conditions", tabName = "condSelection"),
               menuSubItem("Plot Condition", tabName = "condPlot")),
      menuItem("Lake Analytics", tabName = "dashLake", icon = icon("cog", lib = "glyphicon"),
               menuSubItem("Thermic profile", tabName = "thermicProfile"),
               menuSubItem("Schmidt stability", tabName = "schmidt"),
               menuSubItem("Thermocline depth", tabName = "thermocline"),
               menuSubItem("Metalimnion depth", tabName = "metalimnion")
                     ),
      menuItem("Team", tabName = "dashTeam", icon = icon("hand-right", lib = "glyphicon"))
    )
  ),
  
  # Body ----------------------------------------------------------------
    dashboardBody(
      tabItems(
        # Home ----
        tabItem(tabName = "dashHome", source("./UI/00_dashHome_UI_VRE.R")$value),
        # Load Data ----
        tabItem(tabName = "dashData", source("./UI/01_dashData_UI_VRE.R")$value),
        # Check Data ---- Condition Selection
        tabItem(tabName = "condSelection", source("./UI/02_condSelection_UI.R")$value),
        # Check Data ---- Plot Conditions
        tabItem(tabName = "condPlot", source("./UI/03_condPlot_UI.R")$value),
        # Lake analytics ----
        tabItem(tabName = "thermicProfile", source("./UI/04_thermicProfile_UI.R")$value),
        # Team ----
        tabItem(tabName = "dashTeam", source("./UI/05_Team_UI.R")$value)
      )
    )
)
