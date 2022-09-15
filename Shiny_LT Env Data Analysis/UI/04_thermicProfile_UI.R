fluidRow(

##############
# Right side #
##############

column(
      width = 4,
       HTML("<h2>Thermic profile</h2>"),
       box(title = "Options", width = 12,
              column(width = 12,
              textInput(inputId = "depthsThermic",
                        label = "Depths",
                        value = NA)),

              column(width = 6,
              numericInput(inputId = "SminThermic",
                           label = "Smin",
                           value = 0.1)),
              column(width = 6,
              numericInput(inputId = "mixedCutoffThermic",
                           label = "Mixed cutoff",
                           value = 1)
                           ),

              # column(width = 4,
              # checkboxInput(inputId = "seasonalThermic", label = "Seasonal",
              #               value = FALSE, width = NULL)),
              column(width = 4,
              checkboxInput(inputId = "indexThermic", label = "Index",
                            value = FALSE, width = NULL)),
              column(width = 4,
              checkboxInput(inputId = "tseriesThermic", label = "Single profile",
                            value = TRUE, width = NULL)),

box(title = "Run thermich analysis", width = 12,
              column(width = 6,
              checkboxInput(inputId = "thermicFiltered", label = "Use filtered data",
                            value = FALSE, width = NULL)
              ),
              column(width = 6,
              checkboxInput(inputId = "runThermic", label = "Run",
                            value = FALSE, width = NULL)
                            )
              )
       )
       ),

       #############
       # Left side #
       #############

column(
       width = 8,
       #HTML("<h2>Thermic Plot</h2>"),
       uiOutput("thermicData"),
       uiOutput("thermicPlot")
       )
)