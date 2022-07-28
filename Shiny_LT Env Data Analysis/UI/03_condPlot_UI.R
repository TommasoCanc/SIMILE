fluidRow(
  
  ##############
  # Right side #
  ##############
  
  column(width = 12,
         HTML("<h2>Data Analyzer</h2>"),
         box(title = "Plot(s)", width = 12,
br(),
column(width = 3,
numericInput(
   inputId = "conditionPlotList", 
   label = "Number of plots",
   value = 1,
   min = 1)
   ),

   br(),
             tabBox(width = 12,
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabCond",
                    tabPanel("Plot 1", "Condition 1",
                             checkboxInput(inputId = "plot.cond1", label = "Condition 1", 
                                           value = FALSE, width = NULL),
                             plotOutput("cond1Plot")),
                    tabPanel("Plot 2", "Condition 2",
                             checkboxInput(inputId = "plot.cond2", label = "Condition 2", 
                                           value = FALSE, width = NULL),
                             plotOutput("cond2Plot")),
                    tabPanel("Plot 3", "Condition 3",
                             checkboxInput(inputId = "plot.cond3", label = "Condition 3", 
                                           value = FALSE, width = NULL),
                             plotOutput("cond3Plot")),
                    tabPanel("Plot 4", "Condition 4",
                             checkboxInput(inputId = "plot.cond4", label = "Condition 4", 
                                           value = FALSE, width = NULL),
                             plotOutput("cond4Plot")),
                    tabPanel("Plot 5", "Condition 5",
                             checkboxInput(inputId = "plot.cond5", label = "Condition 5", 
                                           value = FALSE, width = NULL),
                             plotOutput("cond5Plot")),
                    tabPanel("Plot 6", "Condition 6",
                             checkboxInput(inputId = "plot.cond6", label = "Condition 6", 
                                           value = FALSE, width = NULL),
                             plotOutput("cond6Plot")),
                    tabPanel("Plot Total", "Plot Total",
                             checkboxInput(inputId = "plot.tot", label = "Plot Total", 
                                           value = FALSE, width = NULL),
                             plotOutput("totPlot"))
             )
         )
  )
)