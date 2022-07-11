fluidRow(

       ##############
       # Right side #
       ##############

column(
      width = 4,
       HTML("<h2>Data Analyzer</h2>"),
       box(
       title = "Condition(s)", width = 12,
              tabBox(
                     width = 12,
                     # The id lets us use input$tabset1 on the server to find the current tab
                     id = "tabset1",

                     # Tab panel condition 1 ----
                     tabPanel(
                            "Cnd 1", HTML("<b>Details:</b> Check the presence of NA values"),
                            checkboxInput(
                                   inputId = "cond1", label = "Condition 1",
                                   value = FALSE, width = NULL
                            )
                     ),

                     # Tab panel condition 2 ----
                     tabPanel(
                            "Cnd 2", HTML("<b>Details:</b> Check the presence of 0"),
                            checkboxInput(
                                   inputId = "cond2", label = "Condition 2",
                                   value = FALSE, width = NULL
                            )
                     ),

                     # Tab panel condition 3 ----
                     tabPanel(
                            "Cnd 3", HTML("<b>Details:</b> Thresholds"),
                            checkboxInput(
                                   inputId = "cond3", label = "Condition 3",
                                   value = FALSE, width = NULL
                            ),
                            selectInput(
                                   inputId = "inequalitySelection",
                                   label = "Select inequality condition",
                                   choices = c(
                                          ">" = ">",
                                          ">=" = ">="
                                   ),
                                   selected = ">"
                            ),
                            textInput(inputId = "t1Min", label = "t1.min"),
                            textInput(inputId = "t1Max", label = "t1.max"),
                            textInput(inputId = "t2Min", label = "t2.min"),
                            textInput(inputId = "t2Max", label = "t2.max"),
                            textInput(inputId = "t3Min", label = "t3.min"),
                            textInput(inputId = "t3Max", label = "t3.max"),
                            textInput(inputId = "t4Min", label = "t4.min"),
                            textInput(inputId = "t4Max", label = "t4.max") # Add input for the Ponsel sensor
                     ),

                     # Tab panel condition 4 ----
                     tabPanel(
                            "Cnd 4", HTML("<b>Details:</b> 3 standard deviation (mean)"),
                            checkboxInput(
                                   inputId = "cond4", label = "Condition 4",
                                   value = FALSE, width = NULL
                            )
                     ),

                     # Tab panel condition 5 ----
                     tabPanel(
                            "Cnd 5", HTML("<b>Details:</b> 3 standard deviation (mad)"),
                            checkboxInput(
                                   inputId = "cond5", label = "Condition 5",
                                   value = FALSE, width = NULL
                            )
                     ),

                     # Tab panel condition 6 ----
                     tabPanel(
                            "Cnd 6", HTML("<b>Details:</b> Outlier function"),
                            checkboxInput(
                                   inputId = "cond6", label = "Condition 6",
                                   value = FALSE, width = NULL
                            )
                     )
              ),
 box(title = "Run condition", width = 12,
              checkboxInput(inputId = "runCondition", label = "Run",
                            value = FALSE, width = NULL))
       ),
       HTML("<h2>Aggregation</h2>"),
       box(
              title = "Aggregation", collapsible = TRUE, collapsed = TRUE, width = 12,
              column(
                     width = 6,
                     selectInput(
                            inputId = "agrDataCond",
                            label = "Select Aggregation",
                            choices = c(
                                   "Month" = "month",
                                   "Day" = "day",
                                   "Hour" = "hour",
                                   "Minute" = "minute"
                            ),
                            selected = "hour"
                     ),
                     checkboxInput(
                            inputId = "checkAgrCond",
                            label = "Aggregate",
                            value = FALSE, width = NULL
                     )
              )
       )
),

       #############
       # Left side #
       #############

column(
       width = 8,
       #HTML("<h2>Table</h2>"),
       uiOutput("dataCondition"),
       uiOutput("dataAgrCond")
)
)