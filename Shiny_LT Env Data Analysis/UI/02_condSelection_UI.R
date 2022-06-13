fluidRow(
  
  ##############
  # Right side #
  ##############
  
  column(width = 4,
         HTML("<h2>Data Analyzer</h2>"),
         box(title = "Condition(s)", width = 12,
             tabBox(width = 12,
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", 
                    tabPanel("Cnd 1", HTML("<b>Details:</b> Check the presence of NA values"),
                             checkboxInput(inputId = "cond1", label = "Condition 1", 
                                           value = FALSE, width = NULL)),
                    tabPanel("Cnd 2", HTML("<b>Details:</b> Check the presence of 0"),
                             checkboxInput(inputId = "cond2", label = "Condition 2", 
                                           value = FALSE, width = NULL)),
                    tabPanel("Cnd 3", HTML("<b>Details:</b> Thresholds"),
                             checkboxInput(inputId = "cond3", label = "Condition 3", 
                                           value = FALSE, width = NULL),
                             selectInput(inputId = "inequalitySelection", 
                                         label = "Select inequality condition",
                                         choices = c(">" = ">",
                                                     ">=" = ">="),
                                         selected = ">"),
                             textInput(inputId = "ChlSMin", label = "Chl_S_min"),
                             textInput(inputId = "ChlSMax", label = "Chl_S_max"),
                             textInput(inputId = "ChlDMin", label = "Chl_D_min"),
                             textInput(inputId = "ChlDMax", label = "Chl_D_max"),
                             textInput(inputId = "PCMin", label = "PC_min"),
                             textInput(inputId = "PCMax", label = "PC_max"),
                             textInput(inputId = "PEMin", label = "PE_min"),
                             textInput(inputId = "PEMax", label = "PE_max")
                    ),
                    tabPanel("Cnd 4", HTML("<b>Details:</b> 3 standard deviation (mean)"),
                             checkboxInput(inputId = "cond4", label = "Condition 4", 
                                           value = FALSE, width = NULL)),
                    tabPanel("Cnd 5", HTML("<b>Details:</b> 3 standard deviation (mad)"),
                             checkboxInput(inputId = "cond5", label = "Condition 5", 
                                           value = FALSE, width = NULL)),
                    tabPanel("Cnd 6", HTML("<b>Details:</b> Outlier function"),
                             checkboxInput(inputId = "cond6", label = "Condition 6", 
                                           value = FALSE, width = NULL))
             )
         )
  ),
  
  #############
  # Left side #
  #############
  
  column(width = 8, 
         HTML("<h2>Table</h2>"),
         uiOutput("dataCondition"))
  
)