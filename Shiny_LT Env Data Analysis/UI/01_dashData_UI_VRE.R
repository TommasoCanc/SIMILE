fluidRow(
  
  ##############
  # Right side #
  ##############

column(width = 4,
         HTML("<h2>Data Input & Management</h2>"),
         box(title = "Load Data", collapsible = TRUE, collapsed = TRUE, width = 12,
             column(width = 12,
             textInput(inputId = "mainPath",
                       label = "Main Path",
                       value = "~/workspace/VREFolders/Limnodata/Simile/Pallanza/prova/"
                       ),
             selectInput(inputId = "selectfile",
                         label = "Select File",
                         choice = NA,
                         multiple = TRUE)
              ),
             column(width = 6,
                    selectInput(inputId = "dataSelection",
                                label = "Select data type",
                                choices = c("Chlorophyll" = "Chlorophyll",
                                            "Ponsel" = "Ponsel"),
                                selected = "chl")
             ),
             column(width = 6,
                    selectInput(inputId = "separator",
                                label = "Separator",
                                choices = c(";" = ";",
                                            "," = ",",
                                            "|" = "|"),
                                selected = ",")
             ),
             column(width = 12,
             checkboxInput(
                     inputId = "loadData",
                     label = "Load Data",
                     value = FALSE,
                     width = NULL
                     )
             )
         ),

# Filter columns --------------------------------
         box(title = "Filter Columns", collapsible = TRUE, collapsed = TRUE, width = 12,
             uiOutput("picker"),
             actionButton("view", "View Selection"),
             checkboxInput(inputId = "checkFilteredColumns",
                           label = "Use filtered Columns",
                           value = FALSE, width = NULL)
             ),

# Filter rows --------------------------------
         box(title = "Filter Rows", collapsible = TRUE, collapsed = TRUE, width = 12,
       #       column(width = 6,
       #              # Filter year
       #              textInput(inputId = "filterYear", label = "Year"),
       #              # Filter month
       #              textInput(inputId = "filterMonth", label = "Month")
       #       ),
       #       column(width = 6,
       #              # Filter day
       #              textInput(inputId = "filterDay", label = "Day"),
       #              # Filter hour
       #              textInput(inputId = "filterHour", label = "Hour")
       #       ),
             dateRangeInput("dateRange", "Date range:",
                            start = "2020-01-01",
                            end   = "2022-12-31"),
             column(width = 12,
                    checkboxInput(inputId = "checkFilteredRows",
                                  label = "Use filtered data",
                                  value = FALSE, width = NULL)
                    )
         ),

# Data aggregation --------------------------------
         box(title = "Data aggregation", collapsible = TRUE, collapsed = TRUE, width = 12,

             column(width = 6,
              selectInput(inputId = "agrData",
             label = "Select Aggregation",
             choices = c(
               "Month" = "month",
               "Day" = "day",
               "Hour" = "hour",
               "Minute" = "min"),
             selected = "hour"),
             
             checkboxInput(inputId = "checkAgr",
               label = "Aggregate",
               value = FALSE, width = NULL)
       )
),

# Snrise sunset plot --------------------------------
         box(title = "Plot sunrise/sunset", collapsible = TRUE, collapsed = TRUE, width = 12,
                # Latitude
                column(
                       width = 6,
                       numericInput(inputId = "latitudeSun", label = "Latitude", value = 45.9283)
                ),
                # Longitude
                column(
                       width = 6,
                       numericInput(inputId = "longitudeSun", label = "Longitude", value = 8.5554)
                ),
                # Number of columns Sun Plot
                column(
                       width = 6,
                       numericInput(inputId = "ncolSunPlot", label = "Numper of columns Plot", value = 2)
                ),
                # Number of rows Sun Plot
                column(
                       width = 6,
                       numericInput(inputId = "nrowSunPlot", label = "Numper of rows Plot", value = 2)
                ),
                # Plot title
                textInput(
                       inputId = "sunPlotTitle",
                       label = "Plot title",
                       value = NA
                ),
                column(
                       width = 4,
                       checkboxInput(
                              inputId = "sunPlot", label = "Plot",
                              value = FALSE, width = NULL)
                ),
                column(
                       width = 4,
                       checkboxInput(
                              inputId = "sunPlotFiltered", label = "Use filtered Data",
                              value = FALSE, width = NULL)
                ),
                column(
                       width = 4,
                       checkboxInput(
                              inputId = "sunPlotAgr", label = "Use Agr Data",
                              value = FALSE, width = NULL)
                )
         )
),
  
  #############
  # Left side #
  #############
  
  column(width = 8,
         #HTML("<h2>Summary</h2>"),
         htmlOutput("summaryInFiles"),
         uiOutput("dataMain")
       #   br(),
       #   htmlOutput("pathFile"),
       #   tabBox(
       #          width = 12, id = "sumData",
       #          tabPanel(
       #                 "Data Table",
       #                 uiOutput("dataTable")
       #          ),
       #          tabPanel(
       #                 "Column Filtered Table", "Details",
       #                 uiOutput("dataFilteredCol"),
       #                 br(),
       #                 downloadButton("downloadFilteredColumns")
       #          ),
       #          tabPanel(
       #                 "Row Filtered Table", "Details",
       #                 uiOutput("dataFiltered"),
       #                 br(),
       #                 downloadButton("downloadFilteredRows")
       #          ),
       #          tabPanel(
       #                 "Agr Data Table", "Details",
       #                 uiOutput("dataAgr"),
       #                 br(),
       #                 downloadButton("downloadDataAgr")
       #          ),
       #          tabPanel(
       #                 "Plot", "Details: You can use the aggregate data at maximum hour resolution for the plot",
       #                 plotOutput("summaryPlot")
       #          )
       #   )
  )
)
