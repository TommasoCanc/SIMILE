fluidRow(
  
  ##############
  # Right side #
  ##############
  
  column(width = 4,
         HTML("<h2>Data Input & Management</h2>"),
         box(title = "Load Data", collapsible = TRUE, collapsed = TRUE, width = 12,
             column(width = 6,
                    selectInput(inputId = "dataSelection",
                                label = "Select data type",
                                choices = c("Chlorophyll" = "chl",
                                            "Ponsel" = "pon"),
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

             textInput(inputId = "mainPath",
                       label = "Main Path",
                       value = "~/workspace/VREFolders/Limnodata/Simile/Pallanza/prova/"
                       ),

             selectInput(inputId = 'selectfile',
                         label = 'Select File',
                         choice = NA, #list.files("~/workspace/VREFolders/Limnodata/Simile/Pallanza/prova/"),
                         multiple = TRUE),

             checkboxInput(
                     inputId = "loadData",
                     label = "Load Data",
                     value = FALSE,
                     width = NULL
              )
         ),
       
         box(title = "Filter Columns", collapsible = TRUE, collapsed = TRUE, width = 12,
             uiOutput("picker"),
             actionButton("view", "View Selection"),
             checkboxInput(inputId = "checkFilteredColumns",
                           label = "Use filtered Columns",
                           value = FALSE, width = NULL)
             ),
         
         box(title = "Filter Rows", collapsible = TRUE, collapsed = TRUE, width = 12,
             column(width = 6,
                    # Filter year
                    textInput(inputId = "filterYear", label = "Year"),
                    # Filter month
                    textInput(inputId = "filterMonth", label = "Month")
             ),
             column(width = 6,
                    # Filter day
                    textInput(inputId = "filterDay", label = "Day"),
                    # Filter hour
                    textInput(inputId = "filterHour", label = "Hour")
             ),
             column(width = 12,
                    checkboxInput(inputId = "checkFiltered",
                                  label = "Use filtered data",
                                  value = FALSE, width = NULL)
                    )
         ),
       
         box(title = "Plot sunrise/sunset", collapsible = TRUE, collapsed = TRUE, width = 12,
                # Latitude
                column(width = 6,
                       numericInput(inputId = "latitudeSun", label = "Latitude", value = 45.9283)
                ),
                # Longitude
                column(width = 6,
                       numericInput(inputId = "longitudeSun", label = "Longitude", value = 8.5554)
                ),
                # Number of columns Sun Plot
                column(width = 6,
                       numericInput(inputId = "ncolSunPlot", label = "Numper of columns Plot", value = 2)
                ),
                # Number of rows Sun Plot
                column(width = 6,
                       numericInput(inputId = "nrowSunPlot", label = "Numper of rows Plot", value = 2)
                ),
                # Plot title
                textInput(
                       inputId = "sunPlotTitle",
                       label = "Plot title",
                       value = NA
                ),
                column(width = 6,
                       checkboxInput(
                              inputId = "sunPlot", label = "Plot",
                              value = FALSE, width = NULL
                       )
                ),
                column(width = 6,
                       checkboxInput(
                              inputId = "sunPlotFiltered", label = "Use filtered Data",
                              value = FALSE, width = NULL
                       )
                )
         )
  ),
  
  #############
  # Left side #
  #############
  
  column(width = 8,
         HTML("<h2>Summary</h2>"),
         htmlOutput("summaryInFiles"),
         br(),
         htmlOutput("pathFile"),
         
         tabBox(width = 12, id = "sumData", 
                tabPanel("Data Table",
                         uiOutput("dataTable")),
                tabPanel("Column Filtered Table", "Details",
                         uiOutput("dataFilteredCol"),
                         br(),
                         uiOutput("downloadFilteredColumns")),
                tabPanel("Row Filtered Table", "Details",
                         uiOutput("dataFiltered"),
                         br(),
                         uiOutput("downloadFilteredRows")),
                tabPanel("Plot", "Details",
                         plotOutput("summaryPlot"))
         )
  )
)
