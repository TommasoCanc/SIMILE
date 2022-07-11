# Update selectInput mainPath --------------------------------
# When you set the main path in the selectfile box appear the file contained into the folder
observe({
   updateSelectInput(session, "selectfile",
      label = "Select file(s)", #paste(input$mainPath), # We can remove this label in the future
      choices = list.files(input$mainPath),
      selected = NA
    )
})



# Load data -------------------------------
dataIn <- reactive({
  
  # validate(need(input$selectfile != "", "Select files..."))
  if (is.null(input$selectfile)) {
    return(NULL)
  } else {

# The firs column containing the temporal variable has to be named "datetimeisoformat"

    # Main information about the data ----
    path_list <- as.list(paste0(input$mainPath, input$selectfile))

    # To load the data the input loadData has to be activated ----
    if (isTRUE(input$loadData)) {
      # Create main table reading the .csv data
      mainTable.df <- lapply(path_list, read.csv, sep = input$separator) %>% bind_rows()

      # Remove columns containing only NA values
      mainTable.df[sapply(mainTable.df, function(x) all(is.na(x)))] <- NULL

      # Convert in numeric
      for (i in 2:ncol(mainTable.df)) {mainTable.df[, i] <- as.numeric(mainTable.df[, i])}

      # Convert and create date columns
      mainTable.df$datetimeisoformat <- ymd_hms(mainTable.df$datetimeisoformat)
      mainTable.df$year <- year(ymd_hms(mainTable.df$datetimeisoformat))
      mainTable.df$month <- month(ymd_hms(mainTable.df$datetimeisoformat))
      mainTable.df$day <- day(ymd_hms(mainTable.df$datetimeisoformat))
      mainTable.df$hour <- hour(ymd_hms(mainTable.df$datetimeisoformat))
      mainTable.df$minute <- minute(ymd_hms(mainTable.df$datetimeisoformat))
      mainTable.df$second <- second(ymd_hms(mainTable.df$datetimeisoformat))

      # Column different form date are filtered
      misCol <- colnames(mainTable.df)[colnames(mainTable.df) %ni% c("datetimeisoformat", "year", "month", "day", "hour", "minute", "second")]

      # Reorder main dataset
      mainTable.df <- mainTable.df[, c(
        "datetimeisoformat", "year", "month", "day", "hour", "minute", "second",
        misCol
      )]

      # Create dataframe with main information about the data
      mainInfo.df <- data.frame(
        LoadedFiles = length(input$selectfile),
        timePeriodMin = as.character(min(ymd_hms(mainTable.df$datetimeisoformat))), # It doesn't work in VRE
        timePeriodMax = as.character(max(ymd_hms(mainTable.df$datetimeisoformat))), # It doesn't work in VRE
        nOfRow = nrow(mainTable.df)
      )

      return(list(
        mainInfo = mainInfo.df,
        mainTable = mainTable.df,
        misCol = misCol,
        path_list = path_list
      ))
    }
  }
})

# Filter Columns -------------------------------
# Select columns ----
output$picker <- renderUI({
  pickerInput(
    inputId = "pick",
    label = "Choose columns",
    selected = NULL,
    choices = dataIn()$misCol,
    options = list(`actions-box` = TRUE),
    multiple = TRUE
  )
})

# Activate column selection
datasetInput <- eventReactive(input$view, {
  datasetInput.df <- dataIn()$mainTable %>%
    select("datetimeisoformat", input$pick)
  return(datasetInput.df)
})

# Filter rows -------------------------------
# Update date in the filter section automatically
observe({
if (isFALSE(input$checkFilteredColumns)) {
    mainTable <- dataIn()$mainTable
  } else {
    mainTable <- datasetInput()
  }

if(isTRUE(input$loadData)){
   updateDateRangeInput(session, "dateRange",
      label = "Date range",
      start = min(as.Date(ymd_hms(mainTable$datetimeisoformat))),
      end = max(as.Date(ymd_hms(mainTable$datetimeisoformat)))
    )
}
})

dataFilteredRow <- reactive({
  if (isFALSE(input$checkFilteredColumns)) {
    mainTable <- dataIn()$mainTable
  } else {
    mainTable <- datasetInput()
  }
# Filter dates
  mainTable.filtered <- mainTable %>% filter(as.Date(ymd_hms(mainTable$datetimeisoformat)) >= input$dateRange[1] & as.Date(ymd_hms(mainTable$datetimeisoformat)) <= input$dateRange[2])

  return(mainTable.filtered)
})

# Data aggregation --------------------------------
dataAggregation <- reactive({
  if (isTRUE(input$checkAgr)) {

if (isFALSE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
    mainTable <- dataIn()$mainTable[, c("datetimeisoformat", dataIn()$misCol)]
  }
if (isTRUE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
    mainTable <- datasetInput()
  }
if (isFALSE(input$checkFilteredColumns) && isTRUE(input$checkFilteredRows)) {
    mainTable <- dataFilteredRow()[, c("datetimeisoformat", dataIn()$misCol)]
  }
if (isTRUE(input$checkFilteredColumns) && isTRUE(input$checkFilteredRows)) {
    mainTable <- dataFilteredRow()
  }

    mainTable$datehour <- cut(ymd_hms(mainTable$datetimeisoformat), breaks = input$agrData)

    #data.agr <- mainTable[, c("datehour", dataIn()$misCol)]
    data.agr <- mainTable %>% select("datehour", everything())

    colnames(data.agr)[1] <- "datetimeisoformat"

    # Data aggregation Table
    mainTable.agr <- aggregate(. ~ datetimeisoformat, data = data.agr, FUN = mean)
    mainTable.agr[, 2:ncol(mainTable.agr)] <- round(mainTable.agr[, 2:ncol(mainTable.agr)], digits = 2)

    return(mainTable.agr)
  }
})

##############
# Right side #
##############

# Show files path (we can remove it in the future)
# output$pathFile <- renderUI({
#   box(
#     title = "path", width = 12,
#     HTML(paste0(dataIn()$path_list))
#   )
# })

# Output Main information output ----
output$summaryInFiles <- renderUI({
  if (isTRUE(input$loadData)) {
    HTML("<h3>Data view</h3>")
    box(
      title = "Summary Information", width = 12,
      HTML(
        "<b>You have selcted:</b>", dataIn()$mainInfo$LoadedFiles, "<b>file(s)</b>",
        "<br>",
        "<b>The time period of your variables span from</b>", dataIn()$mainInfo$timePeriodMin,
        "<b>to</b>", dataIn()$mainInfo$timePeriodMax,
        "<br>",
        "<b>Your dataset contains</b>", dataIn()$mainInfo$nOfRow, "<b>data</b>"
      )
    )
  } else {HTML("<h3>Please load your data...</h3>")}
})

# Main Table Output ----
output$dataTable <- renderUI({
  if (isTRUE(input$loadData)) {
    DT::renderDataTable(
      dataIn()$mainTable,
      options = list(autoWidth = F, scrollX = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
  }
})

# Filtered columns output and download button ----
output$dataFilteredCol <- renderUI({
  if (isTRUE(input$loadData)) {
    DT::renderDataTable(
      datasetInput(),
      options = list(autoWidth = F, scrollX = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE)
  }
})

output$downloadFilteredColumns <- downloadHandler(
 filename = function() {
   paste(input$dataSelection, "_filteredColumns_", Sys.Date(), ".csv", sep = "")
 },
 content = function(con) {
   write.csv(datasetInput(), con, row.names = FALSE)
 }
)

# Filtered rows output and download button ----
output$dataFiltered <- renderUI({
    DT::renderDataTable(
      dataFilteredRow(),
      options = list(autoWidth = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
})

output$downloadFilteredRows <- renderUI({
    downloadButton("downloadFilteredRows.id", "Download Filtered Table")
})

output$downloadFilteredRows <- downloadHandler(
  filename = function() {
    paste(input$dataSelection, "_filteredRows_", Sys.Date(), ".csv", sep = "")
  },
  content = function(con) {
    write.csv(dataFilteredRow(), con, row.names = FALSE)
  }
)

# Aggregation table output and download button ----
output$dataAgr <- renderUI({
    DT::renderDataTable(
      dataAggregation(),
      options = list(autoWidth = F, scrollX = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
})

output$downloadDataAgr <- downloadHandler(
  filename = function() {
    paste(input$dataSelection, "_mainAggregation_", Sys.Date(), ".csv", sep = "")
  },
  content = function(con) {
    write.csv(dataAggregation(), con, row.names = FALSE)
  }
)

# Sunrise/Sunshine Plot ----
output$summaryPlot <- renderPlot({
  if (isTRUE(input$sunPlot) | isTRUE(input$sunPlotFiltered) | isTRUE(input$sunPlotAgr)) {

    if (isTRUE(input$sunPlot)) {
      dataPlot <- dataIn()$mainTable
    }

    if (isTRUE(input$sunPlotFiltered)) {
      dataPlot <- dataFilteredRow()
    }

    if (isTRUE(input$sunPlotAgr)) {
      dataPlot <- dataAggregation()
    }

    plotOutput(dn.plot(dataPlot,
      latitude = input$latitudeSun,
      longitude = input$longitudeSun,
      title = input$sunPlotTitle,
      f.ncol = input$ncolSunPlot,
      f.nrow = input$nrowSunPlot
    ))
  }
})

output$sunPlotDownload <- downloadHandler(
    filename = function() {
    paste(input$dataSelection, "_sunPlot_", Sys.Date(), ".pdf", sep = "")
  },
    content = function(con) {
 if (isTRUE(input$sunPlot) | isTRUE(input$sunPlotFiltered) | isTRUE(input$sunPlotAgr)) {

    if (isTRUE(input$sunPlot)) {
      dataPlot <- dataIn()$mainTable
    }

    if (isTRUE(input$sunPlotFiltered)) {
      dataPlot <- dataFilteredRow()
    }

    if (isTRUE(input$sunPlotAgr)) {
      dataPlot <- dataAggregation()
    }
   ggsave(dn.plot(dataPlot,
      latitude = input$latitudeSun,
      longitude = input$longitudeSun,
      title = input$sunPlotTitle,
      f.ncol = input$ncolSunPlot,
      f.nrow = input$nrowSunPlot
    ), filename = con, dpi = 300, width = 40, height = 20, units = "cm")
  
  }
    })

# Total Table output ----
output$dataMain <- renderUI({
  if (isTRUE(input$loadData)) {
              tabBox(
                width = 12, id = "sumData",
                tabPanel(
                       "Data Table",
                       uiOutput("dataTable")
                ),
                tabPanel(
                       "Column Filtered Table", "Details",
                       uiOutput("dataFilteredCol"),
                       br(),
                       downloadButton("downloadFilteredColumns")
                ),
                tabPanel(
                       "Row Filtered Table", "Details",
                       uiOutput("dataFiltered"),
                       br(),
                       downloadButton("downloadFilteredRows")
                ),
                tabPanel(
                       "Agr Data Table", "Details",
                       uiOutput("dataAgr"),
                       br(),
                       downloadButton("downloadDataAgr")
                ),
                tabPanel(
                       "Plot", "Details: You can use the aggregate data at maximum hour resolution for the plot",
                       plotOutput("summaryPlot"),
                       br(),
                       downloadButton("sunPlotDownload")
                )
         )
  }
})