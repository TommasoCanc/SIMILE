# Update selectInput mainPath ----
observe({
   updateSelectInput(session, "selectfile",
      label = paste(input$mainPath), # We can remove this label in the future
      choices = list.files(input$mainPath),
      selected = NA
    )
})

# Load data ----
dataIn <- reactive({
  validate(need(input$selectfile != "", "select files..."))
  if(is.null(input$selectfile)) {
    return(NULL)
  } else {
    
    # Main information about the data ----
    path_list <- as.list(paste0(input$mainPath, input$selectfile))

# To load the data the input loadData has to be activated ----
  if(isTRUE(input$loadData)) {
# Create main table reading the csv data
    mainTable.df <- lapply(path_list, read.csv, sep = input$separator) %>% bind_rows
    
    # Remove columns containing only NA values
    mainTable.df[sapply(mainTable.df, function(x) all(is.na(x)))] <- NULL
    
    # Convert in numeric
    for(i in 2:ncol(mainTable.df)){mainTable.df[ ,i] <- as.numeric(mainTable.df[ ,i])} 
    
    # Convert and create date columns
    mainTable.df$datetimeisoformat <- ymd_hms(mainTable.df$datetimeisoformat)
    mainTable.df$year <- year(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$month <- month(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$day <- day(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$hour <- hour(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$minute <-  minute(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$second <-  second(ymd_hms(mainTable.df$datetimeisoformat))
    
    # Column different form date are filtered
    misCol <- colnames(mainTable.df)[colnames(mainTable.df) %ni% c("datetimeisoformat", "year", "month", "day", "hour", "minute", "second")]
    
    # Reorder main dataset
    mainTable.df <- mainTable.df[ ,c("datetimeisoformat", "year", "month", "day", "hour", "minute", "second", 
                                     misCol)]

    # Create dataframe with main information about the data
        mainInfo.df <- data.frame(LoadedFiles = length(input$selectfile),
                                  timePeriodMin = as.character(min(ymd_hms(mainTable.df$datetimeisoformat))),
                                  timePeriodMax = as.character(max(ymd_hms(mainTable.df$datetimeisoformat))),
                                  nOfRow = nrow(mainTable.df)
                                  )
    
    return(list(mainInfo = mainInfo.df,
                mainTable = mainTable.df,
                misCol = misCol,
                path_list = path_list)
           )
    }
  }
})

# Output Main information ----
output$summaryInFiles <- renderUI({
  if (!is.null(input$selectfile)) {
    box(title = "Summary Information", width = 12,
        HTML("<b>You have selcted:</b>", dataIn()$mainInfo$LoadedFiles, "<b>file(s)</b>",
             "<br>",
             "<b>The time period of your variables span from</b>", dataIn()$mainInfo$timePeriodMin,
             "<b>to</b>", dataIn()$mainInfo$timePeriodMax,
             "<br>",
             "<b>Your dataset contains</b>", dataIn()$mainInfo$nOfRow,"<b>data</b>"
        )
    )
  }
})

# Show files path (we can remove it in the future)
output$pathFile <- renderUI({
    box(title = "path", width = 12,
        HTML(paste0(dataIn()$path_list))
    )
})

# Main Table Output ----
output$dataTable <- renderUI({
  if (!is.null(input$selectfile)) {
    DT::renderDataTable(
      dataIn()$mainTable,
      options = list(autoWidth = F, scrollX = TRUE ,scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
  }
})

# Filter Columns -------------------------------
# Select columns ----
output$picker <- renderUI({
  pickerInput(inputId = "pick",
              label = "Choose columns",
              selected = NULL,
              choices = colnames(dataIn()$mainTable),
              options = list(`actions-box` = TRUE),
              multiple = TRUE)
})

# Activate column selection
datasetInput <- eventReactive(input$view, {
    datasetInput.df <- dataIn()$mainTable %>%
      select(input$pick)
  return(datasetInput.df)
})

output$dataFilteredCol <- renderUI({
  if(!is.null(input$selectfile)) {
    DT::renderDataTable(
      datasetInput(),
      options = list(autoWidth = TRUE),
      rownames = FALSE
    )
  }
})

# Download columns filtered data
output$downloadFilteredColumns <- renderUI({
  if(input$filterYear != "" |
     input$filterMonth != "" |
     input$filterDay != "" |
     input$filterHour != "") {
    downloadButton("downloadFilteredColumns.id", "Download Filtered Table")
  }
})
output$downloadFilteredColumns.id <- downloadHandler(
  filename = function() {
    paste(input$dataSelection, "_filtered_", Sys.Date(), ".csv", sep = "")
  },
  content = function(con) {
    write.csv(datasetInput(), con, row.names = FALSE)
  })

# Filter rows -------------------------------
dataFilteredRow <- reactive({
  if(isFALSE(input$checkFilteredColumns)) {
    mainTable <- dataIn()$mainTable
  } else {
    mainTable <- datasetInput()
  }
  
    # Filter Main Table by dates values
    mainTable.filtered <- filter(mainTable,
                                 conditional(input$filterYear != "", year == input$filterYear),
                                 conditional(input$filterMonth != "", month == input$filterMonth),
                                 conditional(input$filterDay != "", day == input$filterDay),
                                 conditional(input$filterHour != "", hour == input$filterHour)
                                 )
    return(mainTable.filtered)
})

# Filtered Main Table Output
output$dataFiltered <- renderUI({
  if (input$filterYear != "" |
      input$filterMonth != "" |
      input$filterDay != "" |
      input$filterHour != "") {
    DT::renderDataTable(
      dataFilteredRow(),
      options = list(autoWidth = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
  }
})

# Download row filtered data
output$downloadFilteredRows <- renderUI({
  if(input$filterYear != "" |
     input$filterMonth != "" |
     input$filterDay != "" |
     input$filterHour != "") {
    downloadButton("downloadFilteredRows.id", "Download Filtered Table")
  }
})

output$downloadFilteredRows.id <- downloadHandler(
  filename = function() {
    paste(input$dataSelection, "_filtered_", Sys.Date(), ".csv", sep = "")
  },
  content = function(con) {
    write.csv(dataFilteredRow(), con, row.names = FALSE)
  })

################################################################
# Data aggregation --------------------------------
dataAggregation <- reactive({

if(isTRUE(input$checkAgr)) {

mainTableAgr <-  dataIn()$mainTable
mainTableAgr$datehour <- cut(as.POSIXct(mainTableAgr$datetimeisoformat, format= "%Y-%m-%dT%H:%M"), 
                                breaks = input$agrData) # We can substitute with day 

data.agr <- mainTableAgr[ ,c("datehour", dataIn()$misCol)]

    # Data aggregation Table
mainTable.agr <- aggregate(. ~ datehour, data = data.agr, FUN = mean)
mainTable.agr[ ,2:ncol(mainTable.agr)] <- round(mainTable.agr[ ,2:ncol(mainTable.agr)], digits = 2)

    return(mainTable.agr)
}
})

# Filtered Main Table Output
output$dataAgr <- renderUI({
    DT::renderDataTable(
     dataAggregation(),
      options = list(autoWidth = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
})

################################################################


# Sunrise/Sunshine Plot --------------------------------
output$summaryPlot <- renderPlot({
  if (isTRUE(input$sunPlot)) {
    if(isFALSE(input$sunPlotFiltered)) {
      dataPlot <- dataIn()$mainTable
    } else {
      dataPlot <- dataFilteredRow()
    }
    
    plotOutput(dn.plot(dataPlot,
                       latitude = input$latitudeSun,
                       longitude = input$longitudeSun,
                       title = input$sunPlotTitle,
                       f.ncol = input$ncolSunPlot,
                       f.nrow = input$nrowSunPlot))
  }
})