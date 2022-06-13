# Load data ----
dataIn <- reactive({
  validate(need(input$inFiles != "", "select files..."))
  if (is.null(input$inFiles)) {
    return(NULL)
  } else {
    
    # Main information about the data ----
    path_list <- as.list(input$inFiles$datapath) # Data Path
    
    mainTable.df <- lapply(path_list, read.csv, sep = input$separator) %>% bind_rows
    
    # Extract the Date
    # date <- unique(as.Date(str_extract(input$inFiles$name, "[0-9]{4}-[0-9]{2}-[0-9]{2}"), 
    #                        format="%Y-%m-%d")) 
    
    # Data table creation ----
    # mainTable.df <- data.frame()
    # for(i in 1:length(input$inFiles$datapath)){
    #   mainTable.df.1 <- read.csv(input$inFiles$datapath[i],
    #                              sep = input$separator, header = FALSE, skip = 1) # skip= 1 do not read the first line
    #   mainTable.df <- rbind(mainTable.df, mainTable.df.1)
    # }

    
    # Remove columns containing only NA values
    mainTable.df[sapply(mainTable.df, function(x) all(is.na(x)))] <- NULL 

    
    # Update select input with column names
    updateSelectInput(session, inputId = "dateColumn", 
                      label = 'Select date column',
                      choices  = colnames(mainTable.df))
    
    # Chl_S: Chlorophyll superficial; Chl_D: Chlorophyll deep; PC: Pycochanin; PE: Phycoeritrin 
    colnames(mainTable.df) <- c("datetimeisoformat", "Chl_S", "Chl_D", "PC", "PE")
    
    for(i in 2:5){mainTable.df[ ,i] <- as.numeric(mainTable.df[ ,i])} # Convert in numeric
    mainTable.df$datetimeisoformat <- ymd_hms(mainTable.df$datetimeisoformat)
    
    mainTable.df$year <- year(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$month <- month(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$day <- day(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$hour <- hour(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$minute <-  minute(ymd_hms(mainTable.df$datetimeisoformat))
    mainTable.df$second <-  second(ymd_hms(mainTable.df$datetimeisoformat))
    
    mainTable.df <- mainTable.df[, c("datetimeisoformat", "year", "month", "day", "hour", "minute", "second", "Chl_S", "Chl_D", "PC", "PE")]
    head(mainTable.df)
    
    # Filter of Main Table
    mainTable.filtered <- filter(mainTable.df,
                                 conditional(input$filterYear != "", year == input$filterYear),
                                 conditional(input$filterMonth != "", month == input$filterMonth),
                                 conditional(input$filterDay != "", day == input$filterDay),
                                 conditional(input$filterHour != "", hour == input$filterHour))
    
    # Create dataframe with main information about the data
    mainInfo.df <- data.frame(LoadedFiles = length(input$inFiles$datapath),
                              timePeriodMin = as.character(min(ymd_hms(mainTable.df$datetimeisoformat))), 
                              timePeriodMax = as.character(max(ymd_hms(mainTable.df$datetimeisoformat))),
                              nOfRow = nrow(mainTable.df)
    )
    
    
    return(list(mainInfo = mainInfo.df,
                mainTable = mainTable.df,
                mainFiltered = mainTable.filtered
    ))
  }
})

# Output ----
output$summaryInFiles <- renderUI({
  if (!is.null(input$inFiles)) {
    box(title = "Summary Information", width = 12,
        HTML("<b>You have selcted:</b>", paste(dataIn()$mainInfo$LoadedFiles), "<b>file(s)</b>",
             "<br>",
             "<b>The time period of your variables span from</b>", dataIn()$mainInfo$timePeriodMin, 
             "<b>to</b>", dataIn()$mainInfo$timePeriodMax,
             "<br>",
             "<b>Your dataset contains</b>", dataIn()$mainInfo$nOfRow,"<b>data</b>"
        )
    )
  }
})

# Main Table Output
output$dataTable <- renderUI({
  if (!is.null(input$inFiles)) {
    DT::renderDataTable(
      dataIn()$mainTable,
      options = list(autoWidth = TRUE),
      rownames = FALSE
    )
    
  }
})

# Filtered Table Output
output$dataFiltered <- renderUI({
  if (input$filterYear != "" |
      input$filterMonth != "" |
      input$filterDay != "" |
      input$filterHour != "") {
    DT::renderDataTable(
      dataIn()$mainFiltered,
      options = list(autoWidth = TRUE, scrollY = "400px", paging = FALSE),
      rownames = FALSE
    )
    
  }
})

output$downloadFiltered <- renderUI({
  if(input$filterYear != "" |
     input$filterMonth != "" |
     input$filterDay != "" |
     input$filterHour != "") {
    downloadButton('downloadFiltered.id', 'Download Filtered Table')
  }
})
output$downloadFiltered.id <- downloadHandler(
  filename = function() {
    paste(input$dataSelection, "_filtered_", Sys.Date(), ".csv", sep = "")
  },
  content = function(con) {
    write.csv(dataIn()$mainFiltered, con, row.names = FALSE)
  }
)


output$summaryPlot <- renderPlot({
  if (isTRUE(input$sunPlot)) {
    plotOutput(dn.plot(dataIn()$mainTable))
  }
})