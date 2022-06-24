# Check data ----
dataCondition <- reactive({
  if (isTRUE(input$checkFiltered)) {
    df <- dataIn()$mainFiltered
  } else {
    df <- dataIn()$mainTable
  }

  # Condition 1
  if (isTRUE(input$cond1)) {
    cond.1 <- cond.1.fn(x = df[, dataIn()$misCol])
  } else {
    cond.1 <- NA
  }
  # Condition 2
  if (isTRUE(input$cond2)) {
    cond.2 <- cond.2.fn(x = df[, dataIn()$misCol])
  } else {
    cond.2 <- NA
  }
  # Condition 3
  if (isTRUE(input$cond3)) {
    cond.3 <- cond.3.fn(
      x = df[, dataIn()$misCol],
      t1.min = input$t1Min, t1.max = input$t1Max,
      t2.min = input$t2Min, t2.max = input$t2Max,
      t3.min = input$t3Min, t3.max = input$t3Max,
      t4.min = input$t4Min, t4.max = input$t4Max,
      inequality = input$inequalitySelection
    )
  } else {
    cond.3 <- NA
  }
  # Condition 4
  if (isTRUE(input$cond4)) {
    cond.4 <- cond.4.fn(x = df[, dataIn()$misCol])
  } else {
    cond.4 <- NA
  }
  # Condition 5
  if (isTRUE(input$cond5)) {
    cond.5 <- cond.5.fn(x = df[, dataIn()$misCol])
  } else {
    cond.5 <- NA
  }
  # Condition 6
  if (isTRUE(input$cond6)) {
    cond.6 <- cond.6.fn(x = df[, dataIn()$misCol])
  } else {
    cond.6 <- NA
  }

  # Condition total
  cond.tot <- cbind(cond.1, cond.2, cond.3, cond.4, cond.5, cond.6)

  # Number of condition applied
  cond.apply <- sum(c(
    ifelse(!is.na(sum(cond.1)), 1, 0),
    ifelse(!is.na(sum(cond.2)), 1, 0),
    ifelse(!is.na(sum(cond.3)), 1, 0),
    ifelse(!is.na(sum(cond.4)), 1, 0),
    ifelse(!is.na(sum(cond.5)), 1, 0),
    ifelse(!is.na(sum(cond.6)), 1, 0)
  ))

  # Moltiplication
  cond.mult <- apply(cond.tot, 1, prod, na.rm = T)
  cond.add <- round(apply(cond.tot, 1, sum, na.rm = T) / (4 * cond.apply), digits = 2)
  cond.df <- cbind(df, cond.tot, cond.mult, cond.add)

  return(list(
    cond.df = cond.df,
    cond.1 = cond.1,
    cond.2 = cond.2,
    cond.3 = cond.3,
    cond.4 = cond.4,
    cond.5 = cond.5,
    cond.6 = cond.6
  ))
})


output$dataCondition <- renderUI({
  if (isTRUE(input$cond1 | input$cond2 | input$cond3 | input$cond4 | input$cond5)) {
    box(
      title = "Data Condition", width = 12,
      DT::renderDataTable(
        dataCondition()$cond.df,
        options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE),
        rownames = FALSE
      ),
      br(),
      downloadHandler(
        filename = function() {
          paste(input$dataSelection, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
          write.csv(dataCondition()$cond.df, con, row.names = FALSE)
        }
      )
    )
  }
})

########################################################################
# Data aggregation --------------------------------
dataAggregationCon <- reactive({
  if (isTRUE(input$checkAgrCond)) {
    tableAgrCond <- dataCondition()$cond.df
    tableAgrCond$datehour <- cut(ymd_hms(tableAgrCond$datetimeisoformat), breaks = input$agrDataCond)

    tableMult <- tableAgrCond[which(tableAgrCond$cond.mult == 1), ] # Select only the record wit condition = 1
    tableMult[sapply(tableMult, function(x) all(is.na(x)))] <- NULL # Remove column with only NA values

    tableMult <- tableMult[, 7:ncol(tableMult)] # Remove columns with date information

    # Data aggregation Table
    mainTableAgr <- aggregate(. ~ datehour, data = tableMult, FUN = mean)
    mainTableAgr[, 2:ncol(mainTableAgr)] <- round(mainTableAgr[, 2:ncol(mainTableAgr)], digits = 2)

    return(mainTableAgr)
  }
})

# Filtered Condition Table Output
output$dataAgrCond <- renderUI({
  if (isTRUE(input$checkAgrCond)) {
    box(
      title = "Data Condition Aggregation", width = 12,
      DT::renderDataTable(
        dataAggregationCon(),
        options = list(autoWidth = TRUE, scrollY = "400px", paging = FALSE),
        rownames = FALSE
      ),
      br(),
      downloadHandler(
        filename = function() {
          paste(input$dataSelection, "AgrCondTable_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
          write.csv(dataAggregationCon(), con, row.names = FALSE)
        }
      )
    )
  }
})

########################################################################