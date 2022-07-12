# Check data ----
dataCondition <- reactive({
  if (isTRUE(input$checkFiltered)) {
    df <- dataIn()$mainFiltered
  } else {
    df <- dataIn()$mainTable
  }

if(isTRUE(input$runCondition)){

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

  # Moltiplication
  cond.mult <- apply(cond.tot, 1, prod, na.rm = T)
  # cond.add <- round(apply(cond.tot, 1, sum, na.rm = T) / (4 * cond.apply), digits = 2)
  cond.df <- cbind(df, cond.tot)
  # Remove columns with all NA values
  cond.df[sapply(cond.df, function(x) all(is.na(x)))] <- NULL

# Create vector with moltiplication column names
  condMult.names <- vector()

  # Moltiplication. Add to the main condition table the multiplication columns. One for each variable
  for (i in 1:length(dataIn()$misCol)) {
    cond.mult <- apply(cond.tot[grepl(paste0(dataIn()$misCol[i]), names(cond.tot))], 1, prod, na.rm = T)
    cond.df <- cbind(cond.df, cond.mult)
    colnames(cond.df)[length(cond.df)] <- paste0("condMult_", dataIn()$misCol[i])
    condMult.names <- c(condMult.names, paste0("condMult_", dataIn()$misCol[i]))
  }

  return(list(
    cond.df = cond.df,
    condMult.names = condMult.names,
    cond.1 = cond.1,
    cond.2 = cond.2,
    cond.3 = cond.3,
    cond.4 = cond.4,
    cond.5 = cond.5,
    cond.6 = cond.6
  ))

}
})


output$dataCondition <- renderUI({
  if (isTRUE(input$runCondition)) {
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

    tableMult <- data.frame(NA)

for(i in 1:length(dataIn()$misCol)) {
  
  if(i == 1) {
    colVar <- which(colnames(tableAgrCond) == paste0("condMult_", dataIn()$misCol[i]))
    tableAgrCond[colVar][tableAgrCond[colVar] == 0] <- NA
    tableAgrCond.var <- tableAgrCond[c("datehour", dataIn()$misCol[i])]
    tableAgrCond.agr <- aggregate(. ~ datehour, data = tableAgrCond.var, FUN = mean)
    tableMult <- cbind(tableMult, tableAgrCond.agr)
  } else {
    colVar <- which(colnames(tableAgrCond) == paste0("condMult_", dataIn()$misCol[i]))
    tableAgrCond[colVar][tableAgrCond[colVar] == 0] <- NA
    tableAgrCond.var <- tableAgrCond[c("datehour", dataIn()$misCol[i])]
    tableAgrCond.agr <- aggregate(. ~ datehour, data = tableAgrCond.var, FUN = mean)[2]
    tableMult <- cbind(tableMult, tableAgrCond.agr)
  }
}

tableMult[sapply(tableMult, function(x) all(is.na(x)))] <- NULL
tableMult[2:ncol(tableMult)] <- round(tableMult[2:ncol(tableMult)], digits = 2)
colnames(tableMult)[1] <- "datetimeisoformat"

    return(tableMult)
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