# Check data ----
#conditionFiltered
condition.df <- reactive({

if (isTRUE(input$conditionFiltered)) {
# if (isFALSE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
#     df <- dataIn()$mainTable
#   }
#if (isTRUE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
#    df <- datasetInput()
#  }
#if (isFALSE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
#    df <- datasetInput()
#  }
if (isFALSE(input$checkFilteredColumns) && isTRUE(input$checkFilteredRows)) {
    df <- dataFilteredRow()
  }
if (isTRUE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
     df <- datasetInput()
  }
} else {
  df <- dataIn()$mainTable
}

# Possiamo metterlo qui e fare return di una lista
# misColCondition <- colnames(df) %in% dataIn()$misCol
misColCondition <- colnames(df)[colnames(df) %in% dataIn()$misCol]

return(list(df = df,
            misColCondition = misColCondition))
})

# Prepare threshold for condition n.3 --------------------------------
# Create the number of tex input for Condition n. 3
output$treshold <- renderUI({
df <- condition.df()$df
misColCondition <- condition.df()$misColCondition

    lapply(1:length(misColCondition), function(i) {
      list(
        div(style="display: inline-block;vertical-align:top; width: 100px;",
        textInput(paste0("thMin", i), label = paste("Thr Min", misColCondition)[i], value = NA)),
        div(style="display: inline-block;vertical-align:top; width: 100px;",
        textInput(paste0("thMax", i), label = paste("Thr Max", misColCondition)[i], value = NA))
           )
    }) #end of lapply

    # lapply(1:ncol(df[, misColCondition]), function(i) {
    #   list(
    #     div(style="display: inline-block;vertical-align:top; width: 100px;",
    #     textInput(paste0("thMin", i), label = paste("Thr Min", colnames(df[, misColCondition]))[i], value = NA)),
    #     div(style="display: inline-block;vertical-align:top; width: 100px;",
    #     textInput(paste0("thMax", i), label = paste("Thr Max", colnames(df[, misColCondition]))[i], value = NA))
    #        )
    # }) #end of lapply
  }) # end of renderUI


# For each MIN threshold we create a vector containing the values
th1 <- reactive({

df <- condition.df()$df
misColCondition <- condition.df()$misColCondition

if(isTRUE(input$cond3)){
  out1 <- vector()
    # Get ids for textboxes
    txtbox_ids1 <- sapply(1:length(misColCondition),function(i){paste0("thMin", i)})
    # Get values
    for(i in 1:length(txtbox_ids1)){out1[[i]] <- as.numeric(input[[txtbox_ids1[i]]])}
    return(out1)
    }


# if(isTRUE(input$cond3)){
#   out1 <- vector()
#     # Get ids for textboxes
#     txtbox_ids1 <- sapply(1:ncol(df[, misColCondition]),function(i){paste0("thMin", i)})
#     # Get values
#     for(i in 1:length(txtbox_ids1)){out1[[i]] <- as.numeric(input[[txtbox_ids1[i]]])}
#     return(out1)
#     }
  })

# For each MAX threshold we create a vector containing the values
th2 <- reactive({

df <- condition.df()$df
misColCondition <- condition.df()$misColCondition

  if(isTRUE(input$cond3)){
    out2 <- vector()
    # Get ids for textboxes
    txtbox_ids2 <- sapply(1:length(misColCondition),function(i){paste0("thMax", i)})
    # Get values
    for(i in 1:length(txtbox_ids2)){out2[[i]] <- as.numeric(input[[txtbox_ids2[i]]])}
    return(out2)
    }



  # if(isTRUE(input$cond3)){
  #   out2 <- vector()
  #   # Get ids for textboxes
  #   txtbox_ids2 <- sapply(1:ncol(df[, misColCondition]),function(i){paste0("thMax", i)})
  #   # Get values
  #   for(i in 1:length(txtbox_ids2)){out2[[i]] <- as.numeric(input[[txtbox_ids2[i]]])}
  #   return(out2)
  #   }
  })

# Create a dataframe with treshold values
threshold <- reactive({
  if(isTRUE(input$cond3)) {
    thMin = th1()
    thMax = th2()
    a <- cbind(thMin, thMax)
    return(a)
  }
  })

# Execute condition n.3 --------------------------------
cond.3 <- reactive({

df <- condition.df()$df
misColCondition <- condition.df()$misColCondition

if(isTRUE(input$cond3)) {
value <- data.frame(NA)
    for(i in 1:nrow(threshold())){
      value.1 <- df[, misColCondition][i][df[, misColCondition][i] > threshold()[i,1] & df[, misColCondition][i] < threshold()[i,2]]
      value.1 <- data.frame(ifelse(df[, misColCondition][,i] %in% value.1, 1, 0))
      colnames(value.1) <- misColCondition[i]
      value <- cbind(value, value.1)
    }
    value[sapply(value, function(x) all(is.na(x)))] <- NULL
    for(j in 1:ncol(df[, misColCondition])){colnames(value)[j] <- paste0("c3_", misColCondition[j])}
    return(value)
  }



#   if(isTRUE(input$cond3)) {
# value <- data.frame(NA)
#     for(i in 1:nrow(threshold())){
#       value.1 <- df[, misColCondition][i][df[, misColCondition][i] > threshold()[i,1] & df[, misColCondition][i] < threshold()[i,2]]
#       value.1 <- data.frame(ifelse(df[, misColCondition][,i] %in% value.1, 1, 0))
#       colnames(value.1) <- colnames(df[, misColCondition][i])
#       value <- cbind(value, value.1)
#     }
#     value[sapply(value, function(x) all(is.na(x)))] <- NULL
#     for(j in 1:ncol(df[, misColCondition])){colnames(value)[j] <- paste0("c3_", colnames(df[, misColCondition])[j])}
#     return(value)
#   }
  })


# Create global table containing condition outcomes --------------------------------
dataCondition <- reactive({
  
df <- condition.df()$df
misColCondition <- condition.df()$misColCondition

if(isTRUE(input$runCondition)){

  # Condition 1
if (isTRUE(input$cond1)) {
    cond.1 <- cond.1.fn(x = df[, misColCondition])
  } else {
    cond.1 <- NA
  }
  # Condition 2
  if (isTRUE(input$cond2)) {
    cond.2 <- cond.2.fn(x = df[, misColCondition])
  } else {
    cond.2 <- NA
  }
  # Condition 3
  if (isTRUE(input$cond3)) {
    # cond.3 <- cond.3.fn(
    #   x = df[, dataIn()$misCol],
    #   t1.min = input$t1Min, t1.max = input$t1Max,
    #   t2.min = input$t2Min, t2.max = input$t2Max,
    #   t3.min = input$t3Min, t3.max = input$t3Max,
    #   t4.min = input$t4Min, t4.max = input$t4Max,
    #   inequality = input$inequalitySelection
    # )

    cond.3 <- cond.3()

  } else {
    cond.3 <- NA
  }
  # Condition 4
  if (isTRUE(input$cond4)) {
    cond.4 <- cond.4.fn(x = df[, misColCondition])
  } else {
    cond.4 <- NA
  }
  # Condition 5
  if (isTRUE(input$cond5)) {
    cond.5 <- cond.5.fn(x = df[, misColCondition])
  } else {
    cond.5 <- NA
  }
  # Condition 6
  if (isTRUE(input$cond6)) {
    cond.6 <- cond.6.fn(x = df[, misColCondition])
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

  for (i in 1:length(misColCondition)) {
  cond.mult <- apply(cond.tot[grepl(paste0(misColCondition[i]), names(cond.tot))], 1, prod, na.rm = T)
  cond.df <- cbind(cond.df, cond.mult)
  colnames(cond.df)[length(cond.df)] <- paste0("condMult_", misColCondition[i])
  condMult.names <- c(condMult.names, paste0("condMult_", misColCondition[i]))
}

#   for (i in 1:length(colnames(df[, misColCondition]))) {
#   cond.mult <- apply(cond.tot[grepl(paste0(colnames(df[, misColCondition])[i]), names(cond.tot))], 1, prod, na.rm = T)
#   cond.df <- cbind(cond.df, cond.mult)
#   colnames(cond.df)[length(cond.df)] <- paste0("condMult_", colnames(df[, misColCondition])[i])
#   condMult.names <- c(condMult.names, paste0("condMult_", misColCondition[i]))
# }
  
  
  # for (i in 1:length(dataIn()$misCol)) {
  #   cond.mult <- apply(cond.tot[grepl(paste0(dataIn()$misCol[i]), names(cond.tot))], 1, prod, na.rm = T)
  #   cond.df <- cbind(cond.df, cond.mult)
  #   colnames(cond.df)[length(cond.df)] <- paste0("condMult_", dataIn()$misCol[i])
  #   condMult.names <- c(condMult.names, paste0("condMult_", dataIn()$misCol[i]))
  # }

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

# Conditions table viewer
output$dataCondition <- renderUI({
  if(isTRUE(input$runCondition)) {
    column(
      width = 12,
      HTML("<h2>Data Table</h2>"),
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
      )
  }
})

# Data aggregation --------------------------------
dataAggregationCon <- reactive({
  if (isTRUE(input$checkAgrCond)) {
    tableAgrCond <- dataCondition()$cond.df
    misColCondition <- condition.df()$misColCondition

    tableAgrCond$datehour <- cut(ymd_hms(tableAgrCond$datetimeisoformat), breaks = input$agrDataCond)

    tableMult <- data.frame(NA)

for(i in 1:ncol(tableAgrCond[ ,grepl("condMult_", names(tableAgrCond))])) {
      if(i == 1) {
        colVar <- which(colnames(tableAgrCond) == paste0("condMult_", misColCondition[i]))
        tableAgrCond[colVar][tableAgrCond[colVar] == 0] <- NA
        tableAgrCond.var <- tableAgrCond[c("datehour", misColCondition[i], colnames(tableAgrCond[colVar]))]
        tableAgrCond.var <- tableAgrCond.var[!is.na(tableAgrCond.var[3]), ]
        tableAgrCond.agr <- aggregate(. ~ datehour, data = tableAgrCond.var, FUN = mean)
        tableAgrCond.agr <- tableAgrCond.agr[,-3]
        tableMult <- cbind(tableMult, tableAgrCond.agr)
      } else {
        colVar <- which(colnames(tableAgrCond) == paste0("condMult_", misColCondition[i]))
        tableAgrCond[colVar][tableAgrCond[colVar] == 0] <- NA
        tableAgrCond.var <- tableAgrCond[c("datehour", misColCondition[i], colnames(tableAgrCond[colVar]))]
        tableAgrCond.var <- tableAgrCond.var[!is.na(tableAgrCond.var[3]), ]
        tableAgrCond.agr <- aggregate(. ~ datehour, data = tableAgrCond.var, FUN = mean)[2]
        tableMult <- cbind(tableMult, tableAgrCond.agr)
      }
    }

tableMult[sapply(tableMult, function(x) all(is.na(x)))] <- NULL
tableMult[2:ncol(tableMult)] <- round(tableMult[2:ncol(tableMult)], digits = 2)
colnames(tableMult)[1] <- "datetimeisoformat"

# If we aggregate at HOUR level
if(input$agrDataCond == "hour"){
    # Convert and create date columns
    tableMult$datetimeisoformat <- ymd_hms(tableMult$datetimeisoformat)
    tableMult$year <- year(ymd_hms(tableMult$datetimeisoformat))
    tableMult$month <- month(ymd_hms(tableMult$datetimeisoformat))
    tableMult$day <- day(ymd_hms(tableMult$datetimeisoformat))
    tableMult$hour <- hour(ymd_hms(tableMult$datetimeisoformat))
    tableMult$minute <- minute(ymd_hms(tableMult$datetimeisoformat))
    tableMult$second <- second(ymd_hms(tableMult$datetimeisoformat))
}

# If we aggregate at DAY level
if(input$agrDataCond == "day"){
    # Convert and create date columns
    tableMult$datetimeisoformat <- ymd_hms(paste(tableMult$datetimeisoformat, " 12:00:00"))
    tableMult$year <- year(paste(tableMult$datetimeisoformat, " 12:00:00"))
    tableMult$month <- month(paste(tableMult$datetimeisoformat, " 12:00:00"))
    tableMult$day <- day(paste(tableMult$datetimeisoformat, " 12:00:00"))
    tableMult$hour <- hour(paste(tableMult$datetimeisoformat, " 12:00:00"))
    tableMult$minute <- minute(paste(tableMult$datetimeisoformat, " 12:00:00"))
    tableMult$second <- second(paste(tableMult$datetimeisoformat, " 12:00:00"))
}

col_order <- c("datetimeisoformat", "year", "month", "day", "hour", "minute", "second")
tableMult <- tableMult[, c(col_order, misColCondition)]

    return(tableMult)
  }
})

# Aggregated Condition Table Output
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
