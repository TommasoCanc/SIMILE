# Thermich Profile ----

# Check data ----
#conditionFiltered
thermic.df <- reactive({

if (isTRUE(input$conditionFiltered)) {
# if (isFALSE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
#     df <- dataIn()$mainTable
#   }
if (isTRUE(input$checkFilteredColumns) && isFALSE(input$checkFilteredRows)) {
    df <- datasetInput()
  }
if (isFALSE(input$checkFilteredColumns) && isTRUE(input$checkFilteredRows)) {
    df <- dataFilteredRow()
  }
if (isTRUE(input$checkFilteredColumns) && isTRUE(input$checkFilteredRows)) {
    df <- dataFilteredRow()
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