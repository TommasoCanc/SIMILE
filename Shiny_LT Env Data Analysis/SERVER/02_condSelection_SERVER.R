# Check data ----
dataCondition <- reactive({
  
  if(isTRUE(input$checkFiltered)){
    df <- dataIn()$mainFiltered
  } else{
    df <- dataIn()$mainTable
  }
  
  
  # Condition 1
  if(isTRUE(input$cond1)){
    cond.1 <- cond.1.fn(x = df[ ,c("Chl_S", "Chl_D", "PC", "PE")])
  } else {cond.1 <- NA}
  # Condition 2
  if(isTRUE(input$cond2)){
    cond.2 <- cond.2.fn(x = df[ ,c("Chl_S", "Chl_D", "PC", "PE")])
  } else {cond.2 <- NA}
  # Condition 3
  if(isTRUE(input$cond3)){
    cond.3 <- cond.3.fn(x = df[ ,c("Chl_S", "Chl_D", "PC", "PE")],
                        Chl_S_min = input$ChlSMin, Chl_S_max = input$ChlSMax,
                        Chl_D_min = input$ChlDMin, Chl_D_max = input$ChlDMax,
                        PC_min = input$PCMin, PC_max = input$PCMax,
                        PE_min = input$PEMin, PE_max = input$PEMax,
                        inequality = input$inequalitySelection)
  } else {cond.3 <- NA}
  # Condition 4
  if(isTRUE(input$cond4)){
    cond.4 <- cond.4.fn(x = df)
  } else {cond.4 <- NA}
  # Condition 5
  if(isTRUE(input$cond5)){
    cond.5 <- cond.5.fn(x = df)
  } else {cond.5 <- NA}
  # Condition 6
  if(isTRUE(input$cond6)){
    cond.6 <- cond.6.fn(x = df)
  } else {cond.6 <- NA}
  
  # Condition total
  cond.tot <- cbind(cond.1, cond.2, cond.3, cond.4, cond.5, cond.6)
  
  # Number of condition applied
  cond.apply <- sum(c(ifelse(!is.na(sum(cond.1)),1,0),
                      ifelse(!is.na(sum(cond.2)),1,0),
                      ifelse(!is.na(sum(cond.3)),1,0),
                      ifelse(!is.na(sum(cond.4)),1,0),
                      ifelse(!is.na(sum(cond.5)),1,0),
                      ifelse(!is.na(sum(cond.6)),1,0)))
  
  # Moltiplication
  cond.mult <- apply(cond.tot, 1, prod, na.rm=T)
  cond.add <- apply(cond.tot, 1, sum, na.rm=T)/(4*cond.apply)
  chl.df <- cbind(df, cond.tot, cond.mult, cond.add)
  
  return(chl.df)
  
})


output$dataCondition <- renderUI({
  if (isTRUE(input$cond1 | input$cond2 | input$cond3 | input$cond4 | input$cond5)) {
    box(title = "Data Condition", width = 12,
        DT::renderDataTable(
          dataCondition(),
          options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE),
          rownames = FALSE
        ),
        
        br(),
        
        downloadHandler(
          filename = function() {
            paste(input$dataSelection, "_", Sys.Date(), ".csv", sep = "")
          },
          content = function(con) {
            write.csv(dataCondition(), con, row.names = FALSE)
          }
        )
        
    )
  }
})