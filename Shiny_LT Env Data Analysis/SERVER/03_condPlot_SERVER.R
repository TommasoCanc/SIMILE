# Condition plots ----

nPlot.reactive <- reactive({input$conditionPlotList})

# Plot contidion 1
output$cond1Plot <- renderPlot({
  if (isTRUE(input$plot.cond1) && isTRUE(input$cond1)) {
    
misColCondition <- condition.df()$misColCondition

    y <- dataCondition()$cond.df[grepl(paste0("c1_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(misColCondition)]
    x <- cbind(x, y)

    box(title = "Condition 1",
        plotOutput(cond.plot(x, condition = 1, title = paste(colnames(condition.df()$df[, misColCondition])[nPlot.reactive()]), nPlot = nPlot.reactive()))
    )
  } else {
    showNotification("Check if Condition 1 and Condition 1 Plot are activetes",
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 2
output$cond2Plot <- renderPlot({
  if (isTRUE(input$plot.cond2) && isTRUE(input$cond2)) {
    
misColCondition <- condition.df()$misColCondition

    y <- dataCondition()$cond.df[grepl(paste0("c2_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(misColCondition)]
    x <- cbind(x, y)
    
    box(title = "Condition 2",
        plotOutput(cond.plot(x, condition = 2, title = paste(colnames(condition.df()$df[, misColCondition])[nPlot.reactive()]), nPlot = nPlot.reactive()))
    )
  } else {
    showNotification("Check if Condition 2 and Condition 2 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 3
output$cond3Plot <- renderPlot({
  if (isTRUE(input$plot.cond3) && isTRUE(input$cond3)) {
    
misColCondition <- condition.df()$misColCondition

    y <- dataCondition()$cond.df[grepl(paste0("c3_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(misColCondition)]
    x <- cbind(x, y)
    
    box(title = "Condition 3",
        plotOutput(cond.plot(x, condition = 3, title = paste(colnames(condition.df()$df[, misColCondition])[nPlot.reactive()]), nPlot = nPlot.reactive()))
    )
  } else {
    showNotification("Check if Condition 3 and Condition 3 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 4
output$cond4Plot <- renderPlot({
  if (isTRUE(input$plot.cond4) && isTRUE(input$cond4)) {
    
misColCondition <- condition.df()$misColCondition

    y <- dataCondition()$cond.df[grepl(paste0("c4_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(misColCondition)]
    x <- cbind(x, y)
    
    box(title = "Condition 4",
        plotOutput(cond.plot(x, condition = 4, title = paste(colnames(condition.df()$df[, misColCondition])[nPlot.reactive()]), nPlot = nPlot.reactive()))
    )
  } else {
    showNotification("Check if Condition 4 and Condition 4 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Pélot condition 5
output$cond5Plot <- renderPlot({
  if (isTRUE(input$plot.cond5) && isTRUE(input$cond5)) {
    
misColCondition <- condition.df()$misColCondition

    y <- dataCondition()$cond.df[grepl(paste0("c5_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(misColCondition)]
    x <- cbind(x, y)
    
    box(title = "Condition 5",
        plotOutput(cond.plot(x, condition = 5, title = paste(colnames(condition.df()$df[, misColCondition])[nPlot.reactive()]), nPlot = nPlot.reactive()))
    )
  } else {
    showNotification("Check if Condition 5 and Condition 5 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 6
output$cond6Plot <- renderPlot({
  if (isTRUE(input$plot.cond6) && isTRUE(input$cond6)) {
    
misColCondition <- condition.df()$misColCondition

    y <- dataCondition()$cond.df[grepl(paste0("c6_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(misColCondition)]
    x <- cbind(x, y)
    
    box(title = "Condition 6",
        plotOutput(cond.plot(x, condition = 6, title = paste(colnames(condition.df()$df[, misColCondition])[nPlot.reactive()]), nPlot = nPlot.reactive()))
    )
  } else {
    showNotification("Check if Condition 6 and Condition 6 Plot are activetes",
                     duration = 6, type = "warning", closeButton = TRUE)
  }
})

# Plot Total Conditions
output$totPlot <- renderPlot({
  if (isTRUE(input$plot.tot)) {

misColCondition <- condition.df()$misColCondition

myplots <- lapply(1:length(condition.df()$misColCondition), function(i) {

  condPlotTot <- dataCondition()$cond.df[ ,c(condition.df()$misColCondition[i], dataCondition()$condMult.names[i])]
  condPlotTot$x <- 1:nrow(condPlotTot)

  cd1 <- condPlotTot[condPlotTot[2] == 1, ]
  cd0 <- condPlotTot[condPlotTot[2] == 0, ]

  ggplot() +
    geom_point(data = cd1, aes(x = x, y = cd1[,1]),
               shape = 21, color = "blue") +
    geom_point(data = cd0, aes(x = x, y = cd0[,1]),
               shape = 21, color = "red") +
    xlab("") + ylab(colnames(cd1)[1]) + theme_bw()

})

title1 <- ggpubr::text_grob(condition.df()$misColCondition[nPlot.reactive()], size = 15, face = "bold")
box(title = "Condition Total",
return(gridExtra::grid.arrange(myplots[[nPlot.reactive()]], top = title1))
#return(gridExtra::grid.arrange(grobs = myplots, nrow = input$nrowPlot, ncol = input$ncolPlot))
)
  }
})

# Updete the maximum value in the numericInput conditionPlotList
observe({
    updateNumericInput(session, "conditionPlotList",  max = length(condition.df()$misColCondition))
    })
