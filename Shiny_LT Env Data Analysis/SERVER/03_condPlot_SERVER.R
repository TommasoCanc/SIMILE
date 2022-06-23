# Condition plots ----
# Plot contidion 1
output$cond1Plot <- renderPlot({
  if (isTRUE(input$plot.cond1) && isTRUE(input$cond1)) {
    
    y <- dataCondition()$cond.df[grepl(paste0("c1_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(dataIn()$misCol)]
    x <- cbind(x, y)
    
    box(title = "Condition 1",
        plotOutput(cond.plot(x, condition = 1, title = "Condition 1", f.ncol = input$ncolPlot, f.nrow = input$nrowPlot))
    )
  } else {
    showNotification("Check if Condition 1 and Condition 1 Plot are activetes",
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 2
output$cond2Plot <- renderPlot({
  if (isTRUE(input$plot.cond2) && isTRUE(input$cond2)) {
    
    y <- dataCondition()$cond.df[grepl(paste0("c2_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(dataIn()$misCol)]
    x <- cbind(x, y)
    
    box(title = "Condition 2",
        plotOutput(cond.plot(x, condition = 2, title = "Condition 2", f.ncol = input$ncolPlot, f.nrow = input$nrowPlot))
    )
  } else {
    showNotification("Check if Condition 2 and Condition 2 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 3
output$cond3Plot <- renderPlot({
  if (isTRUE(input$plot.cond3) && isTRUE(input$cond3)) {
    
    y <- dataCondition()$cond.df[grepl(paste0("c3_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(dataIn()$misCol)]
    x <- cbind(x, y)
    
    box(title = "Condition 3",
        plotOutput(cond.plot(x, condition = 3, title = "Condition 3", f.ncol = input$ncolPlot, f.nrow = input$nrowPlot))
    )
  } else {
    showNotification("Check if Condition 3 and Condition 3 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 4
output$cond4Plot <- renderPlot({
  if (isTRUE(input$plot.cond4) && isTRUE(input$cond4)) {
    
    y <- dataCondition()$cond.df[grepl(paste0("c4_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(dataIn()$misCol)]
    x <- cbind(x, y)
    
    box(title = "Condition 4",
        plotOutput(cond.plot(x, condition = 4, title = "Condition 4", f.ncol = input$ncolPlot, f.nrow = input$nrowPlot))
    )
  } else {
    showNotification("Check if Condition 4 and Condition 4 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Pélot condition 5
output$cond5Plot <- renderPlot({
  if (isTRUE(input$plot.cond5) && isTRUE(input$cond5)) {
    
    y <- dataCondition()$cond.df[grepl(paste0("c5_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(dataIn()$misCol)]
    x <- cbind(x, y)
    
    box(title = "Condition 5",
        plotOutput(cond.plot(x, condition = 5, title = "Condition 5", f.ncol = input$ncolPlot, f.nrow = input$nrowPlot))
    )
  } else {
    showNotification("Check if Condition 5 and Condition 5 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

# Plot condition 6
output$cond6Plot <- renderPlot({
  if (isTRUE(input$plot.cond6) && isTRUE(input$cond6)) {
    
    y <- dataCondition()$cond.df[grepl(paste0("c6_"), names(dataCondition()$cond.df))]
    x <- dataCondition()$cond.df[, c(dataIn()$misCol)]
    x <- cbind(x, y)
    
    box(title = "Condition 6",
        plotOutput(cond.plot(x, condition = 6, title = "Condition 6", f.ncol = input$ncolPlot, f.nrow = input$nrowPlot))
    )
  } else {
    showNotification("Check if Condition 6 and Condition 6 Plot are activetes", 
                     duration = 6, type = "warning", closeButton = TRUE)
  }
})

# Plot Total
output$totPlot <- renderPlot({
  if (isTRUE(input$plot.tot)) {
    
    cd1 <-dataCondition()$cond.df["cond.mult" == 1, ]
    cd0 <-dataCondition()$cond.df["cond.mult" == 0, ]

    box(title = "Condition Total",
        plotOutput(
    ggplot() +
      geom_point(data = cd1, aes(x = x, y = cd1[,i]),
                 shape = 21, color = "blue") +
      geom_point(data = cd0, aes(x = x, y = cd0[,i]),
                 shape = 21, color = "red") +
      xlab("") + ylab(colnames(cd1)[i]) + theme_bw()
        )
    )
  }
})