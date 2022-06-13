# Condition plots ----
output$cond1Plot <- renderPlot({
  if (isTRUE(input$plot.cond1) & isTRUE(input$cond1)) {
    box(title = "Condition 1",
        plotOutput(cond.plot(dataCondition(), condition = 1, title = "Condition 1"))
    )
  } else {
    showNotification("Check if Condition 1 and Condition 1 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

output$cond2Plot <- renderPlot({
  if (isTRUE(input$plot.cond2) & isTRUE(input$cond2)) {
    box(title = "Condition 2",
        plotOutput(cond.plot(dataCondition(), condition = 2, title = "Condition 2"))
    )
  } else {
    showNotification("Check if Condition 2 and Condition 2 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

output$cond3Plot <- renderPlot({
  if (isTRUE(input$plot.cond3) & isTRUE(input$cond3)) {
    box(title = "Condition 3",
        plotOutput(cond.plot(dataCondition(), condition = 3, title = "Condition 3"))
    )
  } else {
    showNotification("Check if Condition 3 and Condition 3 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

output$cond4Plot <- renderPlot({
  if (isTRUE(input$plot.cond4) & isTRUE(input$cond4)) {
    box(title = "Condition 4",
        plotOutput(cond.plot(dataCondition(), condition = 4, title = "Condition 4"))
    )
  } else {
    showNotification("Check if Condition 4 and Condition 4 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

output$cond5Plot <- renderPlot({
  if (isTRUE(input$plot.cond5) & isTRUE(input$cond5)) {
    box(title = "Condition 5",
        plotOutput(cond.plot(dataCondition(), condition = 5, title = "Condition 5"))
    )
  } else {
    showNotification("Check if Condition 5 and Condition 5 Plot are activetes", 
                     duration = 5, type = "warning", closeButton = TRUE)
  }
})

output$cond6Plot <- renderPlot({
  if (isTRUE(input$plot.cond6) & isTRUE(input$cond6)) {
    box(title = "Condition 6",
        plotOutput(cond.plot(dataCondition(), condition = 6, title = "Condition 6"))
    )
  } else {
    showNotification("Check if Condition 6 and Condition 6 Plot are activetes", 
                     duration = 6, type = "warning", closeButton = TRUE)
  }
})
