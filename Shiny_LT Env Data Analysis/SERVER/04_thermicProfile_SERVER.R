# Thermich Profile ----
# Check the file 02_condSelection_SERVER.R for the main table

dataThermic <- reactive({

if(isTRUE(input$runThermic) && isTRUE(input$checkAgr)){
tchain <- dataAggregation()
#misColCondition <- condition.df()$misColCondition  dataIn()$misCol

depth <- as.numeric(unlist(strsplit(input$depthsThermic, ",")))


tchain.t <- as.data.frame(t(tchain[ ,8:length(tchain)]))
colnames(tchain.t) <- tchain$datetimeisoformat

tchain.t <- gather(tchain.t, datehour, temp, colnames(tchain.t[ ,1:ncol(tchain.t)]))
tchain.t$depth <- rep(depth, length(unique(tchain.t$datehour)))

tchain.t$datehour <- ymd_hms(tchain.t$datehour)

tchain.td <- data.frame()
intersection <- data.frame()

for(i in 1:length(unique(tchain.t$datehour))) {
  
  tchain.t.sub <- tchain.t[tchain.t$datehour == unique(tchain.t$datehour)[i], ]
  
  tchain.td.1 <- data.frame(datetimeisoformat = unique(tchain.t$datehour)[i],
                            sesonalThermo = round(thermo.depth(tchain.t.sub$temp, tchain.t.sub$depth, Smin = input$SminThermic, seasonal = T, index = input$indexThermic, 
                            mixed.cutoff = input$mixedCutoffThermic), digits = 2),
                            maxThermo = round(thermo.depth(tchain.t.sub$temp, tchain.t.sub$depth, Smin = input$SminThermic, seasonal = F, index = input$indexThermic, 
                            mixed.cutoff = input$mixedCutoffThermic), digits = 2)
  )
  
  f1 <- approxfun(tchain.t.sub$temp, tchain.t.sub$depth)

  pSesonal <- optimize(function(t0) abs(f1(t0) - tchain.td.1$sesonalThermo), interval = range(tchain.t.sub$temp))
  pMax <- optimize(function(t0) abs(f1(t0) - tchain.td.1$maxThermo), interval = range(tchain.t.sub$temp))
  
  intersection.1 <- data.frame(datetimeisoformat = unique(tchain.t$datehour)[i],
                               sesonalThermo = pSesonal$minimum,
                               maxThermo = pMax$minimum)

  tchain.td <- rbind(tchain.td, tchain.td.1)
  intersection <- rbind(intersection, intersection.1)
}


return(list(tchain.t = tchain.t,
            tchain.td = tchain.td,
            intersection = intersection)
)

} else {
   showNotification("Data have to be aggregated and/or depths need to be provided.",
                         duration = 5, type = "warning", closeButton = TRUE)
}

})

# Output Main information output ----
output$thermicData <- renderUI({
  if(isTRUE(input$runThermic) && isTRUE(input$checkAgr)) {
    column(
      width = 12,
      HTML("<h2>Thermic Table</h2>"),
    box(
      title = "Dayly thermic profile", width = 12,
      DT::renderDataTable(
        dataThermic()$tchain.td,
        options = list(scrollX = TRUE, scrollY = "150px", paging = FALSE),
        rownames = FALSE
      ),
      br(),
      downloadHandler(
        filename = function() {
          paste(input$dataSelection, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
          write.csv(dataThermic()$tchain.td, con, row.names = FALSE)
        }
      )
    )
      )
  } else {
HTML("Remember: Fill depth parameter")
}
})

# Plot Thermic profile

output$thermicPlot.1 <- renderPlot({
if (isTRUE(input$runThermic) && isTRUE(input$checkAgr)) {

cc <- scales::seq_gradient_pal("red", "blue")(seq(0, 1, length.out = nrow(dataThermic()$tchain.td))) #input$thermicColourPlot

ggplot(data = dataThermic()$tchain.t, aes(x = temp, y = depth, colour = as.factor(datehour))) +
geom_point() + geom_line() +
  #geom_hline(data = dataThermic()$tchain.td, aes(yintercept = sesonalThermo, colour = as.factor(datetimeisoformat))) +
  scale_y_reverse() +
  scale_colour_manual(values=cc) +
  xlab("Temperature (°C)") + ylab("Depth (m)") +
  labs(colour = "Time agr") +
  theme_bw() +
  annotate("point", x = dataThermic()$intersection$sesonalThermo, y = dataThermic()$tchain.td$sesonalThermo, colour = "#d8b365", size = 2) +
  annotate("point", x = dataThermic()$intersection$maxThermo, y = dataThermic()$tchain.td$maxThermo, colour = "black", size = 2)
}
})

output$thermicPlot <- renderUI({
  if (isTRUE(input$runThermic) && isTRUE(input$checkAgr)) {
column(width = 12,
plotOutput("thermicPlot.1")
)
  }
})
