########################################################
# Author: Tommaso Cancellario                          #
# Script Title: Functions                              # 
# Reviewer:                                            #
# Creation: 2022 - 05 - 18                             #
# Last update: 2022 - 06 - 08                         #
########################################################


# If object exists
obj.exist <- function(x){
  if(exists(x)){
    get(x)
  } else {NA}
}

# Not in
"%ni%" <- Negate("%in%")

# Filter conditional
conditional <- function(condition, success) {
  if (condition) success else TRUE
}


# Condition Plot
# x = Main dataset, y = condition dataset
cond.plot <- function(x, condition = NA, title = NA){
  
  y <- x[grepl(paste0("cd", condition), names(x))]
  colnames(y) <- c("Chl_S", "Chl_D", "PC", "PE")
  x$x <- 1:nrow(x)
  
  
  p1 <- ggplot(x)+
    geom_point(data = x[y$Chl_S == 1, ], aes(x = x, y = Chl_S), shape = 21, color = "blue") +
    geom_point(data = x[y$Chl_S == 0, ], aes(x = x, y = Chl_S), shape = 21, color = "red") +
    xlab("") + ylab("Chorophyll sup") + theme_bw()
  
  p2 <- ggplot(x)+
    geom_point(data = x[y$Chl_D == 1, ], aes(x = x, y = Chl_D), shape = 21, color = "blue") +
    geom_point(data = x[y$Chl_D == 0, ], aes(x = x, y = Chl_D), shape = 21, color = "red") +
    xlab("") + ylab("Chorophyll deep") + theme_bw()
  
  p3 <- ggplot(x)+
    geom_point(data = x[y$PC == 1, ], aes(x = x, y = PC), shape = 21, color = "blue") +
    geom_point(data = x[y$PC == 0, ], aes(x = x, y = PC), shape = 21, color = "red") +
    xlab("") + ylab("Phycocyanin") + theme_bw()
  
  p4 <- ggplot(x)+
    geom_point(data = x[y$PE == 1, ], aes(x = x, y = PE), shape = 21, color = "blue") +
    geom_point(data = x[y$PE == 0, ], aes(x = x, y = PE), shape = 21, color = "red") +
    xlab("") + ylab("Phycoerythrin") + theme_bw()
  
  return(gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2, top = title))
} 

# day and night plot
dn.plot <- function(x, latitude = 45.9283, longitude = 8.5554, title = NA){
  
  x$daySun <- as.character(cut(ymd_hms(x$datetimeisoformat), breaks="day"))
  x$hourSun <- as.character(cut(ymd_hms(x$datetimeisoformat), breaks="hour"))
  
  day.night <- data.frame()
  sun.df <- data.frame() 
  
  for(i in 1:length(unique(x$daySun))){
    
    sun <- getSunlightTimes(
      date = seq.Date(as.Date(as.character(unique(x$daySun)[i])), as.Date(as.character(unique(x$daySun)[i])), 
                      by = 1),
      keep = c("sunrise", "sunset"),
      lat = latitude,
      lon = longitude,
      tz = "UTC") # UTC: Coordinated Universal Time
    
    sun$sunrise <- as.character(ymd_hms(sun$sunrise))
    sun$sunset <- as.character(ymd_hms(sun$sunset))
    
    x.sub <- x[x$daySun %in% unique(x$daySun)[i], ]
    
    day.night <- rbind(day.night, x.sub)
    sun.df <- rbind(sun.df, sun)
  }
  
  misCol <- colnames(x)[colnames(x) %ni% c("datetimeisoformat", "year", "month", "day", 
                                           "hour", "minute", "second",
                                           "daySun", "hourSun")]
  
  myplots <- vector('list', length(misCol))
  for (i in 1:length(misCol)) {
    
    myplots[[i]] <- local({
      i <- i
      p1 <- ggplot()+
        #day
        geom_rect(data = sun.df, aes(xmin = ymd_hms(sunrise), xmax = ymd_hms(sunset), ymin = -Inf, ymax = Inf),
                  color = "#FF9900", fill = "#FFCC00", alpha = .4) +
        geom_point(data = day.night, aes(x = ymd_hms(datetimeisoformat), 
                                         y = day.night[ ,misCol[i]]), cex = .2) +
        xlab("") + ylab(misCol[i]) +
        labs(fill = "Day/Night") +
        theme_bw()
    })
  }
  
  return(gridExtra::grid.arrange(grobs = myplots, nrow = 2, ncol = 2, top = title))
  
} 