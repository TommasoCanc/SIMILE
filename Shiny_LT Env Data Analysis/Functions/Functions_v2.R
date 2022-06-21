########################################################
# Author: Tommaso Cancellario                          #
# Script Title: Functions                              # 
# Reviewer:                                            #
# Creation: 2022 - 05 - 18                             #
# Last update: 2022 - 06 - 16                         #
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
cond.plot <- function(x, condition = NA, title = NA, f.ncol = 2, f.nrow = 2){
  
  y <- x[grepl(paste0("c", condition, "_"), names(x))]
  x$x <- 1:nrow(x)
  
  myplots <- lapply(1:length(y), function(i) {
    
    cd1 <-x[y[i] == 1, ]; cd1 <- cd1[,!(names(cd1) %in% colnames(y))]
    cd0 <-x[y[i] == 0, ]; cd0 <- cd0[,!(names(cd0) %in% colnames(y))]
    
    ggplot() +
      geom_point(data = cd1, aes(x = x, y = cd1[,i]),
                 shape = 21, color = "blue") +
      geom_point(data = cd0, aes(x = x, y = cd0[,i]),
                 shape = 21, color = "red") +
      xlab("") + ylab(colnames(cd1)[i]) + theme_bw()
    
  })
  
  return(gridExtra::grid.arrange(grobs = myplots, nrow = f.ncol, ncol = f.nrow, top = title))
} 

# day and night plot
dn.plot <- function(x, latitude = 45.9283, longitude = 8.5554, title = NA, f.ncol = 2, f.nrow = 2){
  
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
  
  return(gridExtra::grid.arrange(grobs = myplots, nrow = f.nrow, ncol = f.ncol, top = title))
  
} 