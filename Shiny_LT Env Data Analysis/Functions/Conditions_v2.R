########################################################
# Author: Tommaso Cancellario                          #
# Script Title: Conditions                             # 
# Reviewer:                                            #
# Creation: 2022 - 05 - 18                             #
# Last update: 2022 - 06 - 12                          #
########################################################

# # column different form the temporal
# misCol <- colnames(mainTable.df)[colnames(mainTable.df) %ni% c("datetimeisoformat", "year", "month", "day", "hour", "minute", "second")]

# Checking condition

# Condition 1. Check if exists rows contain NA values
cond.1.fn <- function(x){
  cond.1 <- data.frame(ifelse(!is.na(x) == TRUE, 1, 0))
  for(j in 1:ncol(x)){colnames(cond.1)[j] <- paste0("c1_", colnames(x)[j])}
  return(cond.1)
} 

# Condition 2. Check if exists rows contain 0
cond.2.fn <- function(x){
  cond.2 <- data.frame(ifelse(x != 0, 1, 0))
  for(j in 1:ncol(x)){colnames(cond.2)[j] <- paste0("c2_", colnames(x)[j])}
  return(cond.2)
  }

# Condition 3. Check values outside specific thresholds
# cond.3.fn <- function(x,
#                       t1.min = NA,
#                       t1.max = NA,
#                       t2.min = NA,
#                       t2.max = NA,
#                       t3.min = NA,
#                       t3.max = NA,
#                       t4.min = NA,
#                       t4.max = NA,
#                       inequality = ">") {
  
#   if(inequality == ">"){
#     cond.3 <- data.frame(
#       t1 = ifelse(x[ ,1] > t1.min & x[ ,1] < t1.max, 1, 0),
#       t2 = ifelse(x[ ,2] > t2.min & x[ ,2] < t2.max, 1, 0),
#       t3 = ifelse(x[ ,3] > t3.min & x[ ,3] < t3.max, 1, 0),
#       t4 = ifelse(x[ ,4] > t4.min & x[ ,4] < t4.max, 1, 0)
#     )
#   }
  
#   if(inequality == ">="){
#     cond.3 <- data.frame(
#       t1 = ifelse(x[ ,1] >= t1.min & x[ ,1] <= t1.max, 1, 0),
#       t2 = ifelse(x[ ,2] >= t2.min & x[ ,2] <= t2.max, 1, 0),
#       t3 = ifelse(x[ ,3] >= t3.min & x[ ,3] <= t3.max, 1, 0),
#       t4 = ifelse(x[ ,4] >= t4.min & x[ ,4] <= t4.max, 1, 0)
#     )
#   }
  
#   for(j in 1:ncol(x)){colnames(cond.3)[j] <- paste0("c3_", colnames(x)[j])}
  
#   return(cond.3)
  
# }

# Condition 4. Check values outside 3 standard deviation.
cond.4.fn <- function(x) {
  
  colMean <- x %>% summarise_if(is.numeric, mean, na.rm = TRUE) 
  colSd <- x %>% summarise_if(is.numeric, sd, na.rm = TRUE)
  
  tMax <- colMean + (3*colSd)
  tMin <- colMean - (3*colSd)
  
  cond.4 <- data.frame(row.names = 1:nrow(x))
  
  for(i in 1:ncol(x)){
    cond.4.1 <- data.frame(ifelse(x[ ,i] > tMin[ ,i] & x[ ,i] < tMax[ ,i], 1, 0))
    cond.4 <- cbind(cond.4, cond.4.1)  
  }
  
  for(j in 1:ncol(x)){colnames(cond.4)[j] <- paste0("c4_", colnames(x)[j])}
  
  return(cond.4)
}

# Condition 5 Median Absolute Deviation
cond.5.fn <- function(x){
  
  colMean <- x %>% summarise_if(is.numeric, mean, na.rm = TRUE) 
  colMad <- x %>% summarise_if(is.numeric, mad, na.rm = TRUE)
  
  tMax <- colMean + (3*colMad)
  tMin <- colMean - (3*colMad)
  
  cond.5 <- data.frame(row.names = 1:nrow(x))
  
  for(i in 1:ncol(x)){
    cond.5.1 <- data.frame(ifelse(x[ ,i] > tMin[ ,i] & x[ ,i] < tMax[ ,i], 1, 0))
    cond.5 <- cbind(cond.5, cond.5.1)  
  }
  
  for(j in 1:ncol(x)){colnames(cond.5)[j] <- paste0("c5_", colnames(x)[j])}
  
  return(cond.5)
}

# Condition 6 outlier function
cond.6.fn <- function(x){
  
  out.high <- outlier(x, opposite = FALSE)
  out.low <- outlier(x, opposite = TRUE)
  
  cond.6 <- data.frame(row.names = 1:nrow(x))
  
  for(i in 1:ncol(x)){
    cond.6.1 <- data.frame(ifelse(x[ ,i] > out.low[i] & x[ ,i] < out.high[i], 1, 0))
    cond.6 <- cbind(cond.6, cond.6.1)  
  }
  
  for(j in 1:ncol(x)){colnames(cond.6)[j] <- paste0("c6_", colnames(x)[j])}
  
  return(cond.6)
}