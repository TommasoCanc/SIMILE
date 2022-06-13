########################################################
# Author: Tommaso Cancellario                          #
# Script Title: Conditions                             # 
# Reviewer:                                            #
# Creation: 2022 - 05 - 18                             #
# Last update: 2022 - 06 - 08                          #
########################################################

# Checking condition

# Condition 1. Check if exists rows contain NA values
cond.1.fn <- function(x){
  cond.1 <- data.frame(ifelse(!is.na(x) == TRUE, 1, 0))
  colnames(cond.1) <- c("cd1_Chl_S", "cd1_Chl_D",
                        "cd1_PC", "cd1_PE")
  return(cond.1)
} 

# Condition 2. Check if exists rows contain 0
cond.2.fn <- function(x){
  cond.2 <- data.frame(ifelse((x) != 0, 1, 0))
  colnames(cond.2) <- c("cd2_Chl_S", "cd2_Chl_D",
                        "cd2_PC", "cd2_PE")
  return(cond.2)
  }

# Condition 3. Check values outside specific thresholds
cond.3.fn <- function(x,
                      Chl_S_min = NA,
                      Chl_S_max = NA,
                      Chl_D_min = NA,
                      Chl_D_max = NA,
                      PC_min = NA,
                      PC_max = NA,
                      PE_min = NA,
                      PE_max = NA,
                      inequality = ">") {
  
  if(inequality == ">"){
    cond.3 <- data.frame(
      cd3_Chl_S = ifelse(x$Chl_S > Chl_S_min & x$Chl_S < Chl_S_max, 1, 0),
      cd3_Chl_D = ifelse(x$Chl_D > Chl_D_min & x$Chl_D < Chl_D_max, 1, 0),
      cd3_PC = ifelse(x$PC > PC_min & x$PC < PC_max, 1, 0),
      cd3_PE = ifelse(x$PE > PE_min & x$PE < PE_max, 1, 0)
    )
  }
  
  if(inequality == ">="){
    cond.3 <- data.frame(
      cd3_Chl_S = ifelse(x$Chl_S >= Chl_S_min & x$Chl_S <= Chl_S_max, 1, 0),
      cd3_Chl_D = ifelse(x$Chl_D >= Chl_D_min & x$Chl_D <= Chl_D_max, 1, 0),
      cd3_PC = ifelse(x$PC >= PC_min & x$PC <= PC_max, 1, 0),
      cd3_PE = ifelse(x$PE >= PE_min & x$PE <= PE_max, 1, 0)
    )
  }
  
  return(cond.3)
  
}

# Condition 4. Check values outside 3 standard deviation.
cond.4.fn <- function(x) {
    
    min.val_Chl_S <- mean(x$Chl_S) - (3*sd(x$Chl_S))
    max.val_Chl_S <- mean(x$Chl_S) + (3*sd(x$Chl_S))
    
    min.val_Chl_D <- mean(x$Chl_D) - (3*sd(x$Chl_D))
    max.val_Chl_D <- mean(x$Chl_D) + (3*sd(x$Chl_D))
    
    min.val_PC <- mean(x$PC) - (3*sd(x$PC))
    max.val_PC <- mean(x$PC) + (3*sd(x$PC))
    
    min.val_PE <- mean(x$PE) - (3*sd(x$PE))
    max.val_PE <- mean(x$PE) + (3*sd(x$PE))
    
    cond.4 <- data.frame(
      cd4_Chl_S = ifelse(x$Chl_S > min.val_Chl_S & x$Chl_S < max.val_Chl_S, 1, 0),
      cd4_Chl_D = ifelse(x$Chl_D > min.val_Chl_D & x$Chl_D < max.val_Chl_D, 1, 0),
      cd4_PC = ifelse(x$PC > min.val_PC & x$PC < max.val_PC, 1, 0),
      cd4_PE = ifelse(x$PE > min.val_PE & x$PE < max.val_PE, 1, 0)
    )
  }

# Condition 6 Median Absolute Deviation
cond.5.fn <- function(x){
  
  min.mad_Chl_S <- mean(x$Chl_S) - (3*mad(x$Chl_S))
  max.val_Chl_S <- mean(x$Chl_S) + (3*mad(x$Chl_S))
  
  min.val_Chl_D <- mean(x$Chl_D) - (3*mad(x$Chl_D))
  max.val_Chl_D <- mean(x$Chl_D) + (3*mad(x$Chl_D))
  
  min.val_PC <- mean(x$PC) - (3*mad(x$PC))
  max.val_PC <- mean(x$PC) + (3*mad(x$PC))
  
  min.val_PE <- mean(x$PE) - (3*mad(x$PE))
  max.val_PE <- mean(x$PE) + (3*mad(x$PE))
  
  cond.5 <- data.frame(
    cd6_Chl_S = ifelse(x$Chl_S > min.val_Chl_S & x$Chl_S < max.val_Chl_S, 1, 0),
    cd6_Chl_D = ifelse(x$Chl_D > min.val_Chl_D & x$Chl_D < max.val_Chl_D, 1, 0),
    cd6_PC = ifelse(x$PC > min.val_PC & x$PC < max.val_PC, 1, 0),
    cd6_PE = ifelse(x$PE > min.val_PE & x$PE < max.val_PE, 1, 0)
  )
}

# Condition 5 outlier function
cond.6.fn <- function(x){
  
  out.high.Chl_S <- outlier(x$Chl_S, opposite = FALSE)
  out.low.Chl_S <- outlier(x$Chl_S, opposite = TRUE)
  
  out.high.Chl_D <- outlier(x$Chl_D, opposite = FALSE)
  out.low.Chl_D <- outlier(x$Chl_D, opposite = TRUE)
  
  out.high.PC <- outlier(x$PC, opposite = FALSE)
  out.low.PC <- outlier(x$PC, opposite = TRUE)
  
  out.high.PE <- outlier(x$PE, opposite = FALSE)
  out.low.PE <- outlier(x$PE, opposite = TRUE)
  
  cond.6 <- data.frame(
    cd5_Chl_S = ifelse(x$Chl_S > out.low.Chl_S & x$Chl_S < out.high.Chl_S, 1, 0),
    cd5_Chl_D = ifelse(x$Chl_D > out.low.Chl_D & x$Chl_D < out.high.Chl_D, 1, 0),
    cd5_PC = ifelse(x$PC > out.low.PC & x$PC < out.high.PC, 1, 0),
    cd5_PE = ifelse(x$PE > out.low.PE & x$PE < out.high.PE, 1, 0)
  )
}