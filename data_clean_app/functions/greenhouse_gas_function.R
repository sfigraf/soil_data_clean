library(tidyverse)
library(lubridate)
gg_function <- function(gg_input, Node_name) {
  
  data_05 <- gg_input
  units <- as.character(data_05[1,])
  
  
  colnames1 <- colnames(data_05)
  #colnames1[1]
  
  
  new_names1 <- c()
  for(i in 1:length(colnames1)) {
    
    #print(i)
    new_name <- paste0(colnames1[i], "_",units[i])
    new_names1 <- c(new_names1, new_name)
    ## can't have return statement in for loop; that's why it wasn't running
    #return(new_names1)
    
  }
  
  colnames(data_05) <- new_names1
  #removeds first row
  data1 <- data_05[-c(1),]
  #changes to numeric types
  
  cols.num <- c(2:3,5:ncol(data1))
  #mutate_at in dplyr would also work
  data1[cols.num] <- sapply(data1[cols.num],as.numeric)
  
  data2 <- data1 %>%
    rename(DateTime1 = `DATE_TIME initial_value_NA`,
           `N2O_Flux[nmol+1m-2s-1]` = `FN2O_DRY_[nmol+1m-2s-1]`,
           `N2O Concentration[nmol+1mol-1]` = `N2O_DRY mean_[nmol+1mol-1]`,
           `CO2 Flux[nmol+1m-2s-1]` = `FCO2_DRY_[umol+1m-2s-1]`,
           `CO2 Concentration[umol+1mol-1]` = `CO2_DRY mean_[umol+1mol-1]`
    ) %>%
    mutate(DateTime2 = with_tz(DateTime1, tzone = "US/Central"),
           Date1 = date(DateTime2),
           node = Node_name,
    ) %>%
    select(Date1, DateTime2, node, `N2O_Flux[nmol+1m-2s-1]`,`N2O Concentration[nmol+1mol-1]`,
           `CO2 Flux[nmol+1m-2s-1]`,`CO2 Concentration[umol+1mol-1]`) %>%
    distinct()
  
  return(data2)
}

#x <- gg_function(Node_1, "Node 1")
