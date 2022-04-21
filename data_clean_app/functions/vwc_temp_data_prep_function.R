vwc_temp_prep_function <- function(vwc_temp_excel_data, sheet_name) {
  
  data_05 <- vwc_temp_excel_data
  #gets a string of units to add onto column names
  units <- as.character(data_05[1,])
  #units
  units2 <- str_replace(units, c("unitless|NA"), "")
  
  colnames1 <- colnames(data_05)
  
  new_names1 <- c()
  for(i in 1:length(colnames1)) {
    
    #print(i)
    new_name <- paste0(colnames1[i], "_",units2[i])
    new_names1 <- c(new_names1, new_name)
    ## can't have return statement in for loop; that's why it wasn't running
    #return(new_names1)
    
  }
  
  colnames(data_05) <- new_names1
  
  #takes off first two rows
  data1 <- vwc[-c(1:2),]
  
  return(data1)
  
}

