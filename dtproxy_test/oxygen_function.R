#### data cleaning function
#takes excel sheet, renamens columnns, gets rid of values below certain threshhold
# this function is a base cleaner function: other filters on sidebar will be used for more specific filters
#sheet_name = "Node 1"
clean_sheet_function <- function(excel_sheet_data, sheet_name) {
  
  #each sheet doesn't need first two rows and deson't need all columns
  data1 <- excel_sheet_data[-c(1:2),c(1,5:14)]
  
  #renaming columns and gets rid of duplicate entries
  data2 <- data1 %>%
    rename(O2_1_5cm = `O2_Avg(1)`,
           O2_1_15cm = `O2_Avg(2)`,
           O2_2_5cm = `O2_Avg(3)`,
           O2_2_15cm = `O2_Avg(4)`,
           O2_3_5cm = `O2_Avg(5)`,
           O2_3_15cm = `O2_Avg(6)`,
           O2_4_5cm = `O2_Avg(7)`,
           O2_4_15cm = `O2_Avg(8)`,
           O2_5_5cm = `O2_Avg(9)`,
           O2_5_15cm = `O2_Avg(10)`) %>%
    distinct()
  
  #turns all "NaN" entries in "numeric" type columns to NA
  data3 <- data2 %>%
    mutate(across(where(is.numeric), ~na_if(., "NaN")))
  
  #changes data layout to make it easier to identify problem entries
  data4 <- data3 %>%
    gather(key = "sensor_number_and_depth", value = "value1", -TIMESTAMP) %>%
    mutate(depth = str_sub(sensor_number_and_depth, 6, -1),
           node = as.character(sheet_name))
  
  #getting "problem entries": in this case, if value is between June and july and value is less than 10, it's a mistake and should be taken out
  # but values <10 in january are expected
  
  #this gets df of problem rows
  #take this out later for more specific filters
  # problem_rows <- data4 %>%
  #   #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
  #   filter((TIMESTAMP >= as.Date("2021-06-01") & TIMESTAMP <= as.Date("2021-07-10")),
  #          value1 <10)
  # #this takes these rows out of the main dataframe
  # data6 <- anti_join(data4, problem_rows)
  
  #in general only want oxygen values under 22
  #also prepares new columns for summarizing
  #can also just put this in "datasummaries" code
 # data8 <- data4 %>%
    #filter(value1 < 22) %>% #taken out for more specific filters in app
    
  
  
  
  #data_list <- list("clean_data" = data4)
  
  return(data4)
  
}

# x <- clean_sheet_function(data, "Node 1")
# excel_sheet_data <- data
# sheet_name <- "Node 1"

# # x$depth_timestamp_summaries