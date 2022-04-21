



vwc_function <- function(prepped_vwc_data_input, sheet_name) {
  data1 <- prepped_vwc_data_input
  # this is where vwc specific stuff starts
  data2 <- data1 %>%
    select(TIMESTAMP_,
           `VWC1_Avg_m^3/m^3`,
           `VWC3_Avg_m^3/m^3`,
           `VWC5_Avg_m^3/m^3`,
           `VWC7_Avg_m^3/m^3`, 
           `VWC9_Avg_m^3/m^3`,#5 cm
           
           `VWC0_Avg_m^3/m^3`,
           `VWC8_Avg_m^3/m^3`,
           `VWC6_Avg_m^3/m^3`,
           `VWC4_Avg_m^3/m^3`,
           `VWC2_Avg_m^3/m^3` #15 cm
    ) %>%
    rename(
      `VWC1_Avg_m^3/m^3_5cm` = `VWC1_Avg_m^3/m^3`,
      `VWC3_Avg_m^3/m^3_5cm` =          `VWC3_Avg_m^3/m^3`,
      `VWC5_Avg_m^3/m^3_5cm` =          `VWC5_Avg_m^3/m^3`,
      `VWC7_Avg_m^3/m^3_5cm` =          `VWC7_Avg_m^3/m^3`, 
      `VWC9_Avg_m^3/m^3_5cm` =          `VWC9_Avg_m^3/m^3`,
      
      `VWC0_Avg_m^3/m^3_15cm` = `VWC0_Avg_m^3/m^3`,
      `VWC8_Avg_m^3/m^3_15cm` =          `VWC8_Avg_m^3/m^3`,
      `VWC6_Avg_m^3/m^3_15cm` =          `VWC6_Avg_m^3/m^3`,
      `VWC4_Avg_m^3/m^3_15cm` =          `VWC4_Avg_m^3/m^3`,
      `VWC2_Avg_m^3/m^3_15cm` =          `VWC2_Avg_m^3/m^3` 
      
    ) %>%
    distinct()
  

  #makes "NaN" to NA's, then filters out NA rows across all columns
  data3 <- data2 %>%
    mutate(across(where(is.character), ~na_if(., "NaN"))) %>%
    filter_all(any_vars(!is.na(.))) 
  
  data4 <- data3 %>%
    gather(key = "sensor_number_and_depth", value = "VWC_Avg_m^3/m^3", -TIMESTAMP_) %>%
    mutate(depth = str_sub(sensor_number_and_depth,18, -1),
           node = as.character(sheet_name)
    ) %>%
    #makes VWC_Avg_m^3/m^3 column numeric
    mutate_at('VWC_Avg_m^3/m^3', as.numeric)
  
  #df_list = list()
  
  return(data4)
}

# prepped_data <- vwc_temp_prep_function(vwc)
# data4_1 <- vwc_function(prepped_data)
