soil_temp_function <- function(prepped_soil_data, sheet_name) {
  
  data2 <- prepped_soil_data %>%
    select(TIMESTAMP_, 
           #5cm
           `SoilT1_Avg_Deg C`, 
           `SoilT3_Avg_Deg C`, 
           `SoilT5_Avg_Deg C`, 
           `SoilT7_Avg_Deg C`,
           `SoilT9_Avg_Deg C`,
           
           #15 cm
           `SoilT2_Avg_Deg C`,
           `SoilT4_Avg_Deg C`,
           `SoilT6_Avg_Deg C`,
           `SoilT8_Avg_Deg C`,
           `SoilT0_Avg_Deg C`) %>%
    rename(
      `SoilT1_Avg_Deg C_5cm`  = `SoilT1_Avg_Deg C`,
      `SoilT3_Avg_Deg C_5cm`  = `SoilT3_Avg_Deg C`,
      `SoilT5_Avg_Deg C_5cm`  = `SoilT5_Avg_Deg C`,
      `SoilT7_Avg_Deg C_5cm` = `SoilT7_Avg_Deg C`,
      `SoilT9_Avg_Deg C_5cm` = `SoilT9_Avg_Deg C`,
      
      #15 cm
      `SoilT2_Avg_Deg C_15cm` = `SoilT2_Avg_Deg C`,
      `SoilT4_Avg_Deg C_15cm` = `SoilT4_Avg_Deg C`,
      `SoilT6_Avg_Deg C_15cm` = `SoilT6_Avg_Deg C`,
      `SoilT8_Avg_Deg C_15cm` = `SoilT8_Avg_Deg C`,
      `SoilT0_Avg_Deg C_15cm`  = `SoilT0_Avg_Deg C`) %>%
    distinct()
  
  #makes "NaN" to NA's, then filters out NA rows across all columns
  data3 <- data2 %>%
    mutate(across(where(is.character), ~na_if(., "NaN"))) %>%
    filter_all(any_vars(!is.na(.))) 
  
  data4 <- data3 %>%
    gather(key = "sensor_number_and_depth", value = "SoilT_Avg_DegCelsius", -TIMESTAMP_) %>%
    mutate(depth = str_sub(sensor_number_and_depth,18, -1),
           node = as.character(sheet_name)
    ) %>%
    mutate_at('SoilT_Avg_DegCelsius', as.numeric)
  
  return(data4)
}