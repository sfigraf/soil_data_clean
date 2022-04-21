deep_moisture_function <- function(moisture_data) {
  
  data1 <- moisture_data %>%
    mutate(node = case_when(Plot=="A" ~ "Node 1",
                            Plot=="B" ~ "Node 2",
                            Plot=="C" ~ "Node 3",
                            Plot=="D" ~ "Node 4")) %>%
    filter(!is.na(node)) %>%
    distinct()
  
  d50_100 <- data1 %>%
    select(`Date & Time_m/d/yr format`,
           node, 
           `% Vol...6`
           
    ) %>%
    rename(`% Vol` = `% Vol...6`) %>%
    mutate(depth = "50-100")
  
  d200 <- data1 %>%
    select(`Date & Time_m/d/yr format`,
           node, 
           `% Vol...8`
           
    ) %>%
    rename(`% Vol` = `% Vol...8`) %>%
    mutate(depth = "100-200")
  
  d300 <- data1 %>%
    select(`Date & Time_m/d/yr format`,
           node, 
           `% Vol...10`
           
    ) %>%
    rename(`% Vol` = `% Vol...10`) %>%
    mutate(depth = "200-300")
  
  d400 <- data1 %>%
    select(`Date & Time_m/d/yr format`,
           node, 
           `% Vol...12`
           
    ) %>%
    rename(`% Vol` = `% Vol...12`) %>%
    mutate(depth = "300-400")
  
  d600 <- data1 %>%
    select(`Date & Time_m/d/yr format`,
           node, 
           `% Vol...14`
           
    ) %>%
    rename(`% Vol` = `% Vol...14`) %>%
    mutate(depth = "400-600")
  
  d1000 <- data1 %>%
    select(`Date & Time_m/d/yr format`,
           node, 
           `% Vol...16`
           
    ) %>%
    rename(`% Vol` = `% Vol...16`) %>%
    mutate(depth = "600-1000")
  
  d_list <- list(d50_100, d200, d300, d400,d600,d1000)
  
  deep_moisture <- bind_rows(d_list)
  
  #gets rid of all rows with a NA in them
  data3 <- deep_moisture %>%
    na.omit()
  
  return(data3)
}