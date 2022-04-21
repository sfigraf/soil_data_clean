library(tidyverse)
library(lubridate)
library(plotly)
#alt + left mouse click down to do multi curosr editing

library(readxl)

data <- read_excel("O2 temp and VWC data_all nodes_June21-Feb22.xlsx", 
                                                          sheet = "Node 1", col_types = c("date", 
                                                                                          "text", "text", "text", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "numeric", "numeric", "numeric", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text"), 
                   
                                                          skip = 1)
data1 <- data[-c(1:2),c(1,5:14)]


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

data3 <- data2 %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

data4 <- data3 %>%
  gather(key = "sensor_number_and_depth", value = "value1", -TIMESTAMP)

problem_rows <- data4 %>%
  #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
  filter((TIMESTAMP >= as.Date("2021-06-01") & TIMESTAMP <= as.Date("2021-07-10")),
         value1 <10)

data6 <- anti_join(data4, problem_rows)
#specific points to take out
problem_points <- data6 %>%
  #mutate(new_time1 = with_tz(TIMESTAMP, tzone = "US/Central")) %>%
  filter(sensor_number_and_depth == "O2_2_15cm" & value1 <15)

data7 <- anti_join(data6, problem_points)
#in general only want oxygen values under 22
data8 <- data7 %>%
  filter(value1 < 22) %>%
  mutate(depth = str_sub(sensor_number_and_depth, 6, -1),
         node = "Node1")
  

data_summaries <- data8 %>%
  group_by(depth, TIMESTAMP) %>%
  #mutate(avg2 = mean(value1))
  summarise(avg1 = mean(value1)) %>%
  mutate(node = "Node1")





# data4_1 <- data4 %>%
#   #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
#   dplyr::filter((TIMESTAMP >= as.Date("2021-06-01")))

# 
ggplot(data8) +
  aes(x = TIMESTAMP, y = value1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(data8$sensor_number_and_depth)) +
  labs(title = "Cleaned data visual")

ggplot(data4) +
  aes(x = TIMESTAMP, y = value1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(data4$sensor_number_and_depth)) +
  labs(title = "Not Cleaned data visual")

x <- data3 %>%
  ggplot(aes(x = TIMESTAMP, y = O2_2_15cm)) +
  geom_point()

ggplotly(x)


  #na_if(O2_1_5cm, "NaN")
  #filter()



# Combining data code -----------------------------------------------------
names <- list("Cleaned_data_Node1.csv", "Cleaned_data_Node2.csv","Cleaned_data_Node3.csv","Cleaned_data_Node4.csv")

node1_clean <- read_csv("Cleaned_data_Node1.csv")
node2_clean <- read_csv("Cleaned_data_Node2.csv")
node3_clean <- read_csv("Cleaned_data_Node3.csv")
node4_clean <- read_csv("Cleaned_data_Node4.csv")

df_list <- list(node1_clean,node2_clean,node3_clean,node4_clean)

all <- bind_rows(df_list)

rbindlist(lapply(df_list, read_csv),
          use.names = TRUE, fill = TRUE)

x <- lapply(names, read_csv)
all <- bind_rows(x)

??rbindlist
library(data.table)


# Greenhouse gas stuff ----------------------------------------------------

library(readxl)
Node_1 <- read_excel("data_clean_app/Node 1_cleaned_021722.xlsx", 
                                    sheet = "Node 1_cleaned_021722", col_types = c("date", 
                                                                                   "text", "text", "text", "text", "text", 
                                                                                   "text", "text", "text", "text", "text", 
                                                                                   "text", "text", "text", "text", "text", 
                                                                                   "text", "text", "text", "text", "text", 
                                                                                   "text", "text", "text", "text"), 
                                    skip = 1)

#changing column names so they have units in them
units <- as.character(Node_1[1,])
units

colnames1 <- colnames(Node_1)
#colnames1[1]


new_names1 <- c()
for(i in 1:length(colnames1)) {
  
  #print(i)
  new_name <- paste0(colnames1[i], "_",units[i])
  new_names1 <- c(new_names1, new_name)
  ## can't have return statement in for loop; that's why it wasn't running
  #return(new_names1)
  
}

colnames(Node_1) <- new_names1
#removeds first row
data1 <- Node_1[-c(1),]
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
         #make new column here to correspond to Node
         ) %>%
  select(Date1, DateTime2, `N2O_Flux[nmol+1m-2s-1]`,`N2O Concentration[nmol+1mol-1]`,
         `CO2 Flux[nmol+1m-2s-1]`,`CO2 Concentration[umol+1mol-1]`)




# i = 1
# for (i in 1:nrow(Node_1)) {
#   units <- units <- as.character(Node_1[1,])
#   colnames1 <- colnames(Node_1)
#   
#   column_name = as.character(colnames1[i])
#   
#   new_column <- paste(column_name, units[i])
#   Node2 <- Node_1 %>%
#     rename( = column_name[i])
# }


x %>%
  ggplot(aes(x = DateTime2, y = `N2O_Flux[nmol+1m-2s-1]`)) +
  geom_point() +
  labs(title = "N2O Flux") +
  theme_classic()

x %>%
  ggplot(aes(x = DateTime2, y = `N2O Concentration[nmol+1mol-1]`)) +
  geom_point() +
  labs(title = "N2O Concentration") +
  theme_classic()

x %>%
  ggplot(aes(x = DateTime2, y = `CO2 Flux[nmol+1m-2s-1]`)) +
  geom_point() +
  labs(title = "CO2 Flux") +
  theme_classic()

x %>%
  ggplot(aes(x = DateTime2, y = `CO2 Concentration[umol+1mol-1]`)) +
  geom_point() +
  labs(title = "CO2 Concentration") +
  theme_classic()

# Soil VWC Data -----------------------------------------------------------

vwc <- read_excel("data_clean_app/O2 temp and VWC data_all nodes_June21-Feb22.xlsx", 
                                                          sheet = "Node 1_", col_types = c("date", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text"), skip = 1)


units <- as.character(vwc[1,])
#units
units2 <- str_replace(units, c("unitless|NA"), "")
units2

colnames1 <- colnames(vwc)

#str_replace_all(input$nodename1, fixed(" "), "")
#colnames1[1]


new_names1 <- c()
for(i in 1:length(colnames1)) {
  
  #print(i)
  new_name <- paste0(colnames1[i], "_",units2[i])
  new_names1 <- c(new_names1, new_name)
  ## can't have return statement in for loop; that's why it wasn't running
  #return(new_names1)
  
}

colnames(vwc) <- new_names1

cols5cm <- c(`VWC1_Avg_m^3/m^3`,`VWC3_Avg_m^3/m^3`,`VWC5_Avg_m^3/m^3`,
             `VWC7_Avg_m^3/m^3`, `VWC9_Avg_m^3/m^3`)

cols15cm <- c(`VWC0_Avg_m^3/m^3`,`VWC8_Avg_m^3/m^3`,`VWC6_Avg_m^3/m^3`,
              `VWC4_Avg_m^3/m^3`,`VWC2_Avg_m^3/m^3`)
#takes off first two rows
data1 <- vwc[-c(1:2),]
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

#turn column types to numeric
# data3 <- data2 %>%
#   select(-TIMESTAMP_) %>%
#   mutate_at(c(2:length(data2)),as.numeric) 
#x <- data2[!complete.cases(data2), ]
#makes "NaN" to NA's, then filters out NA rows across all columns
data3 <- data2 %>%
  mutate(across(where(is.character), ~na_if(., "NaN"))) %>%
  filter_all(any_vars(!is.na(.))) 

data4 <- data3 %>%
  gather(key = "sensor_number_and_depth", value = "VWC_Avg_m^3/m^3", -TIMESTAMP_) %>%
  mutate(depth = str_sub(sensor_number_and_depth,18, -1),
         #node = as.character(sheet_name)
         ) %>%
  mutate_at('VWC_Avg_m^3/m^3', as.numeric)

data_summaries <- data4 %>%
  group_by(TIMESTAMP_,depth) %>%
  summarize(avg_ = mean(`VWC_Avg_m^3/m^3`))

data4_1 %>%
  ggplot(aes(x = TIMESTAMP_, y = `VWC_Avg_m^3/m^3`, color = depth)) +
  geom_point() +
  theme_classic() +
  labs(title = "VWC Data") +
  facet_wrap(vars(sensor_number_and_depth))

#?filter_all


# Temp data ---------------------------------------------------------------

#same uplaod structure as before
temp <- read_excel("data_clean_app/O2 temp and VWC data_all nodes_June21-Feb22.xlsx", 
                  sheet = "Node 1_", col_types = c("date", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text"), skip = 1)
temp_prepped <- vwc_temp_prep_function(temp)

data2 <- temp_prepped %>%
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
         #node = as.character(sheet_name)
  ) %>%
  mutate_at('SoilT_Avg_DegCelsius', as.numeric)

data_summaries <- data4 %>%
  group_by(TIMESTAMP_,depth) %>%
  summarize(avg_ = mean(`SoilT_Avg_DegCelsius`))

data4 %>%
  ggplot(aes(x = TIMESTAMP_, y = `SoilT_Avg_DegCelsius`, color = depth)) +
  geom_point() +
  theme_classic() +
  labs(title = "Soil Temp Data") +
  facet_wrap(vars(sensor_number_and_depth))



# Deep soil Moisture ------------------------------------------------------

deep_soil_moisture_all <- read_excel("data_clean_app/O2 temp and VWC data_all nodes_June21-Feb22.xlsx", 
                                                          sheet = "Deep soil moisture", col_types = c("text", 
                                                                                                      "date", "numeric", "text", "numeric", 
                                                                                                      "numeric", "numeric", "numeric", 
                                                                                                      "numeric", "numeric", "numeric", 
                                                                                                      "numeric", "text", "numeric", "text", 
                                                                                                      "numeric", "text"), skip = 4)
data1 <- deep_soil_moisture_all %>%
  mutate(node = case_when(Plot=="A" ~ "Node 1",
                          Plot=="B" ~ "Node 2",
                          Plot=="C" ~ "Node 3",
                          Plot=="D" ~ "Node 4")) %>%
  filter(!is.na(node))

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

data_summaries <- data3 %>%
  group_by(date(`Date & Time_m/d/yr format`), node, depth) %>%
  summarise(avg_ = mean(`% Vol`)) %>%
  rename(Date1 = 1)

#all data
data3 %>%
  ggplot(aes(x = `Date & Time_m/d/yr format`, y = `% Vol`,color = depth)) +
  geom_point() +
  theme_classic()

# sumarized
data_summaries %>%
  #rename(Date1 = `date(`Date & Time_m/d/yr format`)`) %>%
  ggplot(aes(x = Date1, y = `avg_`, color = depth,text = node, shape = node)) +
  geom_point() +
  theme_classic() +
  labs(title = "Avg Deep Soil Moisture bby Node, Depth, and Day")

#library(plotly)

ggplotly(x)

# x <- data3 %>%
#   filter(date(`Date & Time_m/d/yr format`) == "2021-07-26",
#          depth == "600-1000",
#          node == "Node 2")
# 
# x1 <- x %>%
#   na.omit()
