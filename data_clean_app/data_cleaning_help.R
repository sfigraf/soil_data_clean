library(tidyverse)
library(lubridate)
library(plotly)

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

