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
