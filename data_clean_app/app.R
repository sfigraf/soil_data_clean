#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#  to do:

#new overarching tab for all_data_combining
#tab for all nodes combined
# tab for combining, with multiple file uploads
#start/ends/values updating in ui with file upload: attempted not succeeded
# combine tabs in "oxygen" tab so plot and data are right next to each other in the tabs
 # update access token (make permanent) so that I can keep updating github
# each time you click update data, will sequentially take off selected outlier dates; so each time you press buttion it "adds on"/substracts from df
 
library(shiny)
library(shinycssloaders)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinythemes)
library(DT)
#changes max upload to 600 mb
options(shiny.maxRequestSize=600*1024^2)

source("clean_sheet_function.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(title = "Soil data wrangle!!",
               theme = shinytheme("yeti"), #end of navbar page arguments; what follow is all inside it
        tabPanel("Upload and Clean Oxygen Data",
                 
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Insert File', accept = c(".xlsx")),
                         textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
                         sliderInput("slider1", "Oxygen Range",
                                     min = 0,
                                     max = 50,  
                                     value = c(10,30),
                                     step = 1),
                         checkboxInput("checkbox1", "Take out Oxygen Range based on Dates?"),
                         hr(),
                         conditionalPanel(condition = "input.checkbox1 == true",
                                          dateRangeInput("drange1",
                                                         "Date you want to take off",
                                                         start = "2021-06-01",
                                                         end = "2021-07-10"),
                                          sliderInput("slider2", "Oxygen range to take out of data between selected date range above",
                                                      min = 0,
                                                      max = 50,  
                                                      value = c(10,30),
                                                      step = 1)
                         ),# end of conditional panel
                         
                         actionButton("button1",
                                      "Render/Update Data")
                         
                     ),#end of sidebar panel
                     mainPanel(tabsetPanel(
                         tabPanel("Raw Data",
                                  withSpinner(DT::dataTableOutput("table1"))),
                         tabPanel("Cleaned Data", 
                                  withSpinner(DT::dataTableOutput("table2")),
                                  hr(),
                                  downloadButton(outputId = "download1", label = "Save this data as CSV"),
                                  hr()),
                         tabPanel("Depth and Time Summaries", 
                                  withSpinner(DT::dataTableOutput("table3")),
                                  hr(),
                                  downloadButton(outputId = "download2", label = "Save this data as CSV"),
                                  hr()),
                         
                         tabPanel("Original data plot", 
                                  withSpinner(plotOutput("plot1")),
                                  
                         ),
                         tabPanel("Filtered/Clean Data plot", 
                                  
                                  withSpinner(plotOutput("plot2")),
                         ),
                         
                     )#end of tabset panel
                     )#end of mainPanel 
                 ) #end of sidebarLayout
                 ),# end of clean oxygen Tab panel
        
        tabPanel("Combine Data",
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("file2", "Upload files to combine", multiple = TRUE),
                         
                     ),
                     mainPanel(tabsetPanel(
                         tabPanel("Combined Data",
                             withSpinner(DT::dataTableOutput("table4")),
                             hr(),
                             downloadButton(outputId = "download3", label = "Save this data as CSV"),
                             hr(),
                             ),

                     )# end of combine data tabset panel
                     ) #end of combine data mainpanel
                     )# end of combinedata sidebar Layout
                 )# end of combine data tabPanel
                 
        
        
    ) #end of navbar page
    
    
    #tableOutput("value")
)

server <- function(input, output, session) {

# Uploading Data Logic ----------------------------------------------------

    
    sheets_name <- reactive({
        if (!is.null(input$file1)) {
            return(excel_sheets(path = input$file1$datapath))  
        } else {
            return(NULL)
        }
    })
    
    raw_data <- reactive({
        if (!is.null(input$file1) && 
            (input$file1sheet %in% sheets_name())) {
            data <- read_excel(input$file1$datapath, 
                               sheet = input$file1sheet,
                               col_types = c("date", 
                                             "text", "text", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text"), 
                               
                               skip = 1)
            
            return(data)
        } else {
            return(NULL)
        }
    })
    
    
    function_cleaned_data <- reactive({
        clean_sheet_function(raw_data(),input$file1sheet)
        
    })
    

# Combine Data Upload Logic -----------------------------------------------

    combined_data <- reactive({
        node_list <- lapply(input$file2$datapath, read_csv)
        all <- bind_rows(node_list)
        #erase duplicates just in case; shouldn't be any since duplicates are also erased in data_cleaning process
        all_1 <- all %>%
            distinct()
        return(all_1)
    })
    
    

# Update UI Logic ---------------------------------------------------------

    # events_list <- reactive({
    #     list(input$file1,input$file1sheet)
    # })
    # observeEvent(events_list(), {
    # 
    #      if (!is.null(input$file1) &&
    #         (input$file1sheet %in% sheets_name())) {
    # 
    #         updateSliderInput(session, "slider1",
    #                           min = min(function_cleaned_data()$value1),
    #                           max = max(function_cleaned_data()$value1),
    #                           value = c(min(function_cleaned_data()$value1) + 10,max(function_cleaned_data()$value1)-10)
    #         )
    #     }
    #     
    # })    
    
    
    

# Data reactives ----------------------------------------------------------

    
    
 
    cleaned_data <- eventReactive(input$button1,{
        print(max(function_cleaned_data()$value1))
        #if checkbox is pressed to modify values with date range, then use slider2 and dateragne inputs in addition to slider1

        if (input$checkbox1 == TRUE) {
            
            
            filtered_clean <- function_cleaned_data() %>%
                filter(
                    value1 >= input$slider1[1] & value1 <= input$slider1[2]
                )
            
            problem_rows <- filtered_clean %>%
                #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
                filter((TIMESTAMP >= input$drange1[1] & TIMESTAMP <= input$drange1[2]),
                       value1 >= input$slider2[1] & value1 <= input$slider2[2]
                       )
            #this takes these rows out of the main dataframe
            filtered_clean2 <- anti_join(filtered_clean, problem_rows)
            
            data_summaries <- filtered_clean2 %>%
                # mutate(depth = str_sub(sensor_number_and_depth, 6, -1),
                #        node = as.character(sheet_name)) %>%
                group_by(TIMESTAMP, depth) %>%
                #mutate(avg2 = mean(value1))
                summarise(avg1 = mean(value1)) %>%
                mutate(node_x = as.character(input$file1sheet))
        } else { #if checkbox not pressed, just use slider1 inputs
            filtered_clean2 <- function_cleaned_data() %>%
                filter(
                    value1 >= input$slider1[1] & value1 <= input$slider1[2]
                )
            
            #each entry sometimes has multiple data points for each timestamp, so this code gives averages for each depth by timestamp
            data_summaries <- filtered_clean2 %>%
                group_by(TIMESTAMP, depth) %>%
                #mutate(avg2 = mean(value1))
                summarise(avg1 = mean(value1)) %>%
                mutate(node_x = as.character(input$file1sheet))
        }
        
        
        

        
        cleaned_data_list <- list("cleaned_data2" = filtered_clean2, "data_summaries1" = data_summaries)
        return(cleaned_data_list)
    })
    

# Table Outputs -----------------------------------------------------------


    output$table1 <- renderDT({
        datatable(raw_data(), editable = TRUE)
    })

    ##table 2 output
    output$table2 <- renderDT({
        datatable(cleaned_data()$cleaned_data2)
    })
    
    ##table 3 output
    output$table3 <- renderDT({
        datatable(cleaned_data()$data_summaries1)
    })
    
    ##table 4 output
    output$table4 <- renderDT({
        datatable(combined_data())
    })
    
    ##plot 1 output
    output$plot1 <- renderPlot({
        ggplot(function_cleaned_data()) +
            aes(x = TIMESTAMP, y = value1) +
            geom_point(shape = "circle", size = 1.5, colour = "#112446") +
            theme_minimal() +
            facet_wrap(vars(sensor_number_and_depth)) +
            labs(title = "Not Cleaned data visual")
    })
    #plot 2 output
    output$plot2 <- renderPlot({
        ggplot(cleaned_data()$cleaned_data2) +
            aes(x = TIMESTAMP, y = value1) +
            geom_point(shape = "circle", size = 1.5, colour = "#112446") +
            theme_minimal() +
            facet_wrap(vars(sensor_number_and_depth)) +
            labs(title = "Cleaned data visual")
    })
    

# Download Handlers -------------------------------------------------------

    output$download1 <- downloadHandler(
        filename = 
            function() {
                paste0("Cleaned_data_", str_replace_all(input$file1sheet, fixed(" "), ""), ".csv")
            }
        ,
        content = function(file) {
            write_csv(cleaned_data()$cleaned_data2, file)
            
            
        }
    ) #end of download1
    
    output$download2 <- downloadHandler(
        filename = 
            function() {
                paste0("Data_summaries_", str_replace_all(input$file1sheet, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(cleaned_data()$data_summaries1, file)
            
            
        }
    ) #end of download2    
    
    output$download3 <- downloadHandler(
        filename = 
            function() {
                paste0("Combined_data.csv")
            }
        ,
        content = function(file) {
            write_csv(combined_data(), file)
            
            
        }
    ) #end of download2   
}

# Run the application 
shinyApp(ui = ui, server = server)
