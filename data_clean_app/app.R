#  to do:

#start/ends/values updating in ui with file upload: attempted not succeeded
# combine tabs in "oxygen" tab so plot and data are right next to each other in the tabs
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

source("functions/oxygen_function.R")
source("functions/greenhouse_gas_function.R") #Node 1_cleaned_021722
source("functions/vwc_temp_data_prep_function.R")
source("functions/vwc_function.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(title = "Soil data wrangle!!",
               theme = shinytheme("yeti"), #end of navbar page arguments; what follow is all inside it
               

# Upload and clean oxygen data --------------------------------------------

               
        tabPanel("Oxygen Data",
                 
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

# Greenhouse Gas cleaning Tab ---------------------------------------------

tabPanel("Greenhouse Gas Data",
         
         sidebarLayout(
             sidebarPanel(
                 fileInput('file2', 'Insert File', accept = c(".xlsx")),
                 textInput('file2sheet','Name of Sheet (Case-Sensitive)'),
                 textInput('nodename1','Name of Node'),
                 
                 
             ),#end of sidebar panel
             mainPanel(tabsetPanel(
                 tabPanel("Raw Data",
                          withSpinner(DT::dataTableOutput("ggtable1"))),
                 tabPanel("Cleaned Data", 
                          withSpinner(DT::dataTableOutput("ggtable2")),
                          hr(),
                          downloadButton(outputId = "ggdownload1", label = "Save this data as CSV"),
                          hr()),
                 
                 tabPanel("Filtered/Clean Data plots", 
                          withSpinner(plotOutput("gg_plot1")),
                          withSpinner(plotOutput("gg_plot2")),
                          withSpinner(plotOutput("gg_plot3")),
                          withSpinner(plotOutput("gg_plot4")),
                 ),
                 
             )#end of tabset panel
             )#end of mainPanel 
         ) #end of sidebarLayout
),# end of clean greenhouse gas Tab panel



# VWC and soil temp data ----------------------------------------------------------------
tabPanel("VWC and Temp Data",
         
         sidebarLayout(
             sidebarPanel(
                 fileInput('file3', 'Insert File', accept = c(".xlsx")),
                 textInput('file3sheet','Name of Sheet (Case-Sensitive)'),
                 textInput('nodename2','Name of Node'),
                 
                 
             ),#end of sidebar panel
             mainPanel(tabsetPanel(
                 tabPanel("Raw Data",
                          withSpinner(DT::dataTableOutput("vwc_temp_table1"))),
                 tabPanel("VWC Cleaned Data", 
                          withSpinner(DT::dataTableOutput("vwctable2")),
                          hr(),
                          downloadButton(outputId = "vwcdownload1", label = "Save this data as CSV"),
                          hr()),
                 tabPanel("VWC Summarized Data", 
                          withSpinner(DT::dataTableOutput("vwctable3")),
                          hr(),
                          downloadButton(outputId = "vwcdownload2", label = "Save this data as CSV"),
                          hr()),
                 
                 tabPanel("Filtered/Clean Data plots", 
                          withSpinner(plotOutput("vwcplot1"))
                 ),
                 
             )#end of tabset panel
             )#end of mainPanel 
         ) #end of sidebarLayout
),# end of clean greenhouse gas Tab panel

# Combine Data UI Tab -----------------------------------------------------


        
        tabPanel("Combine Data",
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("combine_file1", "Upload files to combine", multiple = TRUE),
                         
                     ),
                     mainPanel(tabsetPanel(
                         tabPanel("Combined Data",
                             withSpinner(DT::dataTableOutput("combine_table1")),
                             hr(),
                             downloadButton(outputId = "combine_download1", label = "Save this data as CSV"),
                             hr(),
                             ),

                     )# end of combine data tabset panel
                     ) #end of combine data mainpanel
                     )# end of combinedata sidebar Layout
                 )# end of combine data tabPanel
                 
        
        
    ) #end of navbar page
    
    
    #tableOutput("value")
)

# Server ------------------------------------------------------------------


server <- function(input, output, session) {

# Uploading Oxygen Data Logic ----------------------------------------------------

    
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
    

# Upload Greenhouse gas Data Logic ----------------------------------------
 # had error: path needs to be a string because i had input$file1 somwhere in here instead of input$file2
    gg_sheets_name <- reactive({
        if (!is.null(input$file2)) {
            return(excel_sheets(path = input$file2$datapath))  
        } else {
            return(NULL)
        }
    })
    
    gg_raw_data <- reactive({
        if (!is.null(input$file2) && 
            (input$file2sheet %in% gg_sheets_name())) {
            data <- read_excel(input$file2$datapath, 
                               sheet = input$file2sheet,
                               col_types = c("date", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text"), 
                               
                               skip = 1)
            
            return(data)
        } else {
            return(NULL)
        }
    })
    
    gg_function_cleaned_data <- reactive({
        gg_function(gg_raw_data(),input$nodename1)
        
    })
    

# Upload VWC _temp data logic ---------------------------------------------

    vwc_temp_sheets_name <- reactive({
        if (!is.null(input$file3)) {
            return(excel_sheets(path = input$file3$datapath))  
        } else {
            return(NULL)
        }
    })
    
    vwc_temp_raw_data <- reactive({
        if (!is.null(input$file3) && 
            (input$file3sheet %in% vwc_temp_sheets_name())) {
            data <- read_excel(input$file3$datapath, 
                               sheet = input$file3sheet,
                               col_types = c("date", 
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
                               
                               
            
            return(data)
        } else {
            return(NULL)
        }
    })
    
    #Warning: Error in : evaluation nested too deeply: infinite recursion / options(expressions=)?
    #solved because I had named 2 different reactives the same thing (vwc_temp_raw_data). Changed the one below to vwc_temp_prepped_data
    vwc_temp_prepped_data <- reactive({
        vwc_temp_prep_function(vwc_temp_raw_data())
        
    }) 
    
    vwc_data_clean <- reactive({
        
        vwc_clean <- vwc_function(vwc_temp_prepped_data(), input$nodename2)
        
        data_summaries <- vwc_clean %>%
            group_by(TIMESTAMP_,depth) %>%
            summarize(avg_ = mean(`VWC_Avg_m^3/m^3`))
        
        vwc_cleaned_data_list <- list("vwc_clean" = vwc_clean, "vwc_data_summaries1" = data_summaries)
        return(vwc_cleaned_data_list)
    })
    
    

# Combine Data Upload Logic -----------------------------------------------

    combined_data <- reactive({
        node_list <- lapply(input$combine_file1$datapath, read_csv)
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
    

# Oxygen Table Outputs -----------------------------------------------------------


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

# Greenhouse gas Plots and tables ----------------------------------------------------
    output$ggtable1 <- renderDT({
        datatable(gg_raw_data())
    })
    
    output$ggtable2 <- renderDT({
        datatable(gg_function_cleaned_data())
    })
    
    ##gg_plot1 output
    output$gg_plot1 <- renderPlot({
        gg_function_cleaned_data() %>%
            ggplot(aes(x = DateTime2, y = `N2O_Flux[nmol+1m-2s-1]`)) +
            geom_point() +
            labs(title = "N2O Flux") +
            theme_classic()
    })
    ##gg_plot2 output
    output$gg_plot2 <- renderPlot({
        gg_function_cleaned_data() %>%
            ggplot(aes(x = DateTime2, y = `N2O Concentration[nmol+1mol-1]`)) +
            geom_point() +
            labs(title = "N2O Concentration") +
            theme_classic()
    })
    ##gg_plot3 output
    output$gg_plot3 <- renderPlot({
        gg_function_cleaned_data() %>%
            ggplot(aes(x = DateTime2, y = `CO2 Flux[nmol+1m-2s-1]`)) +
            geom_point() +
            labs(title = "CO2 Flux") +
            theme_classic()
    })
    ##gg_plot4 output
    output$gg_plot4 <- renderPlot({
        gg_function_cleaned_data() %>%
            ggplot(aes(x = DateTime2, y = `CO2 Concentration[umol+1mol-1]`)) +
            geom_point() +
            labs(title = "CO2 Concentration") +
            theme_classic()
    })

    
    

    

# VWC_ and Temp Output Tables and Plots -----------------------------------

    output$vwc_temp_table1 <- renderDT({
        datatable(vwc_temp_raw_data())
    })

    output$vwctable2 <- renderDT({
        datatable(vwc_data_clean()$vwc_clean)
    })

    output$vwctable3 <- renderDT({
        datatable(vwc_data_clean()$vwc_data_summaries1)
    })
    
    output$vwcplot1 <- renderPlot({
        vwc_data_clean()$vwc_clean %>%
            ggplot(aes(x = TIMESTAMP_, y = `VWC_Avg_m^3/m^3`, color = depth)) +
            geom_point() +
            theme_classic() +
            labs(title = "VWC Data") +
            facet_wrap(vars(sensor_number_and_depth))
    })

# combine data table ------------------------------------------------------

    output$combine_table1 <- renderDT({
        datatable(combined_data())
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
    
    output$ggdownload1 <- downloadHandler(
        filename = 
            function() {
                paste0("greenhouse_gas_cleaned_", str_replace_all(input$nodename1, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(gg_function_cleaned_data(), file)
            
            
        }
    ) #end of ggdownload1 
    
    output$vwcdownload1 <- downloadHandler(
        filename = 
            function() {
                paste0("vwc_cleaned_", str_replace_all(input$nodename2, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(vwc_data_clean()$vwc_clean, file)
            
            
        }
    )
    
    output$vwcdownload2 <- downloadHandler(
        filename = 
            function() {
                paste0("vwc_summarized_", str_replace_all(input$nodename2, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(vwc_data_clean()$vwc_data_summaries1, file)
            
            
        }
    )
    
    output$combine_download1 <- downloadHandler(
        filename = 
            function() {
                paste0("Combined_data.csv")
            }
        ,
        content = function(file) {
            write_csv(combined_data(), file)
            
            
        }
    ) #end   
}

# Run the application 
shinyApp(ui = ui, server = server)
