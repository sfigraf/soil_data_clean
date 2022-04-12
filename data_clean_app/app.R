#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#  to do:
# make tab with graph to QAQC
#new overarching tab for all_data_combining
#filters to get more specific with values

#broad cleaning 

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(readxl)
library(DT)
#changes max upload to 600 mb
options(shiny.maxRequestSize=600*1024^2)

source("clean_sheet_function.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Insert File', accept = c(".xlsx")),
            textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
            sliderInput("slider1", "Oxygen Range",
                        min = 0,
                        max = 50,  
                        value = c(10,30),
                        step = 1)
                        
            
        
    ),#end of sidebar panel
        mainPanel(tabsetPanel(
            tabPanel("Uploaded Table",
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
            
            tabPanel("Plots", 
                     withSpinner(plotOutput("plot1")),
                     withSpinner(plotOutput("plot2")),
                     ),
            
        ))#end of mainPanel 
    ) #end of sidebarLayout
    
    
    
    #tableOutput("value")
)

server <- function(input, output) {
    
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
    
    
    output$table1 <- renderDT({
        datatable(raw_data(), editable = TRUE)
    })
    
    # proxy = dataTableProxy('table1')
    # 
    # observeEvent(input$x1_cell_edit, {
    #     info = input$x1_cell_edit
    #     str(info)
    #     i = info$row
    #     j = info$col
    #     v = info$value
    #     x[i, j] <<- DT::coerceValue(v, x[i, j])
    #     replaceData(proxy, x, resetPaging = FALSE)  # important
    # })
    
    cleaned_data <- reactive({
        results <- clean_sheet_function(raw_data(),input$file1sheet)
        
        filtered_clean <- results$clean_data %>%
            filter(
                value1 >= input$slider1[1] & value1 <= input$slider1[2]
            )
        
        #each entry sometimes has multiple data points for each timestamp, so this code gives averages for each depth by timestamp
        data_summaries <- filtered_clean %>%
            group_by(TIMESTAMP, depth) %>%
            #mutate(avg2 = mean(value1))
            summarise(avg1 = mean(value1)) %>%
            mutate(node_x = as.character(input$file1sheet))
        
        cleaned_data_list <- list("cleaned_data2" = filtered_clean, "data_summaries1" = data_summaries,  "cleaned_list" = results)
        return(cleaned_data_list)
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
        ggplot(cleaned_data()$cleaned_list$not_clean_data) +
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
            write_csv(cleaned_data()$clean_data, file)
            
            
        }
    ) #end of download1
    
    output$download2 <- downloadHandler(
        filename = 
            function() {
                paste0("Data_summaries_", str_replace_all(input$file1sheet, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(cleaned_data()$depth_timestamp_summaries, file)
            
            
        }
    ) #end of download2    
}

# Run the application 
shinyApp(ui = ui, server = server)
