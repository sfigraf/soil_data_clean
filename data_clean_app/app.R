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
                     hr())
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
        clean_sheet_function(raw_data(),input$file1sheet)
    })

    ##table 2 output
    output$table2 <- renderDT({
        datatable(cleaned_data()$clean_data)
    })
    
    ##table 3 output
    output$table3 <- renderDT({
        datatable(cleaned_data()$depth_timestamp_summaries)
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
