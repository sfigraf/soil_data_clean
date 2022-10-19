#  to do:

#start/ends/values updating in ui with file upload: attempted not succeeded
# combine tabs in "oxygen" tab so plot and data are right next to each other in the tabs
# add filters to all stuff 
# double check NA's getting removed correctly
# add text input to show that sliders won't re-update oxygen data; only changing node name will
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinythemes)
library(DT)
library(plotly)
#changes max upload to 600 mb
options(shiny.maxRequestSize=600*1024^2)

source("functions/oxygen_function.R")
source("functions/greenhouse_gas_function.R") #Node 1_cleaned_021722
source("functions/vwc_temp_data_prep_function.R")
source("functions/vwc_function.R")
source("functions/soil_temp_function.R")
source("functions/deep_moisture_function.R")
source("functions/utils.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(title = "Soil data wrangle!!",
               theme = shinytheme("united"), #end of navbar page arguments; what follow is all inside it
               

# Oxygen Upload and clean data --------------------------------------------

               
        tabPanel("Oxygen Data",
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Insert File', accept = c(".xlsx")),
                         pickerInput01("file1_picker"),
                         # pickerInput("file1_picker", "Sheet Name", choices = NULL,
                         #             selected = NULL,
                         #             multiple = FALSE),
                         #textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
                         sliderInput("slider1", "Oxygen Range to Include",
                                     min = 0,
                                     max = 50,  
                                     value = c(10,30),
                                     step = 1),
                         checkboxInput("checkbox1", "Exclude Oxygen Range based on Dates?"),
                         hr(),
                         conditionalPanel(condition = "input.checkbox1 == true",
                                          dateRangeInput("drange1",
                                                         "Date you want to take off",
                                                         start = "2021-06-01",
                                                         end = "2021-07-10"),
                                          sliderInput("slider2", "Oxygen range to exclude between selected date range above",
                                                      min = 0,
                                                      max = 50,  
                                                      value = c(0,30),
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
                 pickerInput01("file2_picker"),
                 textInput('nodename1','Name of Node'),
                 sliderInput("gg_n2o_flux_slider","N2O Flux Range",
                             min = 0,
                             max = 50,  
                             value = c(0,30),
                             step = 1)
                 
             ),#end of sidebar panel
             mainPanel(tabsetPanel(
                 tabPanel("Raw Data",
                          withSpinner(DT::dataTableOutput("ggtable1"))),
                 tabPanel("Cleaned Data", 
                          withSpinner(DT::dataTableOutput("gg_n2o_flux_table")),
                          hr(),
                          downloadButton(outputId = "ggdownload1", label = "Save this data as CSV"),
                          hr(),
                          
                          withSpinner(DT::dataTableOutput("gg_n2o_conc_table")),
                          hr(),
                          downloadButton(outputId = "ggdownload2", label = "Save this data as CSV"),
                          hr(),
                          
                          withSpinner(DT::dataTableOutput("gg_co2_flux_table")),
                          hr(),
                          downloadButton(outputId = "ggdownload3", label = "Save this data as CSV"),
                          hr(),
                          
                          withSpinner(DT::dataTableOutput("gg_co2_conc_table")),
                          hr(),
                          downloadButton(outputId = "ggdownload4", label = "Save this data as CSV"),
                          hr(),
                          ), #end of clean_data_tab
                 
                 
                 
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
                 pickerInput01("file3_picker"), 
                 #textInput('file3sheet','Name of Sheet (Case-Sensitive)'),
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
                 
                 tabPanel("VWC Filtered/Clean Data plots", 
                          withSpinner(plotOutput("vwcplot1"))),
                 
                 ####soil data UI elements
                 tabPanel("Soil Temp Cleaned Data", 
                          withSpinner(DT::dataTableOutput("soiltemp_table2")),
                          hr(),
                          downloadButton(outputId = "soiltemp_download1", label = "Save this data as CSV"),
                          hr()),
                 tabPanel("Soil Temp Summarized Data", 
                          withSpinner(DT::dataTableOutput("soiltemp_table3")),
                          hr(),
                          downloadButton(outputId = "soiltemp_download2", label = "Save this data as CSV"),
                          hr()),
                 
                 tabPanel("Filtered/Clean Soil Temp Data plots", 
                          withSpinner(plotOutput("soiltemp_plot1")))
                 
             )#end of tabset panel
             )#end of mainPanel 
         ) #end of sidebarLayout
),# end of clean vwc/temp Tab panel


# Deep Moisture UI  -------------------------------------------------------
tabPanel("Deep Moisture",
         
         sidebarLayout(
             sidebarPanel(
                 fileInput('dm_file1', 'Insert File', accept = c(".xlsx")),
                 pickerInput01("dm_file1_picker"),
                 #textInput('dm_file1sheet','Name of Sheet (Case-Sensitive)'),
                 #textInput('nodename2','Name of Node'),
                 
                 
             ),#end of sidebar panel
             mainPanel(tabsetPanel(
                 
                 tabPanel("Raw Data",
                          withSpinner(DT::dataTableOutput("dm_table1"))),
                 tabPanel("Deep Moisture Cleaned Data", 
                          withSpinner(DT::dataTableOutput("dm_table2")),
                          hr(),
                          downloadButton(outputId = "dm_download1", label = "Save this data as CSV"),
                          hr()),
                 tabPanel("Deep Moisture Summarized Data", 
                          withSpinner(DT::dataTableOutput("dm_table3")),
                          hr(),
                          downloadButton(outputId = "dm_download2", label = "Save this data as CSV"),
                          hr()),
                 
                 tabPanel("Deep Moisture Filtered/Clean Data plots", 
                          withSpinner(plotlyOutput("dm_plot1")),
                          withSpinner(plotlyOutput("dm_plot2"))
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
) # end of UI tab

# Server ------------------------------------------------------------------


server <- function(input, output, session) {

# Updating Sheet Names ----------------------------------------------------

  #oxygen
  observeEvent(input$file1,{
    updatePickerInput(session, "file1_picker",
                      selected = NULL, 
                      choices = sheets_name()
    )
    
  })
  #greenhouse gas
  observeEvent(input$file2,{
    updatePickerInput(session, "file2_picker",
                      selected = NULL, 
                      choices = gg_sheets_name()
    )
    
  })
  ## VWC and Soil
  observeEvent(input$file3,{
    updatePickerInput(session, "file3_picker",
                      selected = NULL, 
                      choices = vwc_temp_sheets_name()
    )
    
  })
  
  ## Deep Moisture
  observeEvent(input$dm_file1,{
    updatePickerInput(session, "dm_file1_picker",
                      selected = NULL, 
                      choices = dm_sheets_name()
    )
    
  })
  

# Uploading Oxygen Data Logic ----------------------------------------------------

    
    sheets_name <- reactive({
        if (!is.null(input$file1)) {
            return(excel_sheets(path = input$file1$datapath))  
        } else {
            return(NULL)
        }
    })
    
    raw_data <- reactive({
        if (!is.null(input$file1_picker) 
            #&& (input$file1_picker %in% sheets_name())
            ) {
          #makes it so the app doesn't crash
          tryCatch({
            data <- read_excel(input$file1$datapath, 
                               sheet = input$file1_picker,
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
            
          },
          # this part stops the code from exercuting and stops the app from crashing if a warning happens. Since there are often lots of warnings reading in this data, we don't care
          # warning = function(w) {
          #   showNotification('there was a warning','',type = "error")
          #   return()
          # }, 
          #if there is an error reading in the sheet (function(e)), this stops the app from creashing 
          error = function(e) {
            showNotification('Can not read in that Sheet', '',type = "error")
            return()
          }, silent=TRUE
          ) #end of trycatch
          
        } else {
            return(NULL)
        }
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
        if (!is.null(input$file2_picker) 
            # && 
            # (input$file2_picker %in% gg_sheets_name())
            ) {
          tryCatch({data <- read_excel(input$file2$datapath, 
                                       sheet = input$file2_picker,
                                       col_types = c("date", 
                                                     "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "text"), 
                                       
                                       skip = 1)
          
          return(data)},
          error = function(e) {
            showNotification('Can not read in that Sheet', '',type = "error")
            return()
          }, silent=TRUE
          )#end of trycatch
            
        }
        
        else {
            return(NULL)
        }
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
        if (!is.null(input$file3) ) {
            data <- read_excel(input$file3$datapath, 
                               sheet = input$file3_picker,
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
        
        validate(
            need(!is.null(vwc_temp_raw_data() ), "Please upload a data set")
        )
        vwc_temp_prep_function(vwc_temp_raw_data())
        
    }) 
    
    #when I do filters, I'd want to move these to datareactives section
    vwc_data_clean <- reactive({
        
        vwc_clean <- vwc_function(vwc_temp_prepped_data(), input$nodename2)
        
        data_summaries <- vwc_clean %>%
            group_by(TIMESTAMP_,depth) %>%
            summarize(avg_ = mean(`VWC_Avg_m^3/m^3`))
        
        vwc_cleaned_data_list <- list("vwc_clean" = vwc_clean, "vwc_data_summaries1" = data_summaries)
        return(vwc_cleaned_data_list)
    })
    
    soil_temp_data_clean <- reactive({
        
        soiltemp_clean <- soil_temp_function(vwc_temp_prepped_data(), input$nodename2)
        
        data_summaries <- soiltemp_clean %>%
            group_by(TIMESTAMP_,depth) %>%
            summarize(avg_ = mean(`SoilT_Avg_DegCelsius`))
        
        soiltemp_cleaned_data_list <- list("soiltemp_clean" = soiltemp_clean, "soiltemp_data_summaries1" = data_summaries)
        return(soiltemp_cleaned_data_list)
    })
    
    


# Upload Deep Moisture Logic ----------------------------------------------

    dm_sheets_name <- reactive({
        if (!is.null(input$dm_file1)) {
            return(excel_sheets(path = input$dm_file1$datapath))  
        } else {
            return(NULL)
        }
    })
    
    dm_raw_data <- reactive({
        if (!is.null(input$dm_file1)) {
            data <- read_excel(input$dm_file1$datapath, 
                               sheet = input$dm_file1_picker,
                               col_types = c("text", 
                                             "date", "numeric", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "text", "numeric", "text", 
                                             "numeric", "text"), skip = 4)
            return(data)
        } else {
            return(NULL)
        }
    }) 
    
    dm_function_cleaned_data <- reactive({
        deep_moisture_clean <- deep_moisture_function(dm_raw_data())
        
        data_summaries <- deep_moisture_clean %>%
            group_by(date(`Date & Time_m/d/yr format`), node, depth) %>%
            summarise(avg_ = mean(`% Vol`)) %>%
            rename(Date1 = 1)
        
        dm_cleaned_data_list <- list("dm_clean" = deep_moisture_clean, "dm_data_summaries1" = data_summaries)
        return(dm_cleaned_data_list)
        
        
    })
# Combine Data Upload Logic -----------------------------------------------

    combined_data <- reactive({
        node_list <- lapply(input$combine_file1$datapath, read_csv)
        all <- bind_rows(node_list)
        #erase duplicates just in case; shouldn't be any since duplicates are also erased in data_cleaning process
        all_1 <- all %>%
            distinct()
        
        # new_df <- left_join(input$uploaded1, input$upload2, by ="TIMESTAMP")
        # list1 <- list(all_1, new_df)
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
    
    
    

# Oxygen Data reactives ----------------------------------------------------------
    #function that initally cleans the data
    function_cleaned_data <- reactive({
        
        #crucial: was crashing before if there wasn't valid data uploaded
        #in order for raw_data() to not be NULL, a valid datafile and sheet name needs to be entered/uploaded
        
        validate(
            need(!is.null(raw_data() ), "Please upload a data set")
        )
        
        clean_sheet_function(raw_data(),input$file1_picker)

    })
    #creates reactive vals to be modified
    # 
    oxygen_mod_df <- reactiveValues(cleaned = NULL, summaries = NULL)
    
    #assigns each reactive values a dataframe
    observe({
        oxygen_mod_df$cleaned <- function_cleaned_data()
        
        oxygen_mod_df$summaries <- function_cleaned_data() %>%
            group_by(TIMESTAMP, depth) %>%
            #mutate(avg2 = mean(value1))
            summarise(avg1 = mean(value1)) %>%
            mutate(node_x = as.character(input$file1_picker))
    })
    
    #creates proxies to use for datatables
    oxygen_cleaned_proxy <- DT::dataTableProxy('table2')
    oxygen_summaries_proxy <- DT::dataTableProxy('table3')
    
 
    observeEvent(input$button1,{
        #print(max(function_cleaned_data()$value1))
        #if checkbox is pressed to modify values with date range, then use slider2 and dateragne inputs in addition to slider1
        if (input$checkbox1 == TRUE) {
            
            
            filtered_clean <- oxygen_mod_df$cleaned %>%
                filter(
                    value1 >= input$slider1[1] & value1 <= input$slider1[2]
                )
            
            problem_rows <- filtered_clean %>%
                #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
                filter((TIMESTAMP >= input$drange1[1] & TIMESTAMP <= input$drange1[2]),
                       value1 >= input$slider2[1] & value1 <= input$slider2[2]
                )
            #this takes these rows out of the main dataframe
            oxygen_mod_df$cleaned <- anti_join(filtered_clean, problem_rows)
            
            oxygen_mod_df$summaries <- oxygen_mod_df$cleaned %>%
                
                group_by(TIMESTAMP, depth) %>%
                summarise(avg1 = mean(value1)) %>%
                mutate(node_x = as.character(input$file1_picker))
            
        } else { #if checkbox not pressed, just use slider1 inputs
            oxygen_mod_df$cleaned <- function_cleaned_data() %>%
                filter(
                    value1 >= input$slider1[1] & value1 <= input$slider1[2]
                )
            
            #each entry sometimes has multiple data points for each timestamp, so this code gives averages for each depth by timestamp
            oxygen_mod_df$summaries <- oxygen_mod_df$cleaned %>%
                group_by(TIMESTAMP, depth) %>%
                summarise(avg1 = mean(value1)) %>%
                mutate(node_x = as.character(input$file1_picker))
        }
        
        DT::replaceData(oxygen_cleaned_proxy, oxygen_mod_df$cleaned)
        DT::replaceData(oxygen_summaries_proxy, oxygen_mod_df$summaries)
        # cleaned_data_list <- list("cleaned_data2" = filtered_clean2, "data_summaries1" = data_summaries)
        # return(cleaned_data_list)
    })
    


# Greenhouse Gas Data reactives -------------------------------------------
    gg_function_cleaned_data <- reactive({
        
        validate(
            need(!is.null(gg_raw_data()), "Please upload a data set")
        )
        gg_function(gg_raw_data(),input$nodename1)
        
    })
    
    n2o_flux_cleaned <- reactive({
        
        gg_function_cleaned_data()$n2o_flux %>%
            filter(
                `N2O_Flux[nmol+1m-2s-1]` >= input$gg_n2o_flux_slider[1] & `N2O_Flux[nmol+1m-2s-1]` <= input$gg_n2o_flux_slider[2]
            )
        
    })
    
    
# Oxygen Table Outputs -----------------------------------------------------------


    output$table1 <- renderDT({
        datatable(raw_data(), editable = TRUE)
    })
    
    #oxygen_proxy <- DT::dataTableProxy('table2')
    ##table 2 output
    output$table2 <- renderDT({
        datatable(oxygen_mod_df$cleaned)
    })
    
    ##table 3 output
    output$table3 <- renderDT({
        datatable(oxygen_mod_df$summaries)
    })
    
    
    #plot 1 output
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
        ggplot(oxygen_mod_df$cleaned) +
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
    
    output$gg_n2o_flux_table <- renderDT({
        datatable(n2o_flux_cleaned() )
    })
    
    output$gg_n2o_conc_table <- renderDT({
        datatable(gg_function_cleaned_data()$n2o_conc)
    })
    
    output$gg_co2_flux_table <- renderDT({
        datatable(gg_function_cleaned_data()$co2_flux)
    })
    
    output$gg_co2_conc_table <- renderDT({
        datatable(gg_function_cleaned_data()$co2_conc)
    })
    
    ##gg_plot1 output
    output$gg_plot1 <- renderPlot({
        n2o_flux_cleaned() %>%
            ggplot(aes(x = DateTime2, y = `N2O_Flux[nmol+1m-2s-1]`)) +
            geom_point() +
            labs(title = "N2O Flux") +
            theme_classic()
    })
    ##gg_plot2 output
    output$gg_plot2 <- renderPlot({
        gg_function_cleaned_data()$n2o_conc %>%
            ggplot(aes(x = DateTime2, y = `N2O Concentration[nmol+1mol-1]`)) +
            geom_point() +
            labs(title = "N2O Concentration") +
            theme_classic()
    })
    ##gg_plot3 output
    output$gg_plot3 <- renderPlot({
        gg_function_cleaned_data()$co2_flux %>%
            ggplot(aes(x = DateTime2, y = `CO2 Flux[nmol+1m-2s-1]`)) +
            geom_point() +
            labs(title = "CO2 Flux") +
            theme_classic()
    })
    ##gg_plot4 output
    output$gg_plot4 <- renderPlot({
        gg_function_cleaned_data()$co2_conc %>%
            ggplot(aes(x = DateTime2, y = `CO2 Concentration[umol+1mol-1]`)) +
            geom_point() +
            labs(title = "CO2 Concentration") +
            theme_classic()
    })

    
    

    

# VWC_ Output Tables and Plots -----------------------------------

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

# Soil Temp Outputs ------------------------------------------------------------
    output$soiltemp_table2 <- renderDT({
        datatable(soil_temp_data_clean()$soiltemp_clean)
    })
    
    output$soiltemp_table3 <- renderDT({
        datatable(soil_temp_data_clean()$soiltemp_data_summaries1)
    })
    
    output$soiltemp_plot1 <- renderPlot({
        soil_temp_data_clean()$soiltemp_clean %>%
            ggplot(aes(x = TIMESTAMP_, y = `SoilT_Avg_DegCelsius`, color = depth)) +
            geom_point() +
            theme_classic() +
            labs(title = "Soil Temp Data") +
            facet_wrap(vars(sensor_number_and_depth))
    })

# Deep Moisture Outputs ---------------------------------------------------

    output$dm_table1 <- renderDT({
        datatable(dm_raw_data())
    })
    
    output$dm_table2 <- renderDT({
        datatable(dm_function_cleaned_data()$dm_clean)
    })
    
    output$dm_table3 <- renderDT({
        datatable(dm_function_cleaned_data()$dm_data_summaries1)
    })
    
    output$dm_plot1 <- renderPlotly({
        plot<- dm_function_cleaned_data()$dm_clean %>%
            ggplot(aes(x = `Date & Time_m/d/yr format`, y = `% Vol`,color = depth, text = node,shape = node)) +
            geom_point() +
            theme_classic() +
            labs(title = "All Deep Moisture Data")
        
        ggplotly(plot)
    })
    
    output$dm_plot2 <- renderPlotly({
        plot<- dm_function_cleaned_data()$dm_data_summaries1 %>%
            ggplot(aes(x = Date1, y = `avg_`, color = depth,text = node,shape = node)) +
            geom_point() +
            theme_classic() +
            labs(title = "Avg Deep Soil Moisture by Node, Depth, and Day")
        
        ggplotly(plot)
    })

# combine data table ------------------------------------------------------

    output$combine_table1 <- renderDT({
        datatable(combined_data())
    })    
# Download Handlers -------------------------------------------------------

    output$download1 <- downloadHandler(
        filename = 
            function() {
                paste0("Oxygen_Cleaned_data_", str_replace_all(input$file1_picker, fixed(" "), ""), ".csv")
            }
        ,
        content = function(file) {
            write_csv(oxygen_mod_df$cleaned, file)
            
            
        }
    ) #end of download1
    
    output$download2 <- downloadHandler(
        filename = 
            function() {
                paste0("Oxygen_Data_summaries_", str_replace_all(input$file1_picker, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(oxygen_mod_df$summaries, file)
            
            
        }
    ) #end of download2    

# Greenshouse Gas Download Handlers ----------------------------------------

    
    output$ggdownload1 <- downloadHandler(
        filename = 
            function() {
                paste0("greenhouse_gas_n2o_flux_cleaned_", str_replace_all(input$nodename1, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(n2o_flux_cleaned() , file)
            
            
        }
    ) #end of ggdownload1 
    
    output$ggdownload2 <- downloadHandler(
        filename = 
            function() {
                paste0("greenhouse_gas_n2o_conc_cleaned_", str_replace_all(input$nodename1, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(gg_function_cleaned_data()$n2o_conc , file)
            
            
        }
    ) #end of ggdownload1 
    
    output$ggdownload3 <- downloadHandler(
        filename = 
            function() {
                paste0("greenhouse_gas_co2_flux_cleaned_", str_replace_all(input$nodename1, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(gg_function_cleaned_data()$co2_flux , file)
            
            
        }
    ) #end of ggdownload1 
    
    output$ggdownload4 <- downloadHandler(
        filename = 
            function() {
                paste0("greenhouse_gas_co2_conc_cleaned_", str_replace_all(input$nodename1, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(gg_function_cleaned_data()$co2_conc, file)
            
            
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
    output$soiltemp_download1 <- downloadHandler(
        filename = 
            function() {
                paste0("soil_temp_cleaned_", str_replace_all(input$nodename2, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(soil_temp_data_clean()$soiltemp_clean, file)
            
            
        }
    )
    
    output$soiltemp_download2 <- downloadHandler(
        filename = 
            function() {
                paste0("soil_temp_summarized_", str_replace_all(input$nodename2, fixed(" "), ""),".csv")
            }
        ,
        content = function(file) {
            write_csv(soil_temp_data_clean()$soiltemp_data_summaries1, file)
            
            
        }
    ) # end of soil temp downloads
    

# Deep moisture DH's ------------------------------------------------------
    output$dm_download1 <- downloadHandler(
        filename = 
            function() {
                paste0("deep_moisture_cleaned.csv")
            }
        ,
        content = function(file) {
            write_csv(dm_function_cleaned_data()$dm_clean, file)
            
            
        }
    )
    
    output$dm_download2 <- downloadHandler(
        filename = 
            function() {
                paste0("deep_moisture_summarized.csv")
            }
        ,
        content = function(file) {
            write_csv(dm_function_cleaned_data()$dm_data_summaries1, file)
            
            
        }
    ) # end of soil temp downloads
    
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
