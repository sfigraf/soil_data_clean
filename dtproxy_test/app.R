library(shiny)
library(tidyverse)
library(DT)
library(readxl)
#df <- dplyr::tibble(Height = "185", Weight = "95")
# df <- read_excel("O2 temp and VWC data_all nodes_June21-Feb22.xlsx",
#                  sheet = "Node 1", col_types = c("date",
#                                                  "text", "text", "text", "numeric",
#                                                  "numeric", "numeric", "numeric",
#                                                  "numeric", "numeric", "numeric",
#                                                  "numeric", "numeric", "numeric",
#                                                  "text", "text", "text", "text", "text",
#                                                  "text", "text", "text", "text", "text",
#                                                  "text", "text", "text", "text", "text",
#                                                  "text", "text", "text", "text", "text"),
# 
#                  skip = 1)
source("oxygen_function.R")
# df <- clean_sheet_function(df, "Node 1")

ui <- fluidPage(

    # App title ----
    titlePanel("DT + Proxy + Replace Data"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            
            fileInput('file1', 'Insert File', accept = c(".xlsx")),
            textInput('file1sheet','Name of Sheet (Case-Sensitive)'),
            #shiny::textInput(inputId = "nodename1", label = "Nodename"),


            # shiny::selectInput(inputId = "remove_row", label = "Remove Row",
            #                    choices = 1:nrow(df)),

            # shiny::actionButton(inputId = "remove", label = "Remove"),
            #
            # checkboxInput("checkbox1", "Take out Oxygen Range based on Dates?"),
            # hr(),

             dateRangeInput("drange1",
                            "Date you want to take off",
                            start = "2021-06-01",
                            end = "2021-07-10"),
             sliderInput("slider2", "Oxygen range to take out of data between selected date range above",
                         min = 0,
                         max = 50,
                         value = c(10,30),
                         step = 1),

            actionButton("button1",
                         "Render/Update Data")


        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Histogram ----
            DT::DTOutput(outputId = "table"),
            DT::DTOutput(outputId = "table2"),
            downloadButton(outputId = "soiltemp_download2", label = "Save this data as CSV")


        )
    )
)


# server = function(input, output) {
#     xx <- reactive({clean_sheet_function(df,input$nodename1)})
#
#     mod_df <- reactiveValues(x = xx)
#
#     #x$Date = Sys.time() + seq_len(nrow(x))
#
#
#
#
#     observeEvent(input$button1, {
#         #xx <- clean_sheet_function(df,input$nodename1)
#
#
#
#                 #problem with filter: slider1 becuase I didn't have a slider1 in the UI
#                 # mod_df$x <- mod_df$x %>%
#                 #     filter(
#                 #         value1 >= input$slider1[1] & value1 <= input$slider1[2]
#                 #     )
#
#                 problem_rows <- mod_df$x() %>%
#                     #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
#                     filter((TIMESTAMP >= input$drange1[1] & TIMESTAMP <= input$drange1[2]),
#                            value1 >= input$slider2[1] & value1 <= input$slider2[2]
#                     )
#                 #this takes these rows out of the main dataframe
#                 #
#                 mod_df$x <- anti_join(mod_df$x(), problem_rows)
#
#                 # mod_df$x <- mod_df$x %>%
#                 #     dplyr::bind_rows(
#                 #         dplyr::tibble(Height = input$height,
#                 #                       Weight = input$weight)
#                 #     )
#                 #DT::replaceData(proxy, mod_df$x)
#                 #return(mod_df$x)
#
#             })
#     proxy = dataTableProxy('table')
#
#     output$table = renderDT(
#         isolate(mod_df$x() )
#     )
#
#     shiny::observe({
#
#                 DT::replaceData(proxy, mod_df$x())
#
#             })
#
#     # Downloadable table (df) as csv
#     output$downloadData = downloadHandler(
#         filename = function() {
#             paste("test", ".csv", sep = "")
#         },
#         content = function(file) {
#             write.csv(x1, file, row.names = FALSE)
#         }
#     )
# }
#
#
#
# # Run the application
# shinyApp(ui = ui, server = server)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    

# raw data part -----------------------------------------------------------

    sheets_name <- reactive({
        if (!is.null(input$file1)) {
            return(excel_sheets(path = input$file1$datapath))  
        } else {
            return(NULL)
        }
    })
    
    raw_data <- reactive({
        
        # validate(
        #     need(input$file1 != "", "Please upload a data set")
        # )
        # 
        # validate(
        #     need(input$file1sheet != "", "Please choose a sheet")
        # )
        
        # if you put in the wrong name of the sheet, the app will crash...
        #easiest solution should be to 
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

    #important to continuously remove values and prevent values from being added back to the table

    function_cleaned_data <- reactive({
        
        #crucial: was crashing before if there wasn't valid data uploaded
        #in order for raw_data() to not be NULL, a valid datafile and sheet name needs to be entered/uploaded
        validate(
            need(!is.null(raw_data() ), "Please upload a data set")
        )
        clean_sheet_function(raw_data(),input$file1sheet)
        #can try putting filters inside this?

    })

    # need to set this to null at first; for some reason the reactiveValues() function doesn't work super well with reactives
    # that's why mod_df$x is set/altered below in an observe context
    mod_df <- reactiveValues(x = NULL, y = NULL) 
    
    # this observe has to be outside the filters below because we want each button press to decrease the amount of rows in the table; 
    #otherwise, the function will just reset each time the button is pressed
    observe({
       mod_df$x <- function_cleaned_data()
       mod_df$y <- function_cleaned_data() %>%
           group_by(TIMESTAMP, depth) %>%
           #mutate(avg2 = mean(value1))
           summarise(avg1 = mean(value1)) %>%
           mutate(node_x = as.character(input$file1sheet))
    })
    
    proxy1 <- DT::dataTableProxy('table')
    proxy2 <- DT::dataTableProxy('table2')
    
    

    observeEvent(input$button1, {
        

        #problem with filter: slider1 becuase I didn't have a slider1 in the UI

        problem_rows <- mod_df$x %>%
            #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
            filter((TIMESTAMP >= input$drange1[1] & TIMESTAMP <= input$drange1[2]),
                   value1 >= input$slider2[1] & value1 <= input$slider2[2]
            )
        #this takes these rows out of the main dataframe
        #x is not x() because
        mod_df$x <- anti_join(mod_df$x, problem_rows)
        
        mod_df$y <- mod_df$x %>%
            group_by(TIMESTAMP, depth) %>%
            #mutate(avg2 = mean(value1))
            summarise(avg1 = mean(value1)) %>%
            mutate(node_x = as.character(input$file1sheet))
        
        
        DT::replaceData(proxy1, mod_df$x)
        DT::replaceData(proxy2, mod_df$y)
        

    })


    output$table <- DT::renderDT({
    #error: attempt to apply non-function once occurred because of using mod_df$x()
        mod_df$x

    })
    
    output$table2 <- DT::renderDT({
        #error: attempt to apply non-function once occurred because of using mod_df$x()
        mod_df$y
        
    })


    output$soiltemp_download2 <- downloadHandler(
        filename =
            function() {
                paste0("test.csv")
            }
        ,
        content = function(file) {
            write_csv(mod_df$x, file)


        }
    ) # end of soil temp downloads


}
shinyApp(ui, server)

# this was the code that got me to wrap my head around what worked!
# df1 <- df_products_upload()
# vals <- reactiveValues(
#     df1 = NULL,
#     data = NULL
# )
# 
# # set vals$df1 whenever df1 updates
# observe({
#     vals$df1 <- df1()
# })

# library(shiny)
# library(DT)
# library(tidyverse)
# 
# # Define UI for application that edits the table
# ui = fluidPage(
#     DTOutput('x1'),
#     
#     # App title 
#     titlePanel("Downloading Data"),
#     
#     # Sidebar layout with input and output definitions 
#     sidebarLayout(
#         
#         # Sidebar panel for inputs 
#         sidebarPanel(
#             
#             # Input: Choose dataset 
#             selectInput("dataset", "Choose a dataset:",
#                         choices = c("Demo Table")),
#             
#             # Button
#             downloadButton("downloadData", "Download")
#             
#         ),
#         
#         # Main panel for displaying outputs 
#         mainPanel(
#             
#             tableOutput("table")
#             
#         )
#     ))
# 
# # Define server logic required
# server = function(input, output) {
#     x = iris
#     x$Date = Sys.time() + seq_len(nrow(x))
#     output$x1 = renderDT(x, selection = 'none', editable = TRUE)
#     
#     proxy = dataTableProxy('x1')
#     
#     observeEvent(input$x1_cell_edit, {
#         info = input$x1_cell_edit
#         str(info)
#         i = info$row
#         j = info$col
#         v = info$value
#         x[i, j] <<- DT::coerceValue(v, x[i, j])
#         replaceData(proxy, x, resetPaging = FALSE)  # important
#     })
#     
#     # Downloadable table (df) as csv
#     output$downloadData = downloadHandler(
#         filename = function() {
#             paste(input$dataset, ".csv", sep = "")
#         },
#         content = function(file) {
#             write.csv(x, file, row.names = FALSE)
#         }
#     )
# }
# 
# 
# 
# # Run the application 
# shinyApp(ui = ui, server = server)



# Part that works ---------------------------------------------------------
# 
# library(shiny)
# library(tidyverse)
# library(DT)
# library(readxl)
# #df <- dplyr::tibble(Height = "185", Weight = "95")
# df <- read_excel("O2 temp and VWC data_all nodes_June21-Feb22.xlsx", 
#                  sheet = "Node 1", col_types = c("date", 
#                                                  "text", "text", "text", "numeric", 
#                                                  "numeric", "numeric", "numeric", 
#                                                  "numeric", "numeric", "numeric", 
#                                                  "numeric", "numeric", "numeric", 
#                                                  "text", "text", "text", "text", "text", 
#                                                  "text", "text", "text", "text", "text", 
#                                                  "text", "text", "text", "text", "text", 
#                                                  "text", "text", "text", "text", "text"), 
#                  
#                  skip = 1)
# source("oxygen_function.R")
# # df <- clean_sheet_function(df, "Node 1")
# 
# ui <- fluidPage(
#     
#     # App title ----
#     titlePanel("DT + Proxy + Replace Data"),
#     
#     # Sidebar layout with input and output definitions ----
#     sidebarLayout(
#         
#         # Sidebar panel for inputs ----
#         sidebarPanel(
#             
#             # Input: Slider for the number of bins ----
#             shiny::textInput(inputId = "height", label = "height"),
#             shiny::textInput(inputId = "nodename1", label = "Nodename"),
#             
#             shiny::actionButton(inputId = "add", label = "Add"),
#             
#             
#             
#             dateRangeInput("drange1",
#                            "Date you want to take off",
#                            start = "2021-06-01",
#                            end = "2021-07-10"),
#             sliderInput("slider2", "Oxygen range to take out of data between selected date range above",
#                         min = 0,
#                         max = 50,  
#                         value = c(10,30),
#                         step = 1),
#             
#             actionButton("button1",
#                          "Render/Update Data")
#             
#             
#         ),
#         
#         # Main panel for displaying outputs ----
#         mainPanel(
#             
#             # Output: Histogram ----
#             DT::DTOutput(outputId = "table"),
#             downloadButton(outputId = "soiltemp_download2", label = "Save this data as CSV")
#             
#             
#         )
#     )
# )
# 
# 
# 
# # 
# # # Run the application
# # shinyApp(ui = ui, server = server)
# # Define server logic required to draw a histogram ----
# server <- function(input, output, session) {
#     
#     #important to continuously remove values and prevent values from being added back to the table
#     
#     function_cleaned_data <- reactive({
#         clean_sheet_function(df,"Node 1")
#         
#     })
#     mod_df <- reactiveValues(x = clean_sheet_function(df,"Node 1"))
#     
#     #mod_df$x <- function_cleaned_data()
#     
#     
#     # shiny::observe({
#     #     shiny::updateSelectInput(session, inputId = "remove_row",
#     #                              choices = 1:nrow(mod_df$x))
#     # })
#     
#     shiny::observeEvent(input$button1, {
#         
#         #problem with filter: slider1 becuase I didn't have a slider1 in the UI
#         # mod_df$x <- mod_df$x %>%
#         #     filter(
#         #         value1 >= input$slider1[1] & value1 <= input$slider1[2]
#         #     )
#         
#         problem_rows <- mod_df$x %>%
#             #filter(between(TIMESTAMP, as.Date("2021-06-01"), as.Date("2021-07-10")))
#             filter((TIMESTAMP >= input$drange1[1] & TIMESTAMP <= input$drange1[2]),
#                    value1 >= input$slider2[1] & value1 <= input$slider2[2]
#             )
#         #this takes these rows out of the main dataframe
#         mod_df$x <- anti_join(mod_df$x, problem_rows)
#         
#         # mod_df$x <- mod_df$x %>%
#         #     dplyr::bind_rows(
#         #         dplyr::tibble(Height = input$height,
#         #                       Weight = input$weight)
#         #     )
#         DT::replaceData(proxy, mod_df$x)
#         
#     })
#     
#     # shiny::observeEvent(input$remove, {
#     #
#     #     mod_df$x <- mod_df$x[-as.integer(input$remove_row), ]
#     #
#     # })
#     
#     proxy <- DT::dataTableProxy('table')
#     shiny::observe({
#         
#         DT::replaceData(proxy, mod_df$x)
#         
#     })
#     
#     output$table <- DT::renderDT({
#         
#         mod_df$x
#         
#     })
#     
#     
#     output$soiltemp_download2 <- downloadHandler(
#         filename =
#             function() {
#                 paste0("test.csv")
#             }
#         ,
#         content = function(file) {
#             write_csv(mod_df$x, file)
#             
#             
#         }
#     ) # end of soil temp downloads
#     
#     
# }
# shinyApp(ui, server)

# library(shiny)
# library(DT)
# library(tidyverse)
# 
# # Define UI for application that edits the table
# ui = fluidPage(
#     DTOutput('x1'),
#     
#     # App title 
#     titlePanel("Downloading Data"),
#     
#     # Sidebar layout with input and output definitions 
#     sidebarLayout(
#         
#         # Sidebar panel for inputs 
#         sidebarPanel(
#             
#             # Input: Choose dataset 
#             selectInput("dataset", "Choose a dataset:",
#                         choices = c("Demo Table")),
#             
#             # Button
#             downloadButton("downloadData", "Download")
#             
#         ),
#         
#         # Main panel for displaying outputs 
#         mainPanel(
#             
#             tableOutput("table")
#             
#         )
#     ))
# 
# # Define server logic required
# server = function(input, output) {
#     x = iris
#     x$Date = Sys.time() + seq_len(nrow(x))
#     output$x1 = renderDT(x, selection = 'none', editable = TRUE)
#     
#     proxy = dataTableProxy('x1')
#     
#     observeEvent(input$x1_cell_edit, {
#         info = input$x1_cell_edit
#         str(info)
#         i = info$row
#         j = info$col
#         v = info$value
#         x[i, j] <<- DT::coerceValue(v, x[i, j])
#         replaceData(proxy, x, resetPaging = FALSE)  # important
#     })
#     
#     # Downloadable table (df) as csv
#     output$downloadData = downloadHandler(
#         filename = function() {
#             paste(input$dataset, ".csv", sep = "")
#         },
#         content = function(file) {
#             write.csv(x, file, row.names = FALSE)
#         }
#     )
# }
# 
# 
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
