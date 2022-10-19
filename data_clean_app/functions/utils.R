pickerInput01 <- function(id) {
  pickerInput(id, 
              label = "Sheet Name", choices = NULL,
              selected = NULL,
              multiple = FALSE)
}

# update_picker01 <- function(input1, pickername, choices1) {
#   observeEvent(input1,{
#     updatePickerInput(session, pickername,
#                       selected = NULL, 
#                       choices = choices1
#     )
#     
#   })
# }
# thought it could be useful to put vartiabels in a dataframe then map the variables to update picker functions. Will try later
# vars <- tibble::tribble(
#   ~ input1,   ~ pickername, ~ choices1,
#   "alpha",     0,       1,
#   "beta",      0,    10,
#   "gamma",    -1,     1,
#   "delta",     0,     1,
# )