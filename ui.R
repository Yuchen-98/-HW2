library(shinyWidgets)

ui <- shinyUI(navbarPage(id = 'App', # id for the navbar
                         title = 'Compare Many Regression Models:',
                         tabPanel(value = 'pg1', # see server for how this is used to aut-update a page
                                  # define the data load page
                                  title = 'Load Data and Fit Models',
                                  fileInput(
                                    inputId = 'load_file',
                                    buttonLabel = "Browse...",
                                    label = 'Select a .csv file:',
                                    accept = ".csv"
                                  ),
                                  uiOutput('drag_drop'), # drag drop ui
                                  
                                  # click to fit a model this is a button
                                  fluidRow(column(
                                    width = 3,
                                    offset = 8,
                                    actionButton(
                                      inputId = 'fit_model',
                                      label = "Fit Model",
                                      width = '110%'
                                    )
                                  ))
                         ),
                         
                         
                         # define the results page 
                         tabPanel(value = 'pg2', # see server for how this is used to aut-update a page
                                  title = 'Compare Results', sidebarLayout(
                                    # place to flip through fitted models 
                                    sidebarPanel(selectInput(inputId = 'models', 
                                                             label = 'Fitted Models:', 
                                                             choices = NULL), 
                                                 # switch button to allow user to customize the results
                                                 materialSwitch(inputId = "id1",
                                                                label = "Summary",
                                                                value = TRUE,
                                                                status = "primary"),
                                                 materialSwitch(inputId = "id2",
                                                                label = "AIC",
                                                                value = TRUE,
                                                                status = "success"),
                                                 # button to take user back to the data page
                                                 actionButton(inputId = 'new_model',
                                                              label = 'Fit a new model')
                                                 
                                                 
                                    ),
                                    mainPanel(
                                      # big panel to show the results!!
                                      verbatimTextOutput('show_summary')
                                    )))
))