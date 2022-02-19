library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sortable)

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

# Define server logic required to fit the models
server <- shinyServer(function(input, output) {
  
  # to build the drag drop
  # the whole thing is a bucket_list, each of the two rectangles are add_rank_list
  output$drag_drop <- renderUI({
    bucket_list(
      header = 'Specify a Linear Regression:',
      add_rank_list(
        input_id = 'variables',
        text = strong("Avalable Variables"),
        labels = names(load_data()),
        options = sortable_options(multiDrag = TRUE)
      ), 
      add_rank_list(
        input_id = 'X', 
        text = strong("Predictors"), 
        labels = NULL, 
        options = sortable_options(multiDrag = TRUE)
      ),
      add_rank_list(
        input_id = 'Y', 
        text = strong("Outcomes"), 
        labels = NULL, 
        options = sortable_options(multiDrag = FALSE)
      )
    )
  })
  
  # begin by creating a 'store' object
  # store is a reactive container that can hold information across the app
  store <- reactiveValues()
  # add an empty list to store that will be used to save results
  store$results <- list()
  # create an iterator within store, this will count how many models have been fit
  store$number_of_models <- 0
  
  # reactive function to load my data, I use read_csv from tidyverse/readr
  load_data <- reactive({
    dat <- read_csv(input$load_file$datapath)
    return(dat)
    
  })
  
  
  # reactive function to fit the model, display the summary and AIC of the regressions
  fit_model <- reactive({
    # whenever the function is used we update 'number_of_models'
    # this will keep track of how many models we have created
    store$number_of_models <- store$number_of_models + 1
    # we will create a title for each model with Model and a number
    # ex Model1, Model2 etc. 
    model_title <- paste0('Model', store$number_of_models)
    # now lets create a unique id so we don't recreate the same model over and over again
    # REMEMBER to sort!!
    # I used + for collapse but you can use anything '' , ',' would both work but the collapse is important
    unique_id  <- paste0(sort(input$X), collapse = '+')
    
    # we will need our function to output the unique id the model_title and the model
    # we can do this with a list() function
      dat <- load_data()
      call <- paste(input$Y, "~", paste0(input$X, collapse = "+"))
      as.formula(call)
      m <- lm(formula = call, data = dat)
      out <- list(unique_id,model_title,assign(paste0('m_', store$number_of_models), summary(m)),
                  assign(paste0('m_', store$number_of_models), AIC(m)))
  })
  
  # this will search our results list and return the summary/AIC a user has selected using switch buttons
  # input 
  output$show_summary <- renderPrint({
    pull <- which(lapply(store$results, '[[', 2) == input$models)
    if(input$id1 == "TRUE" & input$id2 == "FALSE"){store$results[[pull]][3]}
    else if(input$id1 == "FALSE" & input$id2 == "TRUE"){store$results[[pull]][4]}
    else if(input$id1 == "TRUE" & input$id2 == "TRUE"){store$results[[pull]][3:4]}
    else{return("TRUN ON THE BUTTON :)")}
  })
  

  # fit_model is an action button defined in the UI
  # check an see if model is valid 
  # if the user drags more than one outcome variables in the bucket then a warning and stop the function
  observeEvent(input$fit_model, {
    if(length(input$Y) > 1){
      show_alert(title = 'Fitting Error', 
                 text = 'You must select single outcome variable to be fitted', 
                 type = 'error')
    }
  })
  
  
  observeEvent(input$fit_model,{
    req(length(input$X) > 1) # will not execute unless this is met
    # lets record the proposed new model
    proposal <- paste0(sort(input$X), collapse = '+')
    # this if statement will check and see if the proposal has already been created
    # if it is a new proposal we will run out lm function
    if(isFALSE(proposal %in% lapply(store$results, '[[', 1))){
      store$results[[store$number_of_models]] <- fit_model()
      
    }else{
      # if the proposal already exists as a unique_id (stored in the 1st element of the results list)
      # throw an warning and let them now which model it is
      # notice if the proposal already exists we DO NOT re-run the fit_model function 
      
      replicate <- which(proposal %in% lapply(store$results, '[[', 1))
      show_alert(
        title = 'Replicated Model!',
        text = paste(
          'You have already fit this model! See',
          store$results[[replicate]][[2]],
          'for results'
        ),
        type = 'success'
      )
    }
    
    # this will update the model options on the result page as we create more and more models
    updateSelectInput(inputId = 'models',
                      # update the titles for as more models come in 
                      choices = lapply(store$results, '[[', 2), 
                      # we want to show a new model whenever it is created
                      selected = lapply(store$results, '[[', 2)[[store$number_of_models]])
    
    # this will take us to the result page whenever the fit model button is hit
    # App is the id of my namvbar pg2 is the id of the results page
    # see the ui for examples
    updateNavbarPage(inputId = 'App', 
                     selected = 'pg2')
  })
  
  # when user clicks new plot lets take them back to the build page
  observeEvent(input$new_model,{
    updateNavbarPage(inputId = 'App', 
                     selected = 'pg1')
  })
  
  

  
})

shinyApp(ui, server)
