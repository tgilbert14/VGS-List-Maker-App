
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyalert)
library(tidyverse)

## Create function to create GUID for VGS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),collapsable = TRUE,

                # Application title
                titlePanel(" VGS List Maker"),
                br(),
              
              #   tags$style('.container-fluid {
              #                background-color: teal;
              # }'),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    checkboxInput(inputId = "named_num",
                                  label = "Named-Numeric List?",
                                  value = F),
                    #shiny::fileInput(inputId = "data_file", label = "Choose a list to create", accept = ".csv", multiple = F, placeholder = "vgs_list.csv"),
                    shiny::textInput(inputId = "list_name", label = "List Name", placeholder = "list name", value = NULL),
                    shiny::selectInput(inputId = "list_type", label = "List Type", choices = c("Normal list"=0, "Hierarchical list"=1), multiple = F, selected = F),
                    shiny::selectInput(inputId = "species_type", label = "Filter Type", choices = c("Other"="OT","GroundCover"="GC"), multiple = F, selected = F),
                    shiny::textInput(inputId = "list_decription", label = "Description", value = "NULL", placeholder = "type list decription here if needed"),
                    shiny::actionButton(inputId = "create", label = "Create List"),
                    br(),
                    shiny::actionButton(inputId = "create_2", label = "Update Survey")
                  ),
                  
                  # 
                  mainPanel(
                    withSpinner(textOutput("status"))
                  )
                )
                
)

# Define server logic required to draw a histogram
server <- function(input, session, output) {

  ## initial NULL value to stop spinner at start
  output$status <- renderText(NULL)
  
  ## alert so know how to order list
  shinyalert(
    "VGS List Sorter",
    html = TRUE,
    text = tagList(selectInput(inputId = "order_by",
                               label = "How would you like to order species by?",
                               choices = c("SpeciesName","CommonName"), selected = F)),
    #timer = 1000,
    showConfirmButton = T
  )
  
  observeEvent(input$create, {
    req(input$list_name)
    req(input$order_by)
    
    # if (input$named_num == TRUE) {
    #   shinyalert("Just Checking",
    #              text = "Named-Numeric .csv require 'CommonName' columns (odd columns) to be NUMERIC ONLY",
    #              type = "warning", closeOnEsc = T, confirmButtonText = "thanks", size = "s")
    # }
    
    output$status <- renderText({
      source("Functions/VGS_functions_R.R", local = T)
      source("Functions/list_creator.R", local = T)
      
      create_list(listName = input$list_name, IsHierarchical = input$list_type, spFilterType = input$species_type, description = input$list_decription)

      ## order all lists in VGS
      source("Functions/ordering_list_queries_abc.R", local = T)
      
      shinyalert("Stop! Connect your list to a survey as 'exclusive list'", text = "then export it before you move on to 'Update Survey'",
                 confirmButtonText = "Go to 'Update Survey' Next", type = "info", animation = "slide-from-bottom")
      
      #shinyalert(title = "DONE!!!", type = "info")
      print("Ordering Complete")
    })

  })
  ## End of Create button
  
  ## Update Survey Button
  observeEvent(input$create_2, {
    
    ## pop up to upload survey protocol
    survey<- choose.files(".vgsp", caption = "Select VGS survey to update", multi = F)
    
    t<- read.delim(survey, header = F, quote = "")
    
    ## find all lists in survey uploaded
    row_list<- grep(";ListName", x = t$V1)
    lists_being_used<- t[row_list,]
    
    def_info<- gregexec("ListName",lists_being_used)
    
    number_of_lists_found<- length(def_info)
    
    list_names<- list()
    i=1
    while (i < number_of_lists_found+1) {
      s<- def_info[[i]][1]
      e<- def_info[[i]][2]
      list_names[i]<- substr(lists_being_used[i],s+12,e-6)
      i=i+1
    }
    
    ## select list name to convert
    shinyalert("What List??",
               html = TRUE,
               text = tagList(selectInput(inputId = "list_convert",
                                          label = "Select one...",
                                          choices = c("",unlist(list_names)), selected = F)),
                                          confirmButtonText = "Sweet!", confirmButtonCol = "#00FF00")
    
    ## will override other "status" when starts
    output$status <- renderText({
      req(nchar(input$list_convert)>0)
      
      selected_list<- input$list_convert
      
      ## once confirm list is in survey and exported -> script ran to update survey info
      source("Functions/updating_e_to_h_list.R", local = T)
      
      ## close connections
      suppressWarnings(DBI::dbDisconnect(mydb))
      suppressWarnings(closeAllConnections())
                       
      print("Survey Updated!")
    })
  })
  
  
  # observeEvent(input$create, {
  #   req(input$list_name)
  #   Sys.sleep(4)
  #   ## session refresh
  #   session$reload()
  # })

}

# Run the application 
shinyApp(ui = ui, server = server)
