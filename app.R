
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)

## Create function to create GUID for VGS
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),collapsable = TRUE,

                # Application title
                titlePanel(" VGS List Maker", windowTitle = T),
                
                tags$style('.container-fluid {
                             background-color: teal;
              }'),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    #shiny::fileInput(inputId = "data_file", label = "Choose a list to create", accept = ".csv", multiple = F, placeholder = "vgs_list.csv"),
                    shiny::textInput(inputId = "list_name", label = "List Name", placeholder = "list name", value = NULL),
                    shiny::selectInput(inputId = "list_type", label = "List Type", choices = c("Normal list"=0, "Hierarchical list"=1), multiple = F, selected = F),
                    shiny::selectInput(inputId = "species_type", label = "Filter Type", choices = c("Other"="OT","GroundCover"="GC"), multiple = F, selected = F),
                    shiny::textInput(inputId = "list_decription", label = "Description", value = NULL, placeholder = "type list decription here if needed"),
                    shiny::actionButton(inputId = "create", label = "Create List")
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
  
  observeEvent(input$create, {
    req(input$list_name)
    output$status <- renderText({
      
      source("Functions/VGS_functions_R.R", local = T)
      source("Functions/list_creator.R", local = T)
      
      create_list(listName = input$list_name, IsHierarchical = input$list_type, spFilterType = input$species_type, description = input$list_decription)
      
      ## close connections
      DBI::dbDisconnect(mydb)
      closeAllConnections()
      
      if (nchar(input$list_name) == 0) {
        print("No list name")
      }
      if (nchar(input$list_name) != 0 && tolower(input$list_name) != "chaz stinks") {
        print(paste0("'",input$list_name, "' list created and inserted into VGS50.db -> ",Sys.Date()))
      }
      
      if (tolower(input$list_name) == "chaz stinks") {
        print("Chaz is Mad!! Start Over and think about what you said.")
      }
      

    })
    ## chaz sound board
    if (nchar(input$list_name) == 0) {
      insertUI(selector = "#create",
               where = "afterEnd",
               ui = tags$audio(src = "I hate the VGS team.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;"))
    }
    if (nchar(input$list_name) > 1 && tolower(input$list_name) != "chaz stinks") {
      insertUI(selector = "#create",
               where = "afterEnd",
               ui = tags$audio(src = "Nice.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;"))
    }

    if (tolower(input$list_name) == "chaz stinks") {
      insertUI(selector = "#create",
               where = "afterEnd",
               ui = tags$audio(src = "F VGS man.mp3", type = "audio/mp3", autoplay = T, controls = NA, style="display:none;"))
      ## self destruct
      removeUI(selector = "div:has(> #list_name)")
      removeUI(selector = "div:has(> #species_type)")
      removeUI(selector = "div:has(> #list_type)")
      removeUI(selector = "div:has(> #list_decription)")
      removeUI(selector = "div:has(> #create)")
    }
    
    insertUI(selector = "#create",
             where = "afterEnd",
             ui=tags$img(
               src = "https://portal.dev.vgs.arizona.edu/Content/Images/Headshots/csperry.jpg",
               style = "position:absolute;left:420px;",
               width = "360px", height = "360px"))
    
    

  })
  
  observeEvent(input$create, {
    req(input$list_name)
    Sys.sleep(4)
    ## session refresh
    session$reload()
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
