
# Loading Libs ------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(shinycssloaders)
library(stringr)
library(Rspotify)
library(tidyverse)


# Loading Necessary Data and Functions ------------------------------------
source("User_Tracks_Function.R")
source("User_Features_Function.R")
source("User_Lyric_Analysis_Function.R")
source("User_Lyric_Generation_Function.R")
source("Overview_Figure_Function.R")
source("Playlist_Comparison_Function.R")

load("keys")

#Mandatory Fields
fieldsMandatory <- c("username", "playlists","features")

#Reactive values storage
RV <- reactiveValues()

keys <- keys

# UI ----------------------------------------------------------------------


ui <- dashboardPage(skin = "green",
                    
                    
                    # Dashboard Header --------------------------------------------------------
                    
                    
                    dashboardHeader(title = "Spotify Song Analysis"),
                    
                    # Dashboard Sidebar -------------------------------------------------------  
                    dashboardSidebar(
                      useShinyalert(),  # Set up shinyalert
                      useShinyjs(), #set up shinyjs
                      # Username Input ----------------------------------------------------------
                      textInput("username",label="Enter your Spotify Username or URI",placeholder = "Username or URI..."),
                      actionBttn("usernameclick","Search",style='minimal',size = "sm"),
                      selectizeInput("playlists",label="Choose which playlists to analyze",
                                     choices = "", multiple = T),
                      #Feature selection is the way to get the values, features is the id in the server side, relevant for disabling and enabling
                      checkboxGroupInput("features",label = "Select which features you'd like to analyze",
                                         choices = c("Explicit Status of Songs","Genre Information","Release Dates",
                                                     "Lyrics","Song Features")),
                      actionBttn("analyze","Analyze!",style = "minimal")
                      
                    ),
                    
                    # Dashboard Body ----------------------------------------------------------
                    
                    
                    dashboardBody(
                      tabsetPanel(
                        # Overview tab -------------------------------------------------------------
                        tabPanel(title = "Compare Playlists",
                                 
                                 # Fixing the header color -------------------------------------------------
                                 ##This fixes this the background color of the header to be spotify green. To be honest it is css and idk how it works. 
                                 #Need to learn. Got it from https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
                                 tags$head(tags$style(HTML('
                                                           /*logo*/
                                                           .skin-blue .main-header .logo {
                                                           background-color: #1DB954;
                                                           }
                                                           
                                                           /* navbar (rest of the header) */
                                                           .skin-blue .main-header .navbar {
                                                           background-color: #1DB954;
                                                           }        
                                                           
                                                           /* main sidebar */
                                                           .skin-grey .main-sidebar {
                                                           background-color: #191414;
                                                           }
                                                           
                                                           '))),
                                 
                                 # Mainbody format ---------------------------------------------------------
                                 fluidRow(),
                                 fluidRow(
                                   #Input Selection
                                   column(width = 4, style='padding-left:0px',
                                          box(width = 12,
                                              selectizeInput("Overview_Graph_Variables",label="Which feature would you like to vizualize?",
                                                             choices = ""),
                                              awesomeRadio("Playlist_Comparison_Order",label = "How would you like the figure ordered?",
                                                           choices = c("By Value","By Date of Creation"),
                                                           selected = "By Value",
                                                           inline = T,
                                                           status = "success",
                                              )
                                          )),
                                   #Figure
                                   column(width = 8, style='padding-left:0px', 
                                          box(width=12,
                                              withSpinner(plotOutput("Overview_Figure"),type = 6,color = "#1DB954")
                                          ))
                                 ),
                                 
                                 fluidRow(
                                   #Data table
                                   box(width=8,
                                       withSpinner(DT::dataTableOutput("datatable_overview"),type = 6,color = "#1DB954")
                                   ),
                                   #Input Selection
                                   column(width = 4,
                                          box(width = 12,
                                              selectizeInput("Overview_Table_Variables",label="Which features would you like to explore?",
                                                             choices = "", multiple = T),
                                              actionBttn("Overview_Table_Button_Click","Explore!",style = "minimal",color = "success")
                                          )
                                   )
                                 )
                                 ),
                        
                        # Compare Playlists Tab ---------------------------------------------------
                        tabPanel(title = "Analyze a Playlist")
                                 )
                                 )
                        )


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  ## initialize reactive values
  obj <- reactiveValues()
  
  #Initially disable all functions until a username has been found
  shinyjs::disable("playlists")
  
  #Disable exploration UI until data is collected
  shinyjs::disable("Overview_Table_Variables")
  shinyjs::disable("Overview_Table_Button_Click")
  shinyjs::disable("Overview_Graph_Variables")
  shinyjs::disable("Playlist_Comparison_Order")
  
  #generating playlist selection function -------------------------------------------------------
  observeEvent(input$usernameclick, {
    #This makes it so that we just have the username of the user
    if (str_detect(input$username,"spotify")==T) {
      user <- str_remove(input$username,"spotify:user:")
    } else {
      user <- input$username
    }
    #This gets the playlist
    user_playlists <- getPlaylists(user,token = keys) %>% as_tibble() %>% filter(tracks>0)
    #This tells us whether the search was a success or not
    if(nrow(user_playlists)==0){
      shinyalert("Oops!", "Username not found, please try again.", type = "error")
      updateSelectizeInput(session, "playlists",
                           label = "Choose which playlists to analyze",
                           choices = "")
      #If usename not found, diable future inputs
      shinyjs::disable("playlists")
      
    } else {
      shinyalert("Success!", "Username found. Please select playlists to analyze.", type = "success")
      updateSelectizeInput(session, "playlists",
                           label = "Choose which playlists to analyze",
                           choices = sort(user_playlists$name))
      #Enabling playlist and feature selection if and only if a username is found
      shinyjs::enable("playlists")
      
    }
  })
  
  #Checking if mandatory fields are filled in. Got this code from Dean Attali, need to figure out how it works
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "analyze", condition = mandatoryFilled)
  })
  
  observeEvent(input$analyze,{
    #Getting the order of the playlists
    #This makes it so that we just have the username of the user
    if (str_detect(input$username,"spotify")==T) {
      user <- str_remove(input$username,"spotify:user:")
    } else {
      user <- input$username
    }
    
    playlist_order <- getPlaylists(user,token = keys) %>% 
      as_tibble() %>% 
      mutate(Order=1:nrow(.)) %>% 
      rename("Playlist"=name) %>% 
      select(Playlist,Order)
    
    #Getting tracks from their playlist
    tracks <- Tracks_Function(user = input$username,playlists=input$playlists)    #You need to get rid of the playlist column so that it doesn't try to join by that, and you need to do distinct because if a song
    #appears in 2+ playlists it will get added twice to each corresponding column in the lefthand side
    if ("Song Features" %in% input$features | "Release Dates"  %in% input$features | 
        "Genre Information"  %in% input$features |
        "Explicit Status of Songs" %in% input$features) {
      Features <- Features_Function(track_data = tracks,features = input$features)
      
    }
    if ("Lyrics" %in% input$features) {
      #Getting lyrics
      Lyrics <- Lyric_Generation_Function(tracks)
      #Getting features of the lyrics
      Lyric_Features <- Lyric_Analysis_Function(Lyrics) %>% select(-Lyrics)
    }
    #Joining it all together. HAS TO BE A MORE EFFICIENT WAY TO DO THIS
    placeholder <- ifelse(("Song Features" %in% input$features | "Release Dates"  %in% input$features | 
                             "Genre Information"  %in% input$features |
                             "Explicit Status of Songs" %in% input$features) & 
                            !("Lyrics" %in% input$features),
                          Full_Data <- left_join(tracks,Features),
                          ifelse(!("Song Features" %in% input$features | "Release Dates"  %in% input$features | 
                                     "Genre Information"  %in% input$features |
                                     "Explicit Status of Songs" %in% input$features) & ("Lyrics" %in% input$features),
                                 yes=Full_Data <- left_join(tracks,Lyric_Features),
                                 no=Full_Data <- left_join(left_join(tracks,Features),Lyric_Features)))
    #Saving the data into a reactive object housed in obj
    obj$full_data <- Full_Data %>% select(!c(Artist_full,Id,Artist_id,Album_id))
    
    #Playlist Comparison reactive
    obj$playlist_comparison <- Full_Data %>% 
      select(!c(Artist_full,Id,Artist_id,Album_id)) %>% 
      Playlist_Comparison_Function()
    
    obj$playlist_comparison <- left_join(obj$playlist_comparison,playlist_order)
    
    #Data table is just Song and Playlist until user adds other features
    obj$datatable_data <- obj$playlist_comparison %>% select(Playlist,colnames(obj$playlist_comparison[2]))
    
    #Now that data is loaded, enable the exploration UI
    shinyjs::enable("Overview_Table_Variables")
    shinyjs::enable("Overview_Table_Button_Click")
    shinyjs::enable("Overview_Graph_Variables")
    shinyjs::enable("Playlist_Comparison_Order")
  })
  
  
  
  # Overview Figure ---------------------------------------------------------
  observeEvent(input$analyze,{
    ##Updating which features you can choose
    updateSelectizeInput(session, "Overview_Graph_Variables", label="Which feature would you like to vizualize?",
                         choices = sort(colnames(select(obj$playlist_comparison,-Playlist,-Order))))
  })
  
  output$Overview_Figure <- renderPlot({
    if(!is.null(obj$full_data)==T){
      req(input$Overview_Graph_Variables)
      Playlist_Comparison_Figure(data = obj$playlist_comparison,
                                 input = input$Overview_Graph_Variables, 
                                 order_selection=input$Playlist_Comparison_Order)
    }
  })
  
  # Overview Data Table -----------------------------------------------------
  observeEvent(input$analyze,{
    ##Updating which features you can choose
    updateSelectizeInput(session, "Overview_Table_Variables",label="Which features would you like to explore?",
                         choices = sort(colnames(select(obj$playlist_comparison,-Playlist,-Order))))
  })
  observeEvent(input$Overview_Table_Button_Click,{
    obj$datatable_data <- obj$playlist_comparison %>% select(Playlist,input$Overview_Table_Variables)
  })
  output$datatable_overview <- DT::renderDataTable({obj$datatable_data %>% 
      DT::datatable(rownames = F)})
}

shinyApp(ui, server)
