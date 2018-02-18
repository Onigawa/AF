## app.R ##
require(shiny)
require(shinydashboard)
require(plotly)
library(OpenCalendar)
require(stringr)
require(XML)
require(mailR)

source('utils.R', local = TRUE)
source('boxes.R', local = TRUE)
source('body.R', local = TRUE)



ui <- dashboardPage(skin = "red",
                    
                    dashboardHeader(title = "P.R.J.",tags$li(class = "dropdown", 
                                                             #actionButton("DEBUG", "DEBUG"),
                                                             tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
                                                             tags$li(class = "dropdown", actionLink("header_login", textOutput("logintext")))
                                                             )),
                    dashboardSidebar(uiOutput("sidebarpanel")),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
                      ),
                      #p(id = "logbox", login2),
                      #conditionalPanel(condition = "output.logged==FALSE && input.Login2==0",login2),
                      conditionalPanel(condition = "output.logged==TRUE",bodylogged)
                      
                      #uiOutput("body")
                      )
)


server <- function(input, output, session) {
  
  USER <- reactiveValues(Logged = F, Guard = 0)
  connection<-reactiveValues(session= "",role="")
  
  #all renders
  source('renders.R', local = TRUE)
  output$Projet_Titre<-Projet_Titre
  output$sidebarpanel<-sidebarpanel
  output$Gallerie<-Gallerie
  output$Projet_Resume<-Projet_Resume
  output$table_stage<-table_stage
  output$table_archive<-table_archive
  output$table_projects<-table_projects
  output$table_person<-table_person
  
  observeEvent(input$DEBUG,
               {
                 #DEBUG
                 dummy<-0
                 dummy<-1
               })

  source('handlers.R', local = TRUE)
  


  output$role <- reactive({
    connection$role
  })
  output$logged <- reactive({
    USER$Logged
  })
  outputOptions(output, "logged")
  outputOptions(output, "role", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$header_login, {
    if(USER$Logged){
      session$reload()
    }else{
      showModal(modalDialog(
        login2,
        easyClose = TRUE,
        footer = NULL
      ))
    }  

  })
  
  # show "Login" or "Logout" depending on whether logged out or in
  output$logintext <- renderText({
    if(USER$Logged) return("Logout here")
    return("Login here")
  })
  
  observe({
    updateSelectInput(session, "current_project",
                      choices = data_project[data_project$id==connection$projects$project,"title"]
    )})
  }


shinyApp(ui, server)
