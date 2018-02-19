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
                                                             
                                                             tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
                                                             tags$li(class = "dropdown", actionLink("header_login", textOutput("logintext"))),
                                                             tags$li(class = "dropdown", actionLink("DEBUG", "Debug"))
                                                             )),
                    dashboardSidebar(uiOutput("sidebarpanel")),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
                      ),
                      conditionalPanel(condition = "output.logged==TRUE",bodylogged)
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
  output$gantt<-gantt
  output$changeprofile<-changeprofile
  
  
  
  observeEvent(input$DEBUG,
               {
                 browser()
               })

  source('handlers.R', local = TRUE)
  data_project_id<-reactive(data_project[data_project$title==input$current_project,"id"])


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
  
  output$logged_user <- renderText({
    if(USER$Logged) return(paste("Bienvenue, ",connection$session$name,".",sep=""))
    #return("Login here")
  })
  
  observe({
    updateSelectInput(session, "current_project",
                      choices = data_project[data_project$id==connection$projects$project,"title"]
    )
    })
  observe({
    updateSelectInput(session, "mail_to",
                      choices = data_person[data_person$id %in% project_person.id()$person,"mail"]
    )
  })
  
  observe({
    if(connection$role=="Administrateur") updateTextInput(session,"change_Email",value = connection$session$mail)
    })
  
  observe({
    input$current_project
    df<-read.csv2(file = "projects.csv",stringsAsFactors = FALSE)
    project_id<-reactive( df[df$title==input$current_project,"id"])
    data_project <- read.table(file = "projects.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")[,-1]
    data_project<-data_project[data_project$title==input$current_project,]
    
  })
  
  project.id<-reactive(data_project[data_project$title==input$current_project,"id"])
  }


shinyApp(ui, server)
