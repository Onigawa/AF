## app.R ##
require(shiny)
require(shinydashboard)
require(plotly)
library(OpenCalendar)
require(stringr)
require(XML)
require(mailR)

printGantt<-function(calendar){
  
  # Convert to dates
  calendar$start<-as.Date(calendar$start,format = "%Y-%m-%d")
  calendar$end<-as.Date(calendar$end,format = "%Y-%m-%d")
  
  # Initialize empty plot
  p <- plot_ly()
  
  # Each task is a separate trace
  # Each trace is essentially a thick line plot
  # x-axis ticks are dates and handled automatically
  
  for(i in 1:(nrow(calendar) -1)){
    calendar$duration<-calendar$end[i]-calendar$start[i]
    p <- suppressWarnings(add_trace(p,
                                    x = c(calendar$start[i], calendar$end[i] ),  # x0, x1
                                    y = c(i, i),  # y0, y1
                                    mode = "lines",
                                    line = list( width = 20),
                                    showlegend = F,
                                    hoverinfo = "text",
                                    
                                    # Create custom hover text
                                    text = paste("Task: ", calendar$name[i], "<br>",
                                                 "Duration: ", calendar$duration[i], "days<br>",
                                                 "Resource: ", calendar$participantsID[i]),
                                    
                                    
                                    evaluate = T  # needed to avoid lazy loading
    ))
  }
  suppressWarnings(p)
}

PRJmail<-function(to,subject,msg){
    if(is.na(to)||is.null(subject)||is.null(msg)){
      warning("Empty Fields")
    }else{
      sender <- "postmasterprj@gmail.com"
        send.mail(from = sender,
                               to = to,
                               subject = subject,
                               body = msg,
                               smtp = list(host.name = "smtp.gmail.com", port = 465, 
                                                                    user.name = "postmasterprj@gmail.com",            
                                                                    passwd = "P.R.J.123", ssl = TRUE),
                               authenticate = TRUE,
                               send = TRUE)}
}

randString <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

stage_rss<-function(link="http://recrutement.airfrance.com/handlers/offerRss.ashx?LCID=1036&Rss_Contract=3442"){
  doc<-xmlParse(link)
  
  src<-xpathApply(xmlRoot(doc), "//item")
  
  for (i in 1:length(src)) {
    if (i==1) {
      foo<-xmlSApply(src[[i]], xmlValue)
      DATA<-data.frame(t(foo), stringsAsFactors=FALSE)
    }
    else {
      foo<-xmlSApply(src[[i]], xmlValue)
      tmp<-data.frame(t(foo), stringsAsFactors=FALSE)
      DATA<-rbind(DATA, tmp)
    }
    
  }
  DATA[,1]<-sprintf(paste('<a href="',DATA[,1],'" target="_blank" class="btn btn-primary">Lien</a>'))
  colnames(DATA)<-c("Lien","Categorie","Cat1","Localisation","Intitule","Description","Date de Publication")
  return(DATA)
}




ui <- dashboardPage(skin = "red",
                    
                    dashboardHeader(title = "P.R.J."),
                    dashboardSidebar(uiOutput("sidebarpanel")),
                    dashboardBody(uiOutput("body"))
)

#LOGIN INFOS
login_details <- data.frame(user = c("admin@airfrance.fr", "student@ecole.fr"),
                            pswd = c("admin", "student"))

login <- box(
  title = "Authentification",
  textInput("userName", "Email (admin@airfrance.fr OU student@ecole.fr)"),
  passwordInput("passwd", "Password (admin ou student)"),
  br(),
  actionButton("Login", "Se connecter")
)

login2 <- box(
  title = "Authentification",
  textInput("userName2", "Email (admin@airfrance.fr OU student@ecole.fr)"),
  passwordInput("passwd2", "Password (admin ou student)"),
  br(),
  actionButton("Login2", "Se connecter")
)


newprofile <- box(solidHeader = TRUE, status = "danger",
  title = "Ajouter une personne",
  textInput("add_Projet_Personne", "Identifiants des Projets"),
  textInput("add_Nom", "Nom"),
  textInput("add_Prenom", "Prenom"),
  textInput("add_Email", "Email"),
  textInput("add_Service", "Service"),
  textInput("add_Ecole", "Ecole"),
  
  #passwordInput("passwd", "Password (admin ou student)"),
  br(),
  actionButton("add_Personne", "Sauvegarder")
)

newevent <- box(solidHeader = TRUE, status = "danger",
  title = "Ajouter une tache",footer = "Format de date: 2017-11-07 09:42:25",
  textInput("add_Event_Start", "Debut de la tache"),
  textInput("add_Event_End", "Fin de la tache"),
  textInput("add_Event_Nom", "Nom de la tache"),
  textInput("add_Event_Location", "Localisation"),
  textInput("add_Event_Participants", "Identifiants des participants"),
  
  
  #passwordInput("passwd", "Password (admin ou student)"),
  br(),
  actionButton("add_Event", "Sauvegarder")
)

newproject <- box(solidHeader = TRUE, status = "danger",
  title = "Ajouter un Projet",
  textInput("add_Project_Title", "Titre du projet"),
  textInput("add_Project_Type", "Type de Projet"),
  textInput("add_Project_School", "Nom de l'Ecole"),
  textInput("add_Project_Location", "Localisation"),
  textInput("add_Project_Participants", "Identifiants des participants"),
  
  
  #passwordInput("passwd", "Password (admin ou student)"),
  br(),
  actionButton("add_Project", "Sauvegarder")
)

changeprofile <- box(solidHeader = TRUE, status = "danger",
                  title = "Modifier une personne",
                  conditionalPanel(condition = "connection.session.role=='Administrateur'", textInput("change_Email", "Email du profil a modifer")),
                  textInput("change_Projet_Personne", "Identifiants des Projets"),
                  textInput("change_Nom", "Nom"),
                  textInput("change_Prenom", "Prenom"),
                  textInput("change_Service", "Service"),
                  textInput("change_Ecole", "Ecole"),
                  
                  #passwordInput("passwd", "Password (admin ou student)"),
                  br(),
                  actionButton("change_Personne", "Sauvegarder")
)

changeproject <- box(solidHeader = TRUE, status = "danger",
                  title = "Modifier un Projet",
                  textInput("change_Project_Title", "Titre du projet a modifier"),
                  textInput("change_Project_Type", "Type de Projet"),
                  textInput("change_Project_School", "Nom de l'Ecole"),
                  textInput("change_Project_Location", "Localisation"),
                  textInput("change_Project_Participants", "Identifiants des participants"),
                  
                  
                  #passwordInput("passwd", "Password (admin ou student)"),
                  br(),
                  actionButton("change_Project", "Sauvegarder")
)

changeevent <- box(solidHeader = TRUE, status = "danger",
                title = "Modifier une tache",footer = "Format de date: 2017-11-07 09:42:25",
                textInput("change_Event_Start", "Debut de la tache"),
                textInput("change_Event_End", "Fin de la tache"),
                textInput("change_Event_Nom", "Nom de la tache"),
                textInput("change_Event_Location", "Localisation"),
                textInput("change_Event_Participants", "Identifiants des participants"),
                
                
                #passwordInput("passwd", "Password (admin ou student)"),
                br(),
                actionButton("change_Event", "Sauvegarder")
)

server <- function(input, output, session) {

  
  # #AUTHENTICATION --------------------------------
  # login.page = paste(
  #   isolate(session$clientData$url_protocol),
  #   "//",
  #   isolate(session$clientData$url_hostname),
  #   ":",
  #   isolate(session$clientData$url_port),
  #   sep = ""
  # )
  
  USER <- reactiveValues(Logged = F, Guard = 0)
  connection<-reactiveValues(session= "")
  
  observeEvent(input$Login2,{
    df<-read.csv2(file = "person.csv",stringsAsFactors = FALSE)
    if((df[df$mail==input$userName2,]$password)==input$passwd2){
    USER$Logged<-TRUE
    connection$session<-df[df$mail==input$userName2,]
      }
    
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Profil", tabName = "profile", icon = icon("user")),
        menuItem("Archives", tabName = "archive", icon = icon("archive")),
        menuItem("Stages", tabName = "internship", icon = icon("thumbs-up")),
        menuItem("Mail", tabName = "mail", icon = icon("envelope")),
        conditionalPanel(condition =  "input.userName2=='admin@airfrance.fr'" ,menuItem("Administrateur", tabName = "ajout", icon = icon("plane")))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
      )
      tabItems(
        #DASHBOARD TAB CONTENT
        tabItem(tabName = "dashboard",
                fluidRow(
                  column(width = 12, offset = 5, h1("Bienvenue"), br(), br()
                  )
                ),
                fluidRow(
                  #Projets
                  box(
                    title = "Projet", width = 6, solidHeader = TRUE, status = "danger",
                    DT:: dataTableOutput('table_projects')
                  ),
                  #Workflow
                  box(title = "WorkFlow", width = 6, solidHeader = TRUE, status = "danger",
                      renderPlotly(printGantt(read.csv2(file="./calendar.csv"))),
                      actionButton("box_add_event", "Ajouter une tache")
                      )
                ),
                fluidRow(
                  #Images
                  box(
                    title = "Images", width = 6, solidHeader = TRUE, status = "danger",
                    htmlOutput("Gallerie")
                  ),
                  #Contacts
                  box(
                    title = "Contacts", width = 6, solidHeader = TRUE, status = "danger",
                    DT:: dataTableOutput('table_person'),
                    conditionalPanel(condition =  "input.userName=='admin@airfrance.fr'",
                                     actionButton("box_add_person", "Ajouter une personne")) 
                  )
                )
        ),
        
        #PROFILE PAGE CONTENT
        tabItem(tabName = "profile",
                fluidPage(
                  column(width = 12,
                         #Projets
changeprofile
                  )
                )
        ),
        
        #MES FICHIERS TAB CONTENT
        tabItem(tabName = "archive",
                h2("Mes Fichiers"),
                fluidPage(DT:: dataTableOutput('table_archive')
                )
        ),
        
        #AJOUT TAB CONTENT
        tabItem(tabName = "ajout",
                h2("Ajout"),
                
                fluidPage(       
                  newproject,         
                  changeprofile,
                  changeevent,
                  changeproject
                                          )

        ),
        
        #STAGES TAB CONTENT
        tabItem(tabName = "internship",
                h2("Offres de stages"),
                fluidPage(DT:: dataTableOutput('table_stage')
                )
        ),
        #MAIL TAB CONTENT
        tabItem(tabName = "mail",
                h2("Send E-Mail"),
                mainPanel(
                  #textInput(inputId = "mail_from",label = "From",placeholder = "email@adress.com",width = "100%"),
                  textInput(inputId = "mail_to",label = "To",placeholder = "email@adress.com",width = "100%"),
                  textInput(inputId = "mail_subject",label = "Objet",placeholder = "Object",width = "100%"),
                  textAreaInput(inputId = "mail_text",label = "Message",placeholder = "...",rows = 15,width = "200%"),
                  actionButton(inputId = "mail_send",label = "Send")
                ))
      )
    } else {
      login2
    }
  })
  
  #--------------------------------------------
  
  #GENERAL VARIABLES
  link_icon <- '<a href="http://recrutement.airfrance.com/offre-de-emploi/emploi-stage-assistant-e-communication-promotion-commerciale-h-f_6306.aspx"><img src="https://image.flaticon.com/icons/svg/25/25284.svg" height="15"></img></a>'
  
  #DASHBOARD VARIABLES
  
  output$Projet_Titre <- renderText({"TITRE DU PROJET"})
  output$Projet_Resume <- renderText({"
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore
    magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
    consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."})
  
  output$Gallerie <- renderUI({
    tagList(
      img(src = "http://via.placeholder.com/300", height = 100, width = 100),
      img(src = "http://via.placeholder.com/300", height = 100, width = 100),
      img(src = "http://via.placeholder.com/300", height = 100, width = 100),
      img(src = "http://via.placeholder.com/300", height = 100, width = 100),
      img(src = "http://via.placeholder.com/300", height = 100, width = 100),
      img(src = "http://via.placeholder.com/300", height = 100, width = 100),
      img(src = "http://via.placeholder.com/300", height = 100, width = 100)
    )
  })
  
  #Ajout de personnes
 observeEvent(eventExpr = input$add_Personne,handlerExpr =  {
       line<-data.frame(id=max(read.csv2("person.csv")$id)+1,
                     name=input$add_Nom,
                     surname=input$add_Prenom,
                     mail=input$add_Email,
                     service=input$add_Service,
                     school=input$add_Ecole,
                     password="123")
    
    if(!(""%in%line[1,])){
      
      write.table(x = line,file = "person.csv",append = TRUE,sep = ";",col.names = FALSE,quote = FALSE)
    }else{
        warning("Empty Fields")
      }
      } )
 
 #Ajout de taches
 observeEvent(eventExpr = input$add_Event,handlerExpr =  {
   line<-data.frame(id=max(read.csv2("calendar.csv")$id)+1,
                    start=input$add_Event_Start,
                    end=input$add_Event_End,
                    name=input$add_Event_Nom,
                    location=input$add_Event_Location,
                    participantsID=input$add_Event_Participants)
   
   if(!(""%in%line[1,])){
     
     write.table(x = line,file = "calendar.csv",append = TRUE,sep = ";",col.names = FALSE,quote = FALSE)
   }else{
     warning("Empty Fields")
   }
 } )
 
 #Ajout de projets
 observeEvent(eventExpr = input$add_Project,handlerExpr =  {
   line<-data.frame(id=max(read.csv2("projects.csv")$id)+1,
                    title=input$add_Project_Title,
                    type=input$add_Project_Type,
                    school=input$add_Project_School,
                    location=input$add_Project_Location,
                    participantsID=input$add_Project_Participants)
   
   if(!(""%in%line[1,])){
     
     write.table(x = line,file = "projects.csv",append = TRUE,sep = ";",col.names = FALSE,quote = FALSE)
   }else{
     warning("Empty Fields")
   }
 } )
 
 #Modif person
 observeEvent(eventExpr = input$change_Personne,handlerExpr =  {
   line<-data.frame(id=max(read.csv2("person.csv")$id)+1,
                    name=input$change_Nom,
                    surname=input$change_Prenom,
                    mail=input$change_Email,
                    service=input$change_Service,
                    school=input$change_Ecole,
                    password="123",stringsAsFactors = FALSE)
   
   df<-read.csv2("person.csv",stringsAsFactors = FALSE)
   

   
   if(!(""%in%line[1,])){
     
     temp<-df[df$mail==line$mail,]
     temp$name<-line$name
     temp$surname<-line$surname
     temp$service<-line$service
     temp$school<-line$school
     df[which(df$mail==mail),]<-temp
     write.csv2(x = df,file = "person.csv",row.names = FALSE,quote = FALSE)
   }else{
     warning("Empty Fields")
   }
 } )

 #Modif de projets
 observeEvent(eventExpr = input$change_Project,handlerExpr =  {
   line<-data.frame(id=max(read.csv2("projects.csv")$id)+1,
                    title=input$change_Project_Title,
                    type=input$change_Project_Type,
                    school=input$change_Project_School,
                    location=input$change_Project_Location,
                    participantsID=input$change_Project_Participants,stringsAsFactors = FALSE)
   df<-read.csv2("projects.csv",stringsAsFactors = FALSE)
   
   if(!(""%in%line[1,])){
     temp<-df[df$title==line$title,]
     temp$type<-line$type
     temp$school<-line$school
     temp$location<-line$location
     temp$participantsID<-line$participantsID
     df[which(df$title==line$title),]<-temp
     write.csv2(x = df,file = "projects.csv",row.names = FALSE,quote = FALSE)
   }else{
     warning("Empty Fields")
   }
 } )
 
 #Modif taches
 observeEvent(eventExpr = input$change_Event,handlerExpr =  {
   line<-data.frame(id=max(read.csv2("calendar.csv")$id)+1,
                    start=input$change_Event_Start,
                    end=input$change_Event_End,
                    name=input$change_Event_Nom,
                    location=input$change_Event_Location,
                    participantsID=input$change_Event_Participants,stringsAsFactors = FALSE)
   df<-read.csv2("calendar.csv",stringsAsFactors = FALSE)
   
   
   if(!(""%in%line[1,])){
     temp<-df[df$name==line$name,]
     temp$start<-line$start
     temp$end<-line$end
     temp$location<-line$location
     temp$participantsID<-line$participantsID
     df[which(df$name==line$name),]<-temp
     write.csv2(x = df,file = "calendar.csv",row.names = FALSE,quote = FALSE)
   }else{
     warning("Empty Fields")
   }
 } )
 
 observeEvent(input$box_add_event, {
   showModal(modalDialog(
     newevent,
     easyClose = TRUE
   ))
 }) 
 
 observeEvent(input$box_add_person, {
   showModal(modalDialog(
     newprofile,
     easyClose = TRUE,size = 
   ))
 }) 
 
 
  #PROFILE VARIABLES
  observeEvent(input$SaveFin, {
    ###AJOUTER MODIFICATION DES FICHIERS ICI
  })
  
  observe({
    updateTextInput(session, "input_prenom", value = paste(input$input_prenom))
    updateTextInput(session, "input_nom", value = paste(input$input_nom))
    updateTextInput(session, "input_email", value = paste(input$input_email))
    updateTextInput(session, "input_ecole", value = paste(input$input_ecole))
    updateTextInput(session, "input_majeure", value = paste(input$input_majeure))
    updateTextInput(session, "input_localisation", value = paste(input$input_localisation))
  })
  

  
  #LOADING CSV
  data_person <- read.table(file = "person.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")[,-1]
  output$table_person <- DT::renderDataTable(data_person, rownames = FALSE)
  
  data_project <- read.table(file = "projects.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")[,-1]
  output$table_projects <- DT::renderDataTable(data_project, rownames = FALSE)

  #LINKS DATATABLE
  Projets <- c("PRJ2017", "RX-745")
  LienDrive <- c("https://drive.google.com/drive/folders/0B4PquDdceptmdlJCc3Z4ZUt5MEk")
  LienGit<- c("https://drive.google.com/drive/folders/0B4PquDdceptmdlJCc3Z4ZUt5MEk")
  Lien <- data.frame(Projets= Projets,LienDrive=LienDrive,LienGit=LienGit)
  
  observeEvent(input$mail_send, {
    PRJmail(to = input$mail_to,msg = input$mail_text,subject = input$mail_subject)
  })
  
  output$table_archive <- DT::renderDataTable(Lien, rownames = FALSE)
  
  Stages <- stage_rss()
  output$table_stage <- DT::renderDataTable(Stages[,-c(3,6)], escape = FALSE, rownames = FALSE)

  }


shinyApp(ui, server)
