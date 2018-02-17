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




ui <- dashboardPage(skin = "red",
                    
                    dashboardHeader(title = "P.R.J.",tags$li(class = "dropdown", actionButton("DEBUG", "DEBUG"))),
                    dashboardSidebar(uiOutput("sidebarpanel")),
                    dashboardBody(
                      uiOutput("body")
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

  
  
  #Login 
  observeEvent(input$Login2,{
    df<-read.csv2(file = "person.csv",stringsAsFactors = FALSE)
    if((df[df$mail==input$userName2,]$password)==input$passwd2){
    USER$Logged<-TRUE
    connection$session<-df[df$mail==input$userName2,]
    connection$role<-df[df$mail==input$userName2,"role"]
    df<-read.csv2("project_person.csv",stringsAsFactors = FALSE)
    connection$projects<-df[df$person==connection$session$id,]
    
      }
    
  })
  
  source('body.R', local = TRUE)

    output$body <-  test
 
  
  #--------------------------------------------
  
  #GENERAL VARIABLES
  link_icon <- '<a href="http://recrutement.airfrance.com/offre-de-emploi/emploi-stage-assistant-e-communication-promotion-commerciale-h-f_6306.aspx"><img src="https://image.flaticon.com/icons/svg/25/25284.svg" height="15"></img></a>'
  
  #DASHBOARD VARIABLES
  

  

  
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
      
      write.table(x = line,file = "person.csv",append = TRUE,sep = ";",col.names = FALSE,quote = FALSE,row.names = FALSE)
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
     
     write.table(x = line,file = "calendar.csv",append = TRUE,sep = ";",col.names = FALSE,quote = FALSE,row.names = FALSE)
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
     
     write.table(x = line,file = "projects.csv",append = TRUE,sep = ";",col.names = FALSE,quote = FALSE,row.names = FALSE)
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
  

  


  #LINKS DATATABLE
  Projets <- c("PRJ2017", "RX-745")
  LienDrive <- c("https://drive.google.com/drive/folders/0B4PquDdceptmdlJCc3Z4ZUt5MEk")
  LienGit<- c("https://drive.google.com/drive/folders/0B4PquDdceptmdlJCc3Z4ZUt5MEk")
  Lien <- data.frame(Projets= Projets,LienDrive=LienDrive,LienGit=LienGit)
  
  observeEvent(input$mail_send, {
    PRJmail(to = input$mail_to,msg = input$mail_text,subject = input$mail_subject)
  })
  


  output$role <- reactive({
    connection$role
  })
  output$logged <- reactive({
    USER$Logged
  })
  outputOptions(output, "logged", suspendWhenHidden = FALSE)
  outputOptions(output, "role", suspendWhenHidden = FALSE)
  }


shinyApp(ui, server)
