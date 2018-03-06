#LOADING CSV
data_person <- read.table(file = "person.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
project_person<-read.table(file = "project_person.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
project_person.id<-reactive(project_person[project_person$project==project.id(),])
table_person <- DT::renderDataTable(data_person[data_person$id %in% project_person.id()$person,], rownames = FALSE)
current_project_title<-renderText(data_project[data_project$id==connection$projects$project,"title"])


data_project <- read.table(file = "projects.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")


table_projects <-  DT::renderDataTable( data_project[data_project$title==input$current_project,], rownames = FALSE) #DT::renderDataTable(data_project, rownames = FALSE)

project_title<-renderText(data_project[data_project$title==input$current_project,"title"])
project_description<-renderText(data_project[data_project$title==input$current_project,"description"])

data_event<-read.csv2(file="./calendar.csv",stringsAsFactors = FALSE)

gantt<-renderPlotly(printGantt(data_event[data_event$project==project.id(),]))

#LINKS DATATABLE
Lien<-read.csv2("files.csv",stringsAsFactors = FALSE)
Lien[,"link"]<-sprintf(paste('<a href="',Lien[,"link"],'" target="_blank" class="btn btn-primary">Lien</a>',sep = ""))
colnames(Lien)<-c("id","project","Nom de stockage","Lien")
table_archive <- DT::renderDataTable(Lien[Lien$project==project.id(),c("Nom de stockage","Lien")], rownames = FALSE,escape=FALSE)

Stages <- stage_rss()
table_stage <- DT::renderDataTable(Stages[,-c(3,6)], escape = FALSE, rownames = FALSE)

Projet_Titre <- renderText({"TITRE DU PROJET"})

Projet_Resume <- renderText({"
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore
    magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
    consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."})


  Gallerie <- renderUI({
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

#Side menu

  sidebarpanel <- renderUI(
  {
    if (USER$Logged == TRUE) {
        
      switch(connection$role,
             "Administrateur"={
               sidebarMenu(id = "tabs",
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                 menuItem("Profil", tabName = "profile", icon = icon("user")),
                 menuItem("Archives", tabName = "archive", icon = icon("archive")),
                 menuItem("Stages", tabName = "internship", icon = icon("thumbs-up")),
                 menuItem("Mail", tabName = "mail", icon = icon("envelope")),
                 menuItem("Administrateur", tabName = "ajout", icon = icon("plane"))
               )},
             "Etudiant"={
               sidebarMenu(
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                 menuItem("Profil", tabName = "profile", icon = icon("user")),
                 menuItem("Archives", tabName = "archive", icon = icon("archive")),
                 menuItem("Stages", tabName = "internship", icon = icon("thumbs-up")),
                 menuItem("Mail", tabName = "mail", icon = icon("envelope"))
               )
             }
             
             
      )
      
    }
  })
  
  headers<-renderUI({
    if (USER$Logged == TRUE){
      return(headerlogged)
    }else{
      return(headeroff)
      }
    
    
  })
  

  changeprofile<-renderUI({
    if (USER$Logged == TRUE) {
    switch(connection$role,
           #"Administrateur"={changeprofileadmin},
           changeprofilebase
           
    )
    }
  })

