#LOADING CSV
data_person <- read.table(file = "person.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
project_person<-read.table(file = "project_person.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")
project_person.id<-reactive(project_person[project_person$project==project.id(),])
table_person <- DT::renderDataTable(data_person[data_person$id %in% project_person.id()$person,], rownames = FALSE)

data_project <- read.table(file = "projects.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")[,-1]


table_projects <-  DT::renderDataTable( data_project[data_project$title==input$current_project,], rownames = FALSE) #DT::renderDataTable(data_project, rownames = FALSE)

gantt<-renderPlotly(printGantt(read.csv2(file="./calendar.csv")))
#LINKS DATATABLE
Projets <- c("PRJ2017", "RX-745")
LienDrive <- c("https://drive.google.com/drive/folders/0B4PquDdceptmdlJCc3Z4ZUt5MEk")
LienGit<- c("https://drive.google.com/drive/folders/0B4PquDdceptmdlJCc3Z4ZUt5MEk")
Lien <- data.frame(Projets= Projets,LienDrive=LienDrive,LienGit=LienGit)
table_archive <- DT::renderDataTable(Lien, rownames = FALSE)

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
               sidebarMenu(
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


