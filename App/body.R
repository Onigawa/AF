
body<-renderUI({
  if (USER$Logged == TRUE) {
    bodylogged
  } else {
    login2
  }
})

bodylogged<-tabItems(
  #DASHBOARD TAB CONTENT
  tabItem(tabName = "dashboard",
          
          fluidRow(
            column(width = 6, offset = 5, h1("Bienvenue"), br(), br()
            ),
            column(width = 6,offset = 5,
                   selectInput(inputId = "current_project",label = "Votre Projet",choices =NULL ))#
          ),
          fluidRow(
            #Projets
            box(
              title = textOutput("project_title"), width = 6, solidHeader = TRUE, status = "danger",
             # DT:: dataTableOutput('table_projects')
             textOutput("project_description")
              
            ),
            #Workflow
            box(title = "WorkFlow", width = 6, solidHeader = TRUE, status = "danger",
                plotlyOutput(outputId = "gantt"),
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
              conditionalPanel(condition =  "output.role=='Administrateur'",
                               actionButton("box_add_person", "Ajouter une personne")) 
            )
          )
  ),
  
  #PROFILE PAGE CONTENT
  tabItem(tabName = "profile",
          fluidPage(
            column(width = 12,
                   #Projets
                   uiOutput("changeprofile") 
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
            preprofile,
            preevent,
            preproject,
            preprojectperson,
            newproject
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
            selectInput(inputId = "mail_to",label = "To",choices = NULL,width = "100%",multiple = TRUE),
            textInput(inputId = "mail_subject",label = "Objet",placeholder = "Object",width = "100%"),
            textAreaInput(inputId = "mail_text",label = "Message",placeholder = "...",rows = 15,width = "200%"),
            actionButton(inputId = "mail_send",label = "Send")
          ))
)
