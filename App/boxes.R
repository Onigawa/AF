# login <- box(
#   title = "Authentification",
#   textInput("userName", "Email (admin@airfrance.fr OU student@ecole.fr)"),
#   passwordInput("passwd", "Password (admin ou student)"),
#   br(),
#   actionButton("Login", "Se connecter")
# )

login2 <- fluidPage(
  title = "Authentification",
  textInput("userName2", "Email (admin@airfrance.fr OU student@ecole.fr)",value = "admin@airfrance.fr"),
  passwordInput("passwd2", "Password (admin ou student)",value = "admin"),
  br(),
  actionButton("Login2", "Se connecter")
)


preprofile <- box(
  title = "Modifier un profil",
  textInput("preprofile_email", "Email du compte a modifier"),
  br(),
  actionButton("preprofile_push", "Modifier")
)

preevent <- box(
  title = "Modifier une tache",
  textInput("preevent_name", "Nom de l'evenement a modifier"),
  br(),
  actionButton("preevent_push", "Modifier")
)

preproject <- box(
  title = "Modifier un projet",
  textInput("preproject_name", "Nom du projet a modifier"),
  br(),
  actionButton("preproject_push", "Modifier")
)

postprofile<-fluidPage(
                 title = paste("Modifier le compte"),
                 textOutput("postprofilename"),
                 textInput("change_Nom_post", "Nom"),
                 textInput("change_Prenom_post", "Prenom"),
                 textInput("change_Service_post", "Service"),
                 textInput("change_Ecole_post", "Ecole"),
                 textInput("change_Role_post", "Role"),
                 
                 #passwordInput("passwd", "Password (admin ou student)"),
                 br(),
                 actionButton("change_Personne_post", "Sauvegarder")
)



newprofile <- box(solidHeader = TRUE, status = "danger",
                  title = "Ajouter une personne",
                  textInput("add_Projet_Personne", "Identifiants des Projets"),
                  textInput("add_Nom", "Nom"),
                  textInput("add_Prenom", "Prenom"),
                  textInput("add_Email", "Email"),
                  textInput("add_Service", "Service"),
                  textInput("add_Ecole", "Ecole"),
                  selectInput("add_role","Niveau d'autorisation",choices = c("Administrateur","Etudiant")),
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

newproject <- box(
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

changeprofilebase <- box(
                     title = "Modifier vos informations",
                     textInput("change_Nom", "Nom",value = NULL),
                     textInput("change_Prenom", "Prenom",value = NULL),
                     textInput("change_Service", "Service",value = NULL),
                     textInput("change_Ecole", "Ecole",value = NULL),
                     br(),
                     actionButton("change_Personne", "Sauvegarder")
)

# changeprofileadmin <- box(solidHeader = TRUE, status = "danger",
#                          title = "Modifier une personne",
#                          textInput("change_Email", "Email du profil a modifier",value = NULL),
#                          textInput("change_Projet_Personne", "Identifiants des Projets"),
#                          textInput("change_Nom", "Nom"),
#                          textInput("change_Prenom", "Prenom"),
#                          textInput("change_Service", "Service"),
#                          textInput("change_Ecole", "Ecole"),
#                          
#                          #passwordInput("passwd", "Password (admin ou student)"),
#                          br(),
#                          actionButton("change_Personne", "Sauvegarder")
# )


changeproject <- fluidPage(#solidHeader = TRUE, status = "danger",
                     title = "Modifier un Projet",
                     #textInput("change_Project_Title", "Titre du projet a modifier"),
                     textInput("change_Project_Type", "Type de Projet"),
                     textInput("change_Project_School", "Nom de l'Ecole"),
                     textInput("change_Project_Location", "Localisation"),
                     textInput("change_Project_Participants", "Identifiants des participants"),
                     
                     
                     #passwordInput("passwd", "Password (admin ou student)"),
                     br(),
                     actionButton("change_Project", "Sauvegarder")
)

changeevent <- fluidPage(#solidHeader = TRUE, status = "danger",
                   title = "Modifier une tache",footer = "Format de date: 2017-11-07 09:42:25",
                   textInput("change_Event_Start", "Debut de la tache"),
                   textInput("change_Event_End", "Fin de la tache"),
                   #textInput("change_Event_Nom", "Nom de la tache"),
                   textInput("change_Event_Location", "Localisation"),
                   textInput("change_Event_Participants", "Identifiants des participants"),
                   
                   
                   #passwordInput("passwd", "Password (admin ou student)"),
                   br(),
                   actionButton("change_Event", "Sauvegarder")
)