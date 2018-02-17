# login <- box(
#   title = "Authentification",
#   textInput("userName", "Email (admin@airfrance.fr OU student@ecole.fr)"),
#   passwordInput("passwd", "Password (admin ou student)"),
#   br(),
#   actionButton("Login", "Se connecter")
# )

login2 <- box(
  title = "Authentification",
  textInput("userName2", "Email (admin@airfrance.fr OU student@ecole.fr)",value = "student@ecole.fr"),
  passwordInput("passwd2", "Password (admin ou student)",value = "student"),
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