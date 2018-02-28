
#Login 
observeEvent(input$Login2,{
  df<-read.csv2(file = "person.csv",stringsAsFactors = FALSE)
  pass<-(df[df$mail==input$userName2,]$password)
  if(length(pass)<1) pass<-"NULL"
  if(pass==input$passwd2){
    USER$Logged<-TRUE
    connection$session<-df[df$mail==input$userName2,]
    connection$role<-df[df$mail==input$userName2,"role"]
    df<-read.csv2("project_person.csv",stringsAsFactors = FALSE)
    connection$projects<-df[df$person==connection$session$id,]

    removeModal()
    

  }
  
})
observeEvent(eventExpr = input$preprojectperson_push,{
  df<-read.csv2("projects.csv",stringsAsFactors = FALSE)
  idProject<-df[df$title==input$preproject,"id"]
  df<-read.csv2("person.csv",stringsAsFactors = FALSE)
  idPerson<-df[df$mail==input$preperson,"id"]
  df2<-data.frame(project=idProject,person=idPerson)
  df<-read.csv2(file = "project_person.csv",stringsAsFactors = FALSE)
  df<-rbind(df,df2)
  df<-unique(df)
  write.csv2(x=df,file = "project_person.csv",row.names = FALSE)
  showModal(modalDialog("Project Added",easyClose = TRUE,footer = NULL))
  
})

#Ajout de personnes
observeEvent(eventExpr = input$add_Personne,handlerExpr =  {
  line<-data.frame(id=max(read.csv2("person.csv")$id)+1,
                   name=input$add_Nom,
                   surname=input$add_Prenom,
                   mail=input$add_Email,
                   service=input$add_Service,
                   school=input$add_Ecole,
                   password="123",
                   role=input$add_role)
  
  if(!(""%in%line[1,])){
    
    write.table(x = line,file = "person.csv",append = TRUE,sep = ";",col.names = FALSE,quote = TRUE,row.names = FALSE)
    removeModal()
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
                   participantsID=input$add_Event_Participants,
                   project=project.id())
  
  if(!(""%in%line[1,])){
    
    write.table(x = line,file = "calendar.csv",append = TRUE,sep = ";",col.names = FALSE,quote = TRUE,row.names = FALSE)
    removeModal()
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
                   description=input$add_Project_Description,
                   )
  
  if(!(""%in%line[1,])){
    
    write.table(x = line,file = "projects.csv",append = TRUE,sep = ";",col.names = FALSE,quote = TRUE,row.names = FALSE)
    removeModal()
  }else{
    warning("Empty Fields")
  }
} )

#Modif person
observeEvent(eventExpr = input$change_Personne,handlerExpr =  {
  
  if(connection$role=="Adminstrateur"){
    line<-data.frame(id=max(read.csv2("person.csv")$id)+1,
                     name=input$change_Nom,
                     surname=input$change_Prenom,
                     mail=input$change_Email,
                     service=input$change_Service,
                     school=input$change_Ecole,
                     password="123",stringsAsFactors = FALSE)
  }else{
    line<-data.frame(id=max(read.csv2("person.csv")$id)+1,
                     name=input$change_Nom,
                     surname=input$change_Prenom,
                     mail=connection$session$mail,
                     service=input$change_Service,
                     school=input$change_Ecole,
                     password="123",stringsAsFactors = FALSE)
  }

  
  df<-read.csv2("person.csv",stringsAsFactors = FALSE)
  
  
  
  
  if(!(""%in%line[1,])){
    
    temp<-df[df$mail==line$mail,]
    temp$name<-line$name
    temp$surname<-line$surname
    temp$service<-line$service
    temp$school<-line$school
    df[which(df$mail==connection$session$mail),]<-temp
    write.csv2(x = df,file = "person.csv",row.names = FALSE,quote = TRUE)
    removeModal()
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
                   description=input$change_Project_Description,
                   stringsAsFactors = FALSE)
  df<-read.csv2("projects.csv",stringsAsFactors = FALSE)
  
  if(!(""%in%line[1,])){
    temp<-df[df$id==project.id(),]
    temp$type<-line$type
    temp$title<-line$title
    temp$school<-line$school
    temp$location<-line$location
    temp$description<-line$description
    df[which(df$id==project.id()),]<-temp
    write.csv2(x = df,file = "projects.csv",row.names = FALSE,quote = TRUE)
    removeModal()
  }else{
    warning("Empty Fields")
  }
} )

#Modif taches
observeEvent(eventExpr = input$change_Event,handlerExpr =  {
  line<-data.frame(id=max(read.csv2("calendar.csv")$id)+1,
                   start=input$change_Event_Start,
                   end=input$change_Event_End,
                   name=input$preevent_name,
                   location=input$change_Event_Location,
                   participantsID=input$change_Event_Participants,stringsAsFactors = FALSE)
  df<-read.csv2("calendar.csv",stringsAsFactors = FALSE)
  
  
  if((""%in%line[1,])){
    warning("Empty Fields")
  }else{
    temp<-df[which((df$name==line$name)&(df$project==project.id())),]
    temp$start<-line$start
    temp$end<-line$end
    temp$location<-line$location
    temp$participantsID<-line$participantsID
    df[which((df$name==line$name)&(df$project==project.id())),]<-temp
    
    write.csv2(x = df,file = "calendar.csv",row.names = FALSE,quote = TRUE)
    removeModal()
  }
} )

#Ajout de tache modal
observeEvent(input$box_add_event, {
  showModal(modalDialog(
    newevent,
    easyClose = TRUE
  ))
}) 

#Modif person modal
observeEvent(input$preprofile_push,
             {showModal(modalDialog(
               postprofile,
               easyClose = TRUE
             ))
               
            df<-read.csv2("person.csv",stringsAsFactors = FALSE)
             df<-df[which(df$mail==input$preprofile_email),]
             updateTextInput(session,"change_Nom_post",value = df$name)
             updateTextInput(session,"change_Prenom_post",value = df$surname)
             updateTextInput(session,"change_Service_post",value = df$service)
             updateTextInput(session,"change_Ecole_post",value = df$school)
             updateTextInput(session,"change_Role_post", value =df$role)
             
             }
             )

observeEvent(input$preevent_push,
             {showModal(modalDialog(
               changeevent,
               easyClose = TRUE
             ))
               df<-read.csv2("calendar.csv",stringsAsFactors = FALSE)
               df<-df[which((df$name==input$preevent_name)&(df$project==project.id())),]
               updateTextInput(session,"change_Event_Start", value=df$start)
               updateTextInput(session,"change_Event_End", value=df$end)
               updateTextInput(session,"change_Event_Location",value= df$location)
               updateTextInput(session,"change_Event_Participants",value= df$participantsID)
               }
)
observeEvent(input$preproject_push,
             {showModal(modalDialog(
               changeproject,
               easyClose = TRUE
             ))
               df<-read.csv2("projects.csv",stringsAsFactors = FALSE)
               df<-df[which(df$id==project.id()),]
               updateTextInput(session,"change_Project_Title", value=df$title)
               updateTextInput(session,"change_Project_Type", value=df$type)
               updateTextInput(session,"change_Project_School", value=df$school)
               updateTextInput(session,"change_Project_Location",value= df$location)
               updateTextAreaInput(session,"change_Project_Description",value=df$description)
               }
)

observeEvent(eventExpr = input$change_Personne_post,handlerExpr =  {
  
  line<-data.frame(id=max(read.csv2("person.csv")$id)+1,
                     name=input$change_Nom_post,
                     surname=input$change_Prenom_post,
                     mail=input$preprofile_email,
                     service=input$change_Service_post,
                     school=input$change_Ecole_post,
                     password="123",
                     role=input$change_Role_post,
                     stringsAsFactors = FALSE)
  
  df<-read.csv2("person.csv",stringsAsFactors = FALSE)

  
  if(!(""%in%line[1,])){
    
    temp<-df[df$mail==line$mail,]
    temp$name<-line$name
    temp$surname<-line$surname
    temp$service<-line$service
    temp$school<-line$school
    
    df[which(df$mail==input$preprofile_email),]<-temp
    write.csv2(x = df,file = "person.csv",row.names = FALSE,quote = TRUE)
    removeModal()
  }else{
    warning("Empty Fields")
  }
} )

#ajout de personne modal
observeEvent(input$box_add_person, {
  showModal(modalDialog(
    newprofile,
    easyClose = TRUE
  ))
}) 


#Envoi de mail
observeEvent(input$mail_send, {
  if("" %in% c(input$mail_to,input$mail_text,input$mail_subject)){
    
  }else{
    showModal(modalDialog(
      "Votre Email est en cours d'envoi",
      size="s",
      footer=NULL,
      easyClose = TRUE
    ))
    PRJmail(to = input$mail_to,msg = input$mail_text,subject = input$mail_subject)

  }
  
})
