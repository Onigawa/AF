
#Login 
observeEvent(input$Login2,{
  df<-read.csv2(file = "person.csv",stringsAsFactors = FALSE)
  if((df[df$mail==input$userName2,]$password)==input$passwd2){
    USER$Logged<-TRUE
    connection$session<-df[df$mail==input$userName2,]
    connection$role<-df[df$mail==input$userName2,"role"]
    df<-read.csv2("project_person.csv",stringsAsFactors = FALSE)
    connection$projects<-df[df$person==connection$session$id,]
    removeModal()
  }
  
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
                   participantsID=input$add_Event_Participants,
                   project=project.id())
  
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
  
  
  if((""%in%line[1,])){
    warning("Empty Fields")
  }else{
    temp<-df[df$name==line$name,]
    temp$start<-line$start
    temp$end<-line$end
    temp$location<-line$location
    temp$participantsID<-line$participantsID
    df[which((df$name==line$name)&&(df$project==project.id())),]<-temp
    write.csv2(x = df,file = "calendar.csv",row.names = FALSE,quote = FALSE)
    
  }
} )

#Ajout de tache modal
observeEvent(input$box_add_event, {
  showModal(modalDialog(
    newevent,
    easyClose = TRUE
  ))
}) 

#ajout de personne modal
observeEvent(input$box_add_person, {
  showModal(modalDialog(
    newprofile,
    easyClose = TRUE
  ))
}) 


#Envoi de mail
observeEvent(input$mail_send, {
  PRJmail(to = input$mail_to,msg = input$mail_text,subject = input$mail_subject)
})
