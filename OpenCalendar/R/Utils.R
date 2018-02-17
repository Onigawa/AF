library(plotly)

#'@export
read_save_file<-function(type="event"){

  switch(type,
         "event"=path<-paste(system.file("save",package = "OpenCalendar"),"calendar.csv",sep = "/"),
         "person"=path<-paste(system.file("save",package = "OpenCalendar"),"persons.csv",sep = "/"),
         warning("Type not supported")


         )


  dat<-read.table(file = path,header = TRUE,stringsAsFactors = FALSE,sep = ";")[,-1]
  return(dat)
}

#'@include Event.R
#'@export
#'@param con the rodbc connection to use
save_event<-function(event,path=paste(system.file("save",package = "OpenCalendar"),"calendar.csv",sep = "/"),con=FALSE){
  if(file.exists(path))
  {
    dat<-tolist(event)
    write.table(dat,file = path,append = TRUE,sep = ";",col.names = FALSE)
  }else{
    create_save_file()
    dat<-as.data.frame(tolist(event))
    write.table(dat,file = path,append = TRUE,sep = ";",col.names = FALSE)
  }

  if(con!=FALSE){
#TODO upload to database
    sqlSave(channel=con,dat = dat,tablename="name",rowname=FALSE,append=TRUE)
  }
}

create_save_file<-function(type="event"){

  switch(type,
        "event"=    {
          path=paste(system.file("save",package = "OpenCalendar"),"calendar.csv",sep = "/")
          write("\"line\";\"id\";\"start\";\"end\";\"name\";\"location\";\"participantsID\"" ,file=path)
                    },

        "person"= {
          path=paste(system.file("save",package = "OpenCalendar"),"persons.csv",sep = "/")
          write("\"line\";\"id\";\"firstname\";\"lastname\";\"mail\";\"phone\";\"profile_type\";\"sector\";\"job\";\"calendar_export\";\"school\";\"promotion\"" ,file=path)
        },
        warning("This save file cannot be created")

  )

}

#'@include Person.R
#'@export
#'@param con the rodbc connection to use
save_person<-function(person,path=paste(system.file("save",package = "OpenCalendar"),"persons.csv",sep = "/"),con=FALSE){
  if(file.exists(path))
  {
    dat<-tolist(person)
    write.table(dat,file = path,append = TRUE,sep = ";",col.names = FALSE)
  }else{
    create_save_file(type = "person")
    dat<-as.data.frame(tolist(person))
    write.table(dat,file = path,append = TRUE,sep = ";",col.names = FALSE)
  }
  if(con!=FALSE){

    sqlSave(channel=con,dat = dat,tablename="name",rowname=FALSE,append=TRUE)
  }

}



#' Function to call when creating an event
#' @param start start date and hour of the event (Uses the POSIXct format)
#' @param end end date and hour of the event (Uses the POSIXct format)
#' @param name name of the event (String)
#' @param location name of the location (For now a string in the future a GPS)
createEvent<-function(start,end,name="Untitled",location="",participants_id){
  return(new("Event",start=start,end=end,name=name,location=location,participants_id=participants_id))

}

#'@export
createPerson<-function(...){
  return(new("Person",...))

}

#'Create an event based on a list input of form : start,end,name,location,participants_id(string)
#'@details Please use only in dev function
list_to_event<-function(list){
  createEvent(list$start,list$end,list$name,list$location,strsplit(list$participantsID,split = ", ")[[1]])
}



#'Function used to display a gantt calendar of the tasks
#'@params calendar the dataframe to be displayed
#'@details Still cut the last task at hand
#'@export plotly
#'@export
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
#'@export
DBconnect<-function(){
return(odbcConnect(dsn,uid="",pwd=""))
}
#'@export
load_table<-function(channel,table){
return(sqlFetch(channel = channel,sqtable = table))

}
