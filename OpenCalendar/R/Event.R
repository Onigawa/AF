#Class event
library(methods)
library(lubridate)
#TODO POSIXct to datetime Sql

#TODO event_id

#' A class used to describe an event

setClass("Event",slots = c("event_id","start","end","name","location","participants_id","duration"),


       #  prototype(start = as.POSIXct.Date(Sys.Date()),
        #           end = as.POSIXct.Date(Sys.Date())
         #          ),
         validity = function(object) { ## object : nom reserve !
           if ( (!is.Date(object@start)) || (!is.Date(object@end)))
             ## start and Finish Should be POSIXct

             return(FALSE)
           else
             return(TRUE)
         })

setMethod("initialize",
          "Event",

          function(.Object, start=as.Date(as.numeric(Sys.time()), format = "%m/%d/%Y"), end=as.Date(as.numeric(Sys.time()), format = "%m/%d/%Y"),

                   name="(Untitled)",
                   location="",
                   participants_id=c("1","2"),
                   duration=1) {
            print("Initialize an Event")
            if(file.exists(paste(system.file("save",package = "OpenCalendar"),"calendar.csv",sep = "/")))
            {
              .Object@event_id<- max(read_save_file()[,"id"])+1
            }else{
              .Object@event_id<-0
            }
            .Object@start <- start
            .Object@end <- end
            .Object@name <- name
            .Object@location<- location
            .Object@participants_id<-participants_id  #participants_ids
            validObject(.Object) ## valide l'objet
            return(.Object)
          })

setGeneric(name="get_event_id",
           def=function(obj)
           {
             standardGeneric("get_event_id")
           }
)

setMethod(f="get_event_id",
          signature="Event",
          definition=function(obj)
          {
            return(obj@event_id)
          }
)

setGeneric(name="get_start",
           def=function(obj)
           {
             standardGeneric("get_start")
           }
)

setMethod(f="get_start",
          signature="Event",
          definition=function(obj)
          {
            return(obj@start)
          }
)

setGeneric(name="get_end",
           def=function(obj)
           {
             standardGeneric("get_end")
           }
)

setMethod(f="get_end",
          signature="Event",
          definition=function(obj)
          {
            return(obj@end)
          }
)

setGeneric(name="get_name",
           def=function(obj)
           {
             standardGeneric("get_name")
           }
)

setMethod(f="get_name",
          signature="Event",
          definition=function(obj)
          {
            return(obj@name)
          }
)

setGeneric(name="get_location",
           def=function(obj)
           {
             standardGeneric("get_location")
           }
)

setMethod(f="get_location",
          signature="Event",
          definition=function(obj)
          {
            return(obj@location)
          }
)

setGeneric(name="get_participants_id",
           def=function(obj)
           {
             standardGeneric("get_participants_id")
           }
)

setMethod(f="get_participants_id",
          signature="Event",
          definition=function(obj)
          {
            return(obj@participants_id)
          }
)

setGeneric(name="get_participants_id_as_string",
           def=function(obj)
           {
             standardGeneric("get_participants_id_as_string")
           }
)

setMethod(f="get_participants_id_as_string",
          signature="Event",
          definition=function(obj)
          {
            return(toString(obj@participants_id))
          }
)

setGeneric(name="set_start",
           def=function(obj)
           {
             standardGeneric("set_start")
           }
)

setMethod(f="set_start",
          signature="Event",
          definition=function(obj)
          {
            return(obj@start)
          }
)

setGeneric(name="set_end",
           def=function(obj)
           {
             standardGeneric("set_end")
           }
)

setMethod(f="set_end",
          signature="Event",
          definition=function(obj)
          {
            return(obj@end)
          }
)

setGeneric(name="set_name",
           def=function(obj)
           {
             standardGeneric("set_name")
           }
)

setMethod(f="set_name",
          signature="Event",
          definition=function(obj)
          {
            return(obj@name)
          }
)

setGeneric(name="set_location",
           def=function(obj)
           {
             standardGeneric("set_location")
           }
)

setMethod(f="set_location",
          signature="Event",
          definition=function(obj)
          {
            return(obj@location)
          }
)

setGeneric(name="set_participants_id",
           def=function(obj)
           {
             standardGeneric("set_participants_id")
           }
)

setMethod(f="set_participants_id",
          signature="Event",
          definition=function(obj)
          {
            return(obj@participants_id)
          }
)

setGeneric("tolist", function(obj) {
  return(standardGeneric("tolist"))
})







