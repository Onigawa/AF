#TODO Génération ID

#'Class describing a person in the package
#'
#'@slot person_id An identifier used to differenciate persons
setClass("Person",
         slots=c("person_id","firstname", "lastname","mail","phone","profile_type","project_list"),
         validity = function(object) { ## object : nom reserve !

           if (object@firstname == object@lastname)
             ## Person cannot have firstname equal to lastname
             return(FALSE)
           else
             return(TRUE)
         })


setMethod("initialize",
          "Person",
          function(.Object, firstname="John", lastname="Doe") {
            print("initialize from Person called")

            if(file.exists(paste(system.file("save",package = "OpenCalendar"),"persons.csv",sep = "/")))
            {
              .Object@person_id<- max(read_save_file(type = "person")[,"id"])+1
            }else{
              .Object@person_id<-0
            }
            .Object@firstname <- paste("Mr", firstname, sep = " ")
            .Object@lastname <- lastname
            .Object@mail<-"Unknown"
            .Object@phone<-"Unknown"
            .Object@profile_type <- "Unknown"
            validObject(.Object) ## valide l'objet
            return(.Object)
          }

        )

setGeneric("setFirstName<-", function(obj, value) {
  return(standardGeneric("setFirstName<-"))
})

setReplaceMethod("setFirstName", "Person", function(obj, value) {
  obj@firstname <- value
  return(obj)
})
