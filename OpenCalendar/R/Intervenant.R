
#'@include Person.R
setClass("Intervenant",contains = "Person",
         slots =c("sector","job","calendar_export"),

         validity = function(object) {

         }
)

setMethod("initialize",
          "Intervenant",
          function(.Object, firstname="John", lastname="Doe") {

            print("initialize from Intervenant called")
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
            .Object@profile_type <- "Intervenant"
            .Object@sector<-"Unknown"
            .Object@job<-"Unknown"
            .Object@calendar_export<-"Unknown"
            validObject(.Object) ## valide l'objet
            return(.Object)
          }

)


