


#'@include Person.R
setClass("Student",contains = "Person",
         slots =c("school","promotion","calendar_export"),

         validity = function(object) {
           #TODO validity of student
         }
)


setMethod("initialize",
          "Student",
          function(.Object, firstname="John", lastname="Doe") {


            if(file.exists(paste(system.file("save",package = "OpenCalendar"),"persons.csv",sep = "/")))
            {
              .Object@person_id<- max(read_save_file("person")[,"id"])+1
            }else{
              .Object@person_id<-0
            }
            .Object@firstname <- paste("Mr", firstname, sep = " ")
            .Object@lastname <- lastname
            .Object@mail<-"Unknown"
            .Object@phone<-"Unknown"
            .Object@profile_type <- "Student"
            .Object@school<-"Unknown"
            .Object@promotion<-"Unknown"
            .Object@calendar_export<-"Unknown"
            validObject(.Object) ## valide l'objet
            return(.Object)
          })


