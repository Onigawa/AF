library(RODBC)

#ToListFile
#'@include Person.R
#'@include Student.R
#'@include Intervenant.R

setGeneric("tolist", function(obj) {
  return(standardGeneric("tolist"))
})

setMethod("tolist",
          signature = "Person",
          function(obj){


            return(list(id=obj@person_id,firstname=obj@firstname,lastname=obj@lastname,mail=obj@mail,
                        phone=obj@phone,profile_type=obj@profile_type,project_list=obj@project_list,
                        sector="",job="",calendar_export="",school="",promotion=""))
          }
)

setMethod("tolist",
          signature = "Student",
          function(obj){

            return(list(id=obj@person_id,firstname=obj@firstname,lastname=obj@lastname,mail=obj@mail,
                        phone=obj@phone,profile_type=obj@profile_type,
                        sector="",job="",calendar_export=obj@calendar_export,school=obj@school,promotion=obj@promotion))
          }
)

setMethod("tolist",
          signature = "Intervenant",
          function(obj){

            return(list(id=obj@person_id,firstname=obj@firstname,lastname=obj@lastname,mail=obj@mail,
                        phone=obj@phone,profile_type=obj@profile_type,
                        sector=obj@sector,job=obj@job,calendar_export=obj@calendar_export,school="",promotion=""))
          }
)

setMethod("tolist",
          "Event",
          function(obj){
            return(list(id=obj@event_id,start=obj@start,end=obj@end,name=obj@name,
                        location=obj@location,participantsID=get_participants_id_as_string(obj)))
          }
)
