
printGantt<-function(calendar){
  
  # Convert to dates
  calendar$start<-as.Date(calendar$start,format = "%Y-%m-%d")
  calendar$end<-as.Date(calendar$end,format = "%Y-%m-%d")
  
  # Initialize empty plot
  p <- plot_ly()
  
  # Each task is a separate trace
  # Each trace is essentially a thick line plot
  # x-axis ticks are dates and handled automatically
  
  for(i in 1:(nrow(calendar) )){
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

PRJmail<-function(to,subject,msg){
  if(is.na(to)||is.null(subject)||is.null(msg)){
    warning("Empty Fields")
  }else{
    sender <- "postmasterprj@gmail.com"
    send.mail(from = sender,
              to = to,
              subject = subject,
              body = msg,
              smtp = list(host.name = "smtp.gmail.com", port = 465, 
                          user.name = "postmasterprj@gmail.com",            
                          passwd = "P.R.J.123", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)}
}

randString <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

stage_rss<-function(link="http://recrutement.airfrance.com/handlers/offerRss.ashx?LCID=1036&Rss_Contract=3442"){
  # doc<-xmlParse(link)
  # 
  # src<-xpathApply(xmlRoot(doc), "//item")
  # 
  # for (i in 1:length(src)) {
  #   if (i==1) {
  #     foo<-xmlSApply(src[[i]], xmlValue)
  #     DATA<-data.frame(t(foo), stringsAsFactors=FALSE)
  #   }
  #   else {
  #     foo<-xmlSApply(src[[i]], xmlValue)
  #     tmp<-data.frame(t(foo), stringsAsFactors=FALSE)
  #     DATA<-rbind(DATA, tmp)
  #   }
  #   
  # }
  # DATA[,1]<-sprintf(paste('<a href="',DATA[,1],'" target="_blank" class="btn btn-primary">Lien</a>'))
  # colnames(DATA)<-c("Lien","Categorie","Cat1","Localisation","Intitule","Description","Date de Publication")
  # return(DATA)
}

