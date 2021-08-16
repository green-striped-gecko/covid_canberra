library(leaflet)
library(ggmap)
library(rvest)

fixgeo <- function(search,  lat, lon, tt=tab3) {
  
  ii <- NA
  ii <- grep(search,tt$Place)
  for (c in 1:length(ii)){
    tt[ii[c],"lat"] <-lat
    tt[ii[c],"lon"] <- lon
  }
  return(tt)
}

#load google api
gapi <- readLines("gapi.txt")
register_google(gapi)

#grab from website
es <- read_html("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations")

#check update
ll <- es %>%
  html_nodes("p") %>%
  html_text()
index <- grep("Page last updated:",ll)
dummy <- ll[index]
lu <- substr(strsplit(dummy,"updated:")[[1]][2],2,100)
lu <- gsub(" ", "_",lu)
lu <- gsub(":","",lu)


#grab tables
tnames <- c("Close_Contacts", "Casual_Contacts","Monitor_for_symptoms")
es <- read_html("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations")
tbls <- html_nodes(es, "table")

tbls_ls <- es %>%
  html_nodes("table") %>%
  .[1:3] %>%
  html_table(fill = TRUE)
#there are three tables

tbls_ls[[2]]$State<- NULL
tbls_ls <- lapply(tbls_ls, data.frame)
tbls_ls[[1]]$table="Close Contacts"
tbls_ls[[2]]$table="Casual Contacts"
tbls_ls[[3]]$table="Monitoring"


colnames(tbls_ls[[3]])<-colnames(tbls_ls[[2]])<- colnames(tbls_ls[[1]])


tab3 <- do.call(rbind, tbls_ls)

#change empty to previous
tab3$Status <- ifelse(tab3$Status=="","Previous",tab3$Status)
#check if there was an update....
ff <- list.files("./data/")
wu <- grep(lu, ff)
if(length(wu)>0) stop("No updated since. Current data is from:", lu,"\n") 




if(length(wu)==0)
{

cols <- c("red", "yellow", "blue")

#get coordinates
address <- geocode(paste(tab3$Place,", Canberra, Australia"))

tab3$lat <- address$lat
tab3$lon <- address$lon

##errors (manual)
##oakley in casual 2x  [-35.24167	149.058]
tab3 <- fixgeo("TLE Electrical & Data supplies, 22-36 Oatley Court", lat=-35.24167, lon = 149.058)

##aranda in monitoring [-35.2536 ) 
tab3 <- fixgeo("Aranda Playing Fields", lat=-35.25363, lon=149.08)



#latest files
flast <- list.files("./data/", pattern="table_")
t.name<- flast[which(order(file.mtime(file.path("data",flast)))==3)]
ltab <- read.csv(file.path("data",t.name)) 
if (identical(ltab[,1:6], tabs[[1]][,1:6])) cat("Casual table [table #1] has not changed \n")





# Aggregate method
labs <- paste(tab3$Place, tab3$Date,tab3$Arrival.Time, tab3$Departure.Time, sep="<br/>") 
m <- leaflet() %>% addTiles()

m <- m %>% addCircleMarkers(lat=tab3$lat, lng=tab3$lon,popup = labs, weight=0.5, color = cols[as.numeric(factor(tab3$table))], radius = 5 , fillOpacity = 0.8)



#checks 
m


 range(tab3$lat) 
 range(tab3$lon) 





write.csv(tab3, paste0("./data/table_",lu,".csv"),row.names = FALSE )

}