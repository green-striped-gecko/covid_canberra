library(leaflet)
library(ggmap)
library(rvest)

fixgeo <- function(search,tab,  lat, lon, tt=tabs) {
  
  ii <- NA
  ii <- grep(search,tt[[tab]]$Place)
  for (c in 1:length(ii)){
    tt[[tab]][ii[c],"lat"] <-lat
    tt[[tab]][ii[c],"lon"] <- lon
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

#check if there was an update....

ff <- list.files("./data/")

wu <- grep(lu, ff)

if(length(wu)>0) stop("No updated since. Current data is from:", lu,"\n") 


if (length(wu)==0)
{
unlist(lapply(tbls_ls, nrow))




tabs <- list()
cols <- c("red", "yellow", "blue")
for (i in 1:3)
{
tab <- data.frame(tbls_ls[[i]])  

#change empty to previous
tab$Status <- ifelse(tab$Status=="","Previous",tab$Status)
#get coordinates
address <- geocode(paste(tab$Place,", Canberra, Australia"))

tab$lat <- address$lat
tab$lon <- address$lon
tabs[[i]] <- tab
}
names(tabs)<- tnames
##errors (manual)
##oakley in casual 2x  [-35.24167	149.058]
tabs <- fixgeo("TLE Electrical & Data supplies, 22-36 Oatley Court",tab = 2, lat=-35.24167, lon = 149.058)

##aranda in monitoring [-35.2536 ) 
tabs <- fixgeo("Aranda Playing Fields", tab=3, lat=-35.25363, lon=149.08)


m <- leaflet() %>% addTiles()
for (i in 1:3) {
# Aggregate method
labs <- paste(tabs[[i]]$Place, tabs[[i]]$Date,tabs[[i]]$Arrival.Time, tabs[[i]]$Departure.Time, sep="<br/>") 

m <- m %>% addCircleMarkers(lat=tabs[[i]]$lat, lng=tabs[[i]]$lon,popup = labs, weight=0.5, color = cols[i], radius = 5 , fillOpacity = 0.8)

}

#checks 
m


lapply(tabs,function(x) summary(x$lat) )
lapply(tabs,function(x) summary(x$lon) )



for (i in 1:3)
{
write.csv(tabs[[i]], paste0("./data/",tnames[i],"_",lu,".csv"),row.names = FALSE )
}
}  #wu >0 = new data is there....