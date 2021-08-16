library(leaflet)
library(ggmap)
library(rvest)

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
tnames <- c("Close Contacts", "Casual Contacts","Monitor for symptoms")
es <- read_html("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations")
tbls <- html_nodes(es, "table")

tbls_ls <- es %>%
  html_nodes("table") %>%
  .[1:3] %>%
  html_table(fill = TRUE)
#there are three tables


m <- leaflet() %>% addTiles()
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

write.csv(tab, paste0("./data/",tnames[i],"_",lu,".csv"),row.names = FALSE )


# Aggregate method
labs <- paste(tab$Place, tab$Date,tab$Arrival.Time, tab$Departure.Time, sep="<br/>") 



m <- m %>% addCircleMarkers(lat=tab$lat, lng=tab$lon,popup = labs, weight=0.5, color = cols[i], radius = 3 , fillOpacity = 0.8)

tabs[[i]] <- tab
}

m
names(tabs)<- tnames




