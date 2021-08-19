library(leaflet)
library(ggmap)
library(rvest)
library(RSelenium)
library(tidyverse)

fixgeo <- function(search,  lat, lon, tt=tab3) {
  
  ii <- NA
  ii <- grep(search,tt$Exposure.Location)
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
ll <- signals %>%
  html_nodes("strong") %>%
  html_text()
index <- grep("Page last updated:",ll)
dummy <- ll[index]
lup <- dummy
lu <- substr(strsplit(dummy,"updated:")[[1]][2],2,100)
lu <- gsub(" ", "_",lu)
lu <- gsub(":","",lu)
lu

##### scrape covid exposure table from website

rD <- rsDriver(browser="firefox", port=4545L, verbose=FALSE)
remDr <- rD[["client"]]
remDr$navigate("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations")

Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

remDr$close()
rD$server$stop()
rm(rD)
gc()
#necessary to stop the server...
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
signals <- read_html(html)


tbls <- signals %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

tab3 <- data.frame(tbls)




#change empty to previous
tab3$Status <- ifelse(tab3$Status=="New","New","")
#tab3$type <- paste(tab3$Contact, tab3$Status)
#check if there was an update....
ff <- list.files("./data/")
wu <- grep(lu, ff)




if(length(wu)==0)
{

cols <- c("red", "yellow", "blue")

#get coordinates
address <- geocode(paste0( tab3$Street,", ", tab3$Exposure.Location,", ",tab3$Suburb ,", Canberra, Australia"))

tab3$lat <- address$lat
tab3$lon <- address$lon

######################################################3
##errors (manual)

tab3<- fixgeo("Coles Supermarket Manuka", lat=-35.32102, lon=149.1342)
tab3<- fixgeo("Coles Supermarket  Manuka", lat=-35.32102, lon=149.1342)

tab3<- fixgeo("Basketball ACT", lat=-35.24185, lon=149.057)

tab3<- fixgeo("Flatheads Takeaway", lat=-35.264, lon=149.122)


tab3<- fixgeo("Flatheads Takeaway", lat=-35.264, lon=149.122)

tab3 <- fixgeo("Hawker Drive In Bottle Shop", lat =   -35.2426147, lon =149.0449504)

tab3 <- fixgeo("Westfield Belconnen Food Court", lat =   -35.23793, lon =149.0653)

tab3 <- fixgeo("U14 girls AFL Ainslie Red", lat =   -35.2536251, lon =149.0800225)

tab3 <- fixgeo("Golden touch Kedmar", lat =   -35.1848509, lon =149.1331888)
tab3 <- fixgeo("Golden touch Kedmar", lat =   -35.1848509, lon =149.1331888)
######################################################

#latest files
flast <- list.files("./data/", pattern="table_")
t.name<- flast[order(file.mtime(file.path("data",flast)), decreasing = TRUE)[1]]
ltab <- read.csv(file.path("data",t.name)) 
if (identical(ltab[,1:6], tab3[,1:6])) cat("Casual table [table #1] has not changed \n")

lalo <- paste(tab3$lat, tab3$lon)
tt <- table(lalo)
doubles <- names(tt)[tt>1]
index <- which(lalo %in% doubles)

tab3$doubles <- ""
tab3$doubles[index]<- "<strong/>!Location has more than<br> one entry. Zoom in and search table!</strong/>"



index <- which(tab3$doubles!="")
tab3$moved <-FALSE

for ( i in 1:length(index))
{
  
  dbs <- which((lalo %in% lalo[index[i]]))
  if (tab3$moved[dbs[1]]==FALSE) {
    
    mm <- seq(0, (length(dbs)-1)*0.00005,0.00005 )
    mm <- mm-mean(mm)
    tab3$lon[dbs] <- tab3$lon[dbs] -mm
    
    tab3$moved[dbs]<- TRUE
  }
}


labs <- paste(tab3$Contact, tab3$Status,tab3$Exposure.Location, tab3$Street, tab3$Suburb, tab3$Date,tab3$Arrival.Time, tab3$Departure.Time, tab3$doubles, sep="<br/>") 



cc <- as.numeric(factor(tab3$Contact))

###############################################
##plot the map

m <- leaflet() %>% addTiles()

m <- m %>% addCircleMarkers(lat=tab3$lat, lng=tab3$lon,popup = labs, weight=0.5, color = cols[cc], radius = 5 , fillOpacity = 0.8) %>% addCircleMarkers(lat=tab3$lat, lng=tab3$lon,popup = labs, weight=0.5, color = cols[cc], radius = 5 , fillOpacity = 0.8
   #                                                                                                                                                       ,clusterOptions =  
   #                             markerClusterOptions(spiderfyOnMaxZoom = FALSE, zoomToBoundsOnClick = FALSE,showCoverageOnHover = FALSE, iconCreateFunction=JS("function (cluster) {    
   # var childCount = cluster.getChildCount();  
   #   if (childCount < 100) {  
   #     c = 'rgba(64, 64, 64, 0.3);'
   #   } else if (childCount < 1000) {  
   #     c = 'rgba(64, 64, 64, 0.3);'  
   #   } else { 
   #     c = 'rgba(64, 64, 64, 0.3);'  
   #   }    
   #   return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(5, 5) });
   # 
   # }"))
)
m 
###############################################

 range(tab3$lat) 
 range(tab3$lon) 

 

####################################################
#once fixed save the table again and push to github
write.csv( tab3,"./data/last.csv",row.names = FALSE)
write.csv(tab3, paste0("./data/table_",lu,".csv"),row.names = FALSE )

rmarkdown::render("Covid_Exposure_ACT.rmd", output_dir = "docs", params=list(lup=lup), output_file = "index.html")
####################################################

}


if(length(wu)>0) cat("No new update available. Current data is from:", lu,"\n") 
