library(leaflet)
library(ggmap)
library(rvest)
library(RSelenium)
library(tidyverse)
library(DescTools)
library(knitr)
library(mapview)
library(arsenal)
require(rgdal)


addBuses = TRUE

fixgeo <- function(search,  lat, lon, column="Exposure.Location",tt=tab3) {
  
  ii <- NA
  ii <- grep(search,tt[,which(column==colnames(tab3))])
  if(length(ii>0)) {
  for (c in 1:length(ii)){
    tt[ii[c],"lat"] <-lat
    tt[ii[c],"lon"] <- lon
  }
  }
  return(tt)
}

#load google api
gapi <- readLines("c:/bernd/r/covid_canberra/gapi.txt")
register_google(gapi)

#grab from website
es <- read_html("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations", )

#check update
ll <- es %>%
  html_nodes("strong") %>%
  html_text()
index <- grep("Page last updated:",ll)
dummy <- ll[index]
lup <- dummy
lu <- substr(strsplit(dummy,"updated:")[[1]][2],2,100)
lu <- gsub(" ", "_",lu)
lu <- gsub(":","",lu)
lu
#check if there was an update....
ff <- list.files("c:/Bernd/R/covid_canberra/data/")
wu <- grep(lu, ff)





if(length(wu)==0)
{


  
  
  
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

###todo check only new sites and not the once we have data from

#load last.csv
#check identical entries column(1:8)
#add lat lon
#


#get coordinates only for those where lat lon is empty

address <- geocode(paste0( tab3$Street,", ", tab3$Exposure.Location,", ",tab3$Suburb ,", Canberra, Australia"))

tab3$lat <- address$lat
tab3$lon <- address$lon

######################################################3
##errors (manual)
tab3 <- fixgeo("Franklin Street &, Flinders Way", column = "Street"  , lat =   -35.3210247, lon =149.1341946)
tab3 <- fixgeo("Franklin Street & Flinders Way", column = "Street"  , lat =   -35.3210247, lon =149.1341946)


tab3<- fixgeo("Basketball ACT", lat=-35.24185, lon=149.057)

tab3<- fixgeo("Flatheads Takeaway", lat=-35.264, lon=149.122)


tab3<- fixgeo("Flatheads Takeaway", lat=-35.264, lon=149.122)

tab3 <- fixgeo("Hawker Drive In Bottle Shop", lat =   -35.2426147, lon =149.0449504)

tab3 <- fixgeo("Westfield Belconnen Food Court", lat =   -35.23793, lon =149.0653)

tab3 <- fixgeo("U14 girls AFL Ainslie Red", lat =   -35.2536251, lon =149.0800225)

tab3 <- fixgeo("Golden Touch Kedmar", lat =   -35.1848509, lon =149.1331888)
tab3 <- fixgeo("Golden Touch Kedmar", lat =   -35.1848509, lon =149.1331888)

tab3 <- fixgeo("Coombs to City", column = "Street"  , lat =   -35.2933, lon =149.1269703)
tab3 <- fixgeo("Coombs to Woden", column = "Street"  , lat =   -35.3444429, lon =149.0872442)

######################################################


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

cols <- c( "yellow", "red","cyan", "blue")

labs <- paste(tab3$Contact, tab3$Status,tab3$Exposure.Location, tab3$Street, tab3$Suburb, tab3$Date,tab3$Arrival.Time, tab3$Departure.Time, tab3$doubles, sep="<br/>") 
cc <- as.numeric(factor(tab3$Contact))







###############################################
##plot the map
m <- leaflet() %>% addTiles()

####################################################
### add bus lines mentioned


if (addBuses) {
  #read from shape file
  busses <- readOGR(dsn = "c:/bernd/r/covid_canberra/bus", layer = "geo_export_69c76e06-1d3f-4619-be3b-b4e5789be8ca")
  
  #search all bus lines that are mentioned
  
  bindex <- grep("Bus Route", tab3$Exposure.Location)
  buslanes <- tab3$Exposure.Location[bindex]
  
  busnumbers <- gsub("Bus Route ([0-9,A-Z]+) Transport.*","\\1", buslanes)
  blineindex <- which(busses$short_name %in% busnumbers)
  blabs <- paste(paste0("Bus route: ", busses$short_name[blineindex]),"<strong> For bus stops and ","exposure times, please"," search the table." , sep="<br/>")
  bb <- (busses[blineindex,])
  coo <- coordinates(bb)
  bcols <- colorRampPalette(c("purple", "green"))( length(coo))
  
  for (ib in 1:length(coo))
  {
    cood <- data.frame(coo[[ib]])
    m <- m %>% addPolylines(lng=cood[,1], lat=cood[,2], color = bcols[ib], weight   = 3, opacity = 0.4, popup  = blabs[ib])
  }
  
}


m <- m %>% addCircleMarkers(lat=tab3$lat, lng=tab3$lon,popup = labs, weight=0.5, color = cols[cc], radius = 5 , fillOpacity = 0.8)
m <- m %>% addLegend("bottomright", labels = levels(factor(tab3$Contact)), colors = cols, opacity = 1)



m

###############################################

 range(tab3$lat) 
 range(tab3$lon) 
####################################################

 
 
 
 #once fixed save the table again and push to github
write.csv( tab3,"c:/bernd/r/covid_canberra/data/last.csv",row.names = FALSE)
write.csv(tab3, paste0("c:/bernd/r/covid_canberra/data/table_",lu,".csv"),row.names = FALSE )
writeLines(lup, "c:/Bernd/R/covid_canberra/lastupdated.csv")

l1 <- paste("Updated tab3 and last.csv. Current data is from:", lu,"\nYou should have received a notification email now.\n")
l2 <- as.character(Sys.time())
writeLines(c(l1,l2),"c:/bernd/r/covid_canberra/lastrun.txt")


####################################################
}





if(length(wu)>0) {
  
  l1 <- paste("No new update available. Current data is from:", lu,"\n")
  l2 <- as.character(Sys.time())
  writeLines(c(l1,l2),"c:/bernd/r/covid_canberra/lastrun.txt")
  } else {
  
  cat("Data have been updated.\nNew data is from:", lup,"\n")
  
  #latest files
  flast <- list.files("c:/Bernd/R/covid_canberra/data/", pattern="table_")
  t.name<- flast[order(file.mtime(file.path("data",flast)), decreasing = TRUE)[2]]
  ldata <- read.csv(file.path("c:/bernd/r/covid_canberra/data","last.csv"))
  l2data <- read.csv(file.path("c:/bernd/r/covid_canberra/data",t.name)) 
  

  comp <- comparedf(ldata, l2data)
  scomp <- summary(comp)
 
  obsy <- scomp$obs.table[scomp$obs.table$version=="y",]
  obsx <- scomp$obs.table[scomp$obs.table$version=="x",]
  nm <- leaflet() %>% addTiles()
  if (nrow(obsx)>0) {
    cat("New added location:\n")
    cat(kable(ldata[obsx$observation,c(1:10)]))
    newobs <- obsx$observation
    labs <- paste(ldata$Contact, ldata$Status,ldata$Exposure.Location, ldata$Street, ldata$Suburb, ldata$Date,ldata$Arrival.Time, ldata$Departure.Time, ldata$doubles, sep="<br/>") 
    
    
    nm <- nm %>% addCircleMarkers(lat=ldata$lat[newobs], lng=ldata$lon[newobs],popup = labs[newobs], weight=0.5, color = "purple", radius = 5 , fillOpacity = 1)
    nm
    }
  if (nrow(obsy)>0) {
    cat("Removed locations:\n")
    cat(kable(l2data[obsy$observation,c(1:10)]))
  }
  
  scomp$comparison.summary.table  
  scomp$diffs.table
  
  #### send email to check
  
  
  
############################################################
  
  body <- paste0("New update is from: ", lup,"\n Please be aware data have not been curated yet and locations are assigned via a computer script.\n Therefore locations might be in the wrong place. \nPlease report locations that need to be corrected to: maybe a wiki page???\n Covid resources: 
                 \nACT health pages (official): https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations
                 \nACT health map: https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations/map
                 \nThis map: https://green-striped-gecko.github.io/covid_canberra/
                 \nCovid near me map: https://covid19nearme.com.au/state/act
                 
                 ")
  attach <- kable(list(scomp$comparison.summary.table, scomp$diffs.byvar.table))
  dlat <- paste0("range of lats:",paste0(range(ldata$lat), collapse = " to "))
  dlon <- paste0("range of lons:",paste0(range(ldata$lon), collapse = " to "))
  attach <- c(attach, dlat, dlon)
  writeLines(attach,"c:/Bernd/R/covid_canberra/comparison/attach.txt")
#mapshot by script does not work  
  #mapshot(nm, file = "c:/Bernd/R/covid_canberra/comparison/newsites.png")
  tolist <-  c("bernd.gruber@canberra.edu.au")
  #tolist <- c("bernd.gruber@canberra.edu.au", "Luis.MijangosAraujo@canberra.edu.au", "Anthony.Davidson@canberra.edu.au")
  
  SendOutlookMail(to = paste(tolist,sep="", collapse="; "), 
                  subject = paste0("Bernd new Covid Exposure sites have been added.Update needed\n ", lup), 
                  body = body, attachment = c("c:/bernd/r/covid_canberra/comparison/attach.txt"))
  
  l1 <- paste("Updated tab3 and last.csv. Current data is from:", lu,"\nSend an email. Check the coordinates!!!!!!.\n")
  l2 <- as.character(Sys.time())
  writeLines(c(l1,l2),"c:/bernd/r/covid_canberra/lastrun.txt")
  
  
  
}
  
 

