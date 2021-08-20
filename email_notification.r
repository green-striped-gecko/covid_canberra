#Send email updates (only after update and only manually after check)
lup <- readLines("lastupdated.csv")

cat("Data have been updated.\nNew data is from:", lup,"\n")

#latest files
flast <- list.files("./data/", pattern="table_")
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
  newobs <- obsx$observation
  labs <- paste(ldata$Contact, ldata$Status,ldata$Exposure.Location, ldata$Street, ldata$Suburb, ldata$Date,ldata$Arrival.Time, ldata$Departure.Time, ldata$doubles, sep="<br/>") 
  
  
  nm <- nm %>% addCircleMarkers(lat=ldata$lat[newobs], lng=ldata$lon[newobs],popup = labs[newobs], weight=0.5, color = "purple", radius = 5 , fillOpacity = 1)
  nm
}
if (nrow(obsy)>0) {
  cat("Removed locations:\n")
  l2data[obsy$observation,c(1:10)]
}

scomp$comparison.summary.table  
scomp$diffs.table

#### send email to check



######################################################################

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
writeLines(attach,"c:/Bernd/R/covid_canberra/comparison/attach_email.txt")

mapshot(nm, file = "c:/Bernd/R/covid_canberra/comparison/newsites_email.png")
tolist <-  c("bernd.gruber@canberra.edu.au")
#tolist <- c("bernd.gruber@canberra.edu.au", "Luis.MijangosAraujo@canberra.edu.au", "Anthony.Davidson@canberra.edu.au")

SendOutlookMail(to = paste(tolist,sep="", collapse="; "), 
                subject = paste0("New Covid Exposure sites have been added.\n ", lup), 
                body = body, attachment = c("c:/bernd/r/covid_canberra/comparison/attach_email.txt", "c:/bernd/r/covid_canberra/comparison/newsites_email.png"))

