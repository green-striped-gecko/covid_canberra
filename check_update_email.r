
#switch off
#stop("Switched off for testing")

#monitor updates
library(rvest)
library(DescTools)
library(knitr)
library(mapview)
last <- readLines("c:/Bernd/R/covid_canberra/lastupdated.csv")

#grab from website
es <- read_html("https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations")

#check update
ll <- es %>%
  html_nodes("strong") %>%
  html_text()
index <- grep("Page last updated:",ll)
current <- ll[index]


if (last!=current)
{
  #scomp <- NULL
 
  source("c:/Bernd/R/covid_canberra/gecode_tables.R")
  
  
######################################################################
  
  body <- paste0("New update is from: ", current,"\n Please be aware data have not been curated yet and locations are assigned via a computer script.\n Therefore locations might be in the wrong place. \nPlease report locations that need to be corrected to: maybe a wiki page???\n Covid resources: 
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
  
  mapshot(nm, file = "c:/Bernd/R/covid_canberra/comparison/newsites.png")
  tolist <-  c("bernd.gruber@canberra.edu.au")
  #tolist <- c("bernd.gruber@canberra.edu.au", "Luis.MijangosAraujo@canberra.edu.au", "Anthony.Davidson@canberra.edu.au")
  
  SendOutlookMail(to = paste(tolist,sep="", collapse="; "), 
                  subject = paste0("New Covid Exposure sites have been added.\n ", lup), 
                  body = body, attachment = c("c:/bernd/r/covid_canberra/comparison/attach.txt", "c:/bernd/r/covid_canberra/comparison/newsites.png"))
  
############################################################################
  writeLines(current, "c:/Bernd/R/covid_canberra/lastupdated.csv")

  }


