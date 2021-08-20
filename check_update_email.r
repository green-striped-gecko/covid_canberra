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

  body <- paste0("New update is: ", current,"\n")
  attach <- kable(list(scomp$comparison.summary.table, scomp$diffs.byvar.table))
   dlat <- paste0("range of lats:",paste0(range(ldata$lat), collapse = " to "))
   dlon <- paste0("range of lons:",paste0(range(ldata$lon), collapse = " to "))
   attach <- c(attach, dlat, dlon)
  writeLines(attach,"c:/Bernd/R/covid_canberra/comparison/attach.txt")
  
  mapshot(nm, file = "c:/Bernd/R/covid_canberra/comparison/newsites.png")
 
  SendOutlookMail(to = c("bernd.gruber@canberra.edu.au"), 
                  subject = paste0("There has been a covid update. ", lup), 
                  body = body, attachment = c("c:/bernd/r/covid_canberra/comparison/attach.txt", "c:/bernd/r/covid_canberra/comparison/newsites.png"))
  

  writeLines(current, "c:/Bernd/R/covid_canberra/lastupdated.csv")

  }


