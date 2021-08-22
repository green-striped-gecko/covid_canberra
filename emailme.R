###inform me of an update

library(rvest)

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
  
  l1 <- paste("New update available. Current data is from:", lu,"\nTrying to run geocode_tables.R now\n")
  l2 <- as.character(Sys.time())
  writeLines(c(l1,l2),"c:/bernd/r/covid_canberra/lastrun.txt")  
  
  SendOutlookMail(to = "bernd.gruber@canberra.edu.au", 
                  subject = paste0("New updated. Run Geocode.R now\n ", lup), 
                  body ="ACT health map: https://www.covid19.act.gov.au/act-status-and-response/act-covid-19-exposure-locations
\nThis map: https://green-striped-gecko.github.io/covid_canberra/")
  
}  