#monitor updates
library(rvest)
library(DescTools)
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
 
  SendOutlookMail(to = c("bernd.gruber@canberra.edu.au"), 
                  subject = "The has been a covid update.", 
                  body = paste0("New update is:\n", current))
  
  SendOutlookMail(to = c("luis.mijangosaraujo@canberra.edu.au"), 
                  subject = "The has been a covid update.", 
                  body = paste0("New update is:\n", current))
  writeLines(current, "c:/Bernd/R/covid_canberra/lastupdated.csv")

  }

