
library(rvest)
library(RSelenium)
#update website (once geocode_tables showed an update to create index.html and then commit)
lup <- readLines("lastupdated.csv")

rmarkdown::render("c:/bernd/r/covid_canberra/Covid_Exposure_ACT.rmd", output_dir = "docs", params=list(lup=lup), output_file = "index.html")


#check using Rselenium

rD <- rsDriver(browser="firefox", port=4545L, verbose=FALSE)
remDr <- rD[["client"]]
remDr$navigate("file:///C:/Bernd/R/covid_canberra/docs/index.html")

Sys.sleep(10) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

remDr$close()
rD$server$stop()
rm(rD)
gc()

#if fine commit and push


