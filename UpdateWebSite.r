

#update website (once geocode_tables showed an update to create index.html and then commit)

rmarkdown::render("c:/bernd/r/covid_canberra/Covid_Exposure_ACT.rmd", output_dir = "docs", params=list(lup=lup), output_file = "index.html")


