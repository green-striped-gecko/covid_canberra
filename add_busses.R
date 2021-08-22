require(rgdal)

#as shape file
busses <- readOGR(dsn = "c:/bernd/r/covid_canberra/bus", layer = "geo_export_69c76e06-1d3f-4619-be3b-b4e5789be8ca")

bm <- nm
b43 <- (busses[busses$short_name=="43",])
coo <- coordinates(b43)
cood <- data.frame(coo[[1]])


#bm <- bm %>% addPolylines(lng=cood[,1], lat=cood[,2], color = "green", weight   = 3, opacity = 0.8)


#bm
bindex <- grep("BUS", tab3$Exposure.Location)
buslanes <- tab3$Exposure.Location[bindex]

busnumbers <- gsub("Bus Route ([0-9,A-Z]+) Transport.*","\\1", buslanes)

blineindex <- which(busses$short_name %in% busnumbers)

bnames <- busses$short_name[blineindex]


bb <- (busses[blineindex,])
coo <- coordinates(bb)
bcols <- colorRampPalette(c("purple", "green"))( length(coo)) ## (n)

#bm <- leaflet() %>% addTiles()
for (ib in 1:length(coo))
{

cood <- data.frame(coo[[ib]])
m <- m %>% addPolylines(lng=cood[,1], lat=cood[,2], color = bcols[ib], weight   = 3, opacity = 0.4, popup  = paste0("Bus ", bnames[ib]))

}
m

