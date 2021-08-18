#check points


#206


mb <- m
ss <-"Westfield Belconnen Food Court" 
ii <- grep(ss, tab3$Exposure.Location)
ii

tab3[ii,]
tm <- leaflet() %>% addTiles()
tm <- tm %>% addCircleMarkers(lat=tab3$lat[ii], lng=tab3$lon[ii],popup = labs[ii], weight=0.5, color = "red", radius = 5 , fillOpacity = 0.8)
tm


dd <- geocode("204 Benjamin Way, Belconnen, Food Court, Canberra, Australia")
as.data.frame(dd)



tm <- tm %>% addCircleMarkers(lat=dd$lat, lng=dd$lon,popup = labs[ii], weight=0.5, color = "green", radius = 5 , fillOpacity = 0.8)
tm
#tab3<- fixgeo("Flatheads Takeaway", lat=-35.264, lon=149.122)
cat(paste0('tab3 <- fixgeo("',ss,'", lat =   ', -35.23793, ', lon =', 149.0653,')' ))
