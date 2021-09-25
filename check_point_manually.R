#check points


#206
tab3[which(tab3$lon<145),1:6]
tab3[which(is.na(tab3$lon)),1:6]


column="Street"
ss <-"35 Science Road" 
ii <- grep(ss, tab3[, which(column==colnames(tab3))])
ii

tab3[ii,]
tm <- leaflet() %>% addTiles()
tm <- tm %>% addCircleMarkers(lat=tab3$lat[ii], lng=tab3$lon[ii],popup = labs[ii], weight=0.5, color = "red", radius = 5 , fillOpacity = 0.8)
tm


dd <- geocode("35 Science Road, Acton, Canberra")
as.data.frame(dd)



tm <- tm %>% addCircleMarkers(lat=dd$lat, lng=dd$lon,popup = labs[ii], weight=0.5, color = "green", radius = 5 , fillOpacity = 0.8)
tm
#tab3<- fixgeo("Flatheads Takeaway", lat=-35.264, lon=149.122)
#cat(paste0('tab3 <- fixgeo("',ss,'", lat =   ', -35.23793, ', lon =', 149.0653,')' ))

cat(paste0('tab3 <- fixgeo("',ss,'", column = "',column,'"  , lat =   ', dd$lat, ', lon =', dd$lon,')' ))
