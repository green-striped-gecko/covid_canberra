library(ggplot2)

allfiles <- list.files("./data/", pattern="table_")
fo <- flast[order(file.mtime(file.path("data",flast)), decreasing = FALSE)]
res <- data.frame(update=NA, date=NA,locations=NA, contact=NA,nsites=NA )
cc<-1
for (i in 1:length(fo))
{
  dd <- read.csv(file.path("data",fo[i]))
  tt <- table(dd$Contact)
  if (length(tt)>0) {
  for (ii in 1:length(tt))
    {
    res[cc, 1] <- i
    res[cc,2] <- (substr(dd$Date[i], 1,10))
    res[cc,3] <- nrow(dd)
    res[cc,4] <- names(tt)[ii]
    res[cc,5] <- tt[ii]
    cc <- cc+1
    }
  }
}


res <- res[!res$contact=="Investigation information",]
res <- res[!res$contact=="Investigation location",]
res$contact <- factor(res$contact, levels=c("Close", "Casual","Monitor"))


res <- res[order(res$contact),]
cols <- c( "red", "yellow","blue")[as.numeric(res$contact)]

ggplot(res, aes(x=update,y=nsites, fill=contact))+geom_bar(stat="identity", position = "stack", width=1, fill=cols)+xlab("")+ylab("")+theme_grey()+theme(axis.text.x =element_blank(), axis.ticks = element_blank())


library(dplyr)


res2 <- ddply(res[,c("update","locations")],.(update), summarise, loc=max(locations))
       
res3 <- res2[22:31,]
  
  m <- lm(loc~update, data=res3)
  plot(res3, ylim=c(0,400), xlim=c(25,50))
  m
  summary(m)
  abline(m)
  
  cat(paste("Zero at:", round(-(m$coefficients[1]/m$coefficients[2]),3)))
  
  
  
