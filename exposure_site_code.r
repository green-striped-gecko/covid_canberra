# Exposure sites per Update

``` {r}


allfiles <- list.files("./data/", pattern="table_")
fo <- allfiles[order(file.mtime(file.path("data",allfiles)), decreasing = FALSE)]
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

ggplot(res, aes(x=update,y=nsites))+geom_bar(stat="identity", position = "stack", width=1, aes(fill=contact))+xlab("update #")+ylab("# exposure sites")+ggtitle("Exposure sites over updates")


```

