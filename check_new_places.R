library(stringr)
library(data.table)
data_dir <- paste0(getwd(),"/data")
old_tables <- list.files(data_dir,pattern = "table")
last_table <- read.csv(paste0(data_dir,"/last.csv"))

old_tables_time_temp <- str_split(old_tables,pattern = "_")
old_tables_time <- rbindlist(lapply(lapply(old_tables_time_temp, t), as.data.frame))
colnames(old_tables_time) <- c("type","day","month","year","hour")
old_tables_time$hour <- substr(old_tables_time$hour,1,nchar(old_tables_time$hour)-4)
old_tables_time$hour_a <- str_sub(old_tables_time$hour, start= -6,end = -3)
old_tables_time$hour_a <- as.numeric(old_tables_time$hour_a)
old_tables_time$hour_b <- str_sub(old_tables_time$hour, start= -2) 
pm_times <- which(old_tables_time$hour_b == "pm")
old_tables_time[pm_times,"hour_a"] <-  old_tables_time[pm_times,"hour_a"] + 1200
old_tables_time$file_name <- old_tables
old_tables_time <- old_tables_time[order(old_tables_time$year,
                                         old_tables_time$month,
                                         old_tables_time$day,
                                         old_tables_time$hour_a,decreasing = T),]

second_last_table <- read.csv(paste0(data_dir,"/",old_tables_time[1,"file_name"]))

new_places_temp <- !last_table$Place %in% second_last_table$Place
new_places <- last_table[new_places_temp,]

