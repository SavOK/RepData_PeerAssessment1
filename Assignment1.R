#! /opt/local/bin/Rscript

library("data.table")
library("lubridate")
library("knitr")


CWD <- "~/Coursera/DataScience/Part5/RepData_PeerAssessment1"
setwd(CWD)

cacheData <- function(x, ...) {
  DT <- x$get_dt()
  if(!is.null(DT)) {
    #    message("getting cached data")
    return(DT)
  }
  data <- x$get()
  DT <- get_data_helper(File = data,  ...)
  x$set_dt(DT)
  return(DT)
}

read_dataCache <- function(file = character()) {
  DT <- NULL
  set <- function(y) {
    file <<- y
    DT <<- NULL
  }
  get <- function() {
    file
  }
  set_dt <- function(dt) {
    DT <<- dt
  }
  get_dt <- function() {
    DT
  }
  return (list (set = set, get = get, 
                set_dt = set_dt, get_dt = get_dt))
}

# Read working file returns data.table. If file is absent in directory downloads it from web
get_data_helper <- function(File = character()) {
  if (file.exists(File) ){
    DT <- fread(input = File, nrows = -1L, header = T,
                stringsAsFactors = F)
    DT$date <- as.factor(ymd(DT$date))
    return ( DT )
  }
  URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" # url to data
  download.file(url = URL, "./activity.zip", quiet = T, method = "curl")
  unzip(zipfile = "./activity.zip")
  dataDate <- date()
  print( sptrintf("Data Set was downloaded on %s\n", dataDate))
  DT <- fread(input = File, nrows = -1L, header = T,
              stringsAsFactors = F)
  DT$date <- as.factor(ymd(DT$date))
  return ( DT )
}

DataFile <- "./activity.csv"
CacheData <- read_dataCache( DataFile )
DT <- cacheData(CacheData)

# Calculate total number of steps in each day
DT <- DT[, sum(steps, na.rm = T), by = date]
# some general statistics
summaryStr <- sprintf("mean:    %8.0f\nmedian: %8.0f\n", mean(DT$V1), median(DT$V1) ) 
png("./figures/plot1.png",  width = 800, height = 800)
par(mar=c(5, 5, 5, 1))
hist(DT$V1, breaks = 10, 
     main = "Total number of steps in a day", xlab = "Number of steps", 
     col = "gray", cex.axis = 2, cex.main = 2, cex.lab =2)
text(labels=summaryStr,  x=17000, y=12, cex = 1.5)
dev.off()
print(summaryStr)

# The average daily activity
DT <- cacheData(CacheData)
DT <- DT[, mean(steps, na.rm = T), by = interval]
maxStr <- sprintf("Maximum at interval %4.0f", DT$interval[which.max(DT$V1)] ) 
png("./figures/plot2.png",  width = 800, height = 800)
par(mar=c(5, 5, 5, 1))
plot(x = DT$interval, y = DT$V1, type = "l", lwd = 2, col = "red", 
     xlab = "Interval", ylab = "Number of steps", main = "Avarage number of steps",
     cex.axis = 2, cex.main = 2, cex.lab =2, ylim = c(0, 210) )
points(x = DT$interval[which.max(DT$V1)], y = max( DT$V1), 
       col = "blue", pch = 1, cex = 2)
text(labels = maxStr, x = DT$interval[which.max(DT$V1)] + 30, y = max( DT$V1), 
     cex = 1.5, adj = 0 ) 
dev.off()
print(maxStr)

# Missing values
DT <- cacheData(CacheData)
NAStr <- sprintf("Total number of missing values %d\n", sum(!complete.cases(DT)))
print(NAStr)

# replace NA with mean value
replace_NA <- function(X) {
  MEAN <- round(mean(X, na.rm = T), 0) # use round since 0.4 steps make very little meaning 
  if (is.nan(MEAN) ) { MEAN <- 0 } # If no measurments in this window
  X[is.na(X)] <- MEAN
  return (X)
}

# replace NA with mean values for that marticular window (window is data/inteval)
DT_NEW <- DT[, replace_NA(steps), by = list(date, interval)]
colnames(DT_NEW) <- c("date", "interval", "steps") 
summaryStatNoNA <- summary(data.frame(DT)[,c(3, 1, 2)])
DT_SUM <- DT_NEW[, sum(steps), by = date]
# some general statistics
summaryStr <- sprintf("mean:    %8.0f\nmedian: %8.0f\n", mean(DT_SUM$V1), median(DT_SUM$V1) ) 
png("./figures/plot3.png",  width = 800, height = 800)
par(mar=c(5, 5, 5, 1))
hist(DT_SUM$V1, breaks = 10, 
     main = "Total number of steps in a day", xlab = "Number of steps", 
     col = "gray", cex.axis = 2, cex.main = 2, cex.lab =2)
text(labels=summaryStr,  x=17000, y=12, cex = 1.5)
dev.off()
print(summaryStr)
identicalStr <- sprintf("Is identical, %s",  
                        identical(summary(data.frame(DT_NEW)[,c(3, 1, 2)]), 
                 summary(DT[complete.cases(DT)]) ))
print("Summary initial set\n")
print(summary(DT[complete.cases(DT)]))
print("Summary updated set\n")
print(summary(data.frame(DT_NEW)[,c(3, 1, 2)]))
print(identicalStr)

#Create a new factor variable in the dataset with two levels
DT <-  cacheData(CacheData)
DT_NEW <- DT[, replace_NA(steps), by = list(date, interval)]
colnames(DT_NEW) <- c("date", "interval", "steps") 
DT_NEW$day_type<- as.factor(wday(DT_NEW$date) == 1 | wday(DT_NEW$date) == 7)
levels(DT_NEW$day_type) <- c("weekday", "weekend")

# plot activity in each inteval vs deferent weekday
DT_NEW <- DT_NEW[, mean(steps), by=list(interval, day_type)]
setkey(DT_NEW, day_type)
png("./figures/plot4.png",  width = 800, height = 800)
par(mar=c(1, 5, 2, 1), fig=c(0.,1,0.5,1))
plot(x = DT_NEW["weekday"]$interval, y = DT_NEW["weekday"]$V1, 
     type = "l", col = "red", lwd = 2, 
      xlab = "Interval", ylab = "Avarage number of steps", 
              cex.axis = 2, cex.main = 2, cex.lab =2,  xaxt="n")
mtext("Weekday", side=3 ,adj = 1, cex=2)
par(mar=c(5, 5, 1, 1), fig=c(0.0, 1, 0, 0.5), new = T)
plot(x = DT_NEW["weekend"]$interval, y = DT_NEW["weekend"]$V1, 
     type = "l", col = "red", lwd = 2, 
     xlab = "Interval", ylab = "Avarage number of steps", 
     cex.axis = 2, cex.main = 2, cex.lab =2)
mtext("Weekend", side=3 ,adj = 1, cex=2)
dev.off()




