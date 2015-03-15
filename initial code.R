setwd('c:/users/Lee/Desktop/sharedwithvm/datasciencespec/RepData_PeerAssessment1/')


if (!file.exists('activity.csv')) {
     if (!file.exists('activity.zip')) {
          fileurl<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
          download.file(fileurl,dest='./activity.zip')
     }
     unzip('activity.zip')
     activity <- read.csv('activity.csv')
}

head(activity,15)
which(!is.na(activity))[1]
activity[289:300,]
unique(activity$date)
which(activity[,1]>0)[1]
activity[530:560,]

hist(activity[activity[,1]>0,1])

dailysteps <- aggregate(activity[,1],list(activity[,2]),sum,na.rm=TRUE)
dailymeans <- aggregate(activity[,1],list(activity[,2]),mean,na.rm=TRUE)
dailymedians <- aggregate(activity[,1],list(activity[,2]),median,na.rm=TRUE)

dailymean <- mean(dailysteps$x,na.rm=TRUE)
dailymedian <- median(dailysteps$x,na.rm=TRUE)

library(ggplot2)
png(file = 'dailytotalhist.png',
    width = 640,
    height = 480,
    bg = 'transparent')
g <- ggplot(dailysteps,aes(x))
g + geom_histogram(fill="steelblue",binwidth=500) + 
     annotate('text',
               x=5000,
               y=8,
               label=sprintf('Daily Mean: %4.2f\nDaily Median: %i',
                             dailymean,dailymedian),
               hjust = 0) +
     labs(x = "total steps per day")
dev.off()

averagesteps <- aggregate(list('mean' = activity[,1])
                          ,list('interval' = activity[,3]),
                          mean,
                          na.rm=TRUE)


png(file = 'averageactivity.png',
    width = 640,
    height = 480,
    bg = 'transparent')
g <- ggplot(averagesteps,aes(x=interval,y=mean)) +
     geom_line(aes(group=1),colour="blue")
g <- g + labs(y = "daily average steps")
g <- g + scale_x_discrete(breaks = activity$interval[sep(1,288,12)])

maxind <- which(averagesteps$mean==max(averagesteps$mean))
maxval <- averagesteps$mean[maxind]
maxspot <- averagesteps$interval[maxind]

g + geom_point(aes(x=maxspot,y=maxval),colour='red') +
     annotate("text",x=maxspot,y=maxval,
              label = "maximum steps at 104th interval (8:35 AM)",
              hjust = 0)

dev.off()

table(is.na(activity[,1]))

library(chron)
splitlist <- is.weekend(activity[,2])
weekdays <- activity[splitlist,]
weekends <- activity[!splitlist,]

averageweekday <- aggregate(weekdays[,1],list(weekdays[,3]),mean,na.rm=TRUE)
averageweekend <- aggregate(weekends[,1],list(weekends[,3]),mean,na.rm=TRUE)

iactivity <- activity
indiceswd <- is.na(activity[,1])&!splitlist
indiceswe <- is.na(activity[,1])&splitlist
iactivity[indiceswd,1] <- as.integer(averageweekday$x)
iactivity[indiceswe,1] <- as.integer(averageweekend$x)

idailysteps <- aggregate(iactivity[,1],list(iactivity[,2]),sum,na.rm=TRUE)

idailymean <- mean(idailysteps$x,na.rm=TRUE)
idailymedian <- median(idailysteps$x,na.rm=TRUE)


g <- ggplot(idailysteps,aes(x))
g + geom_histogram(fill="steelblue",binwidth=500) + 
     annotate('text',
              x=5000,
              y=8,
              label=sprintf('Daily Mean: %4.2f\nDaily Median: %i',
                            idailymean,idailymedian),
              hjust = 0) +
     labs(x = "total steps per day")

iactivity$daytype <- factor(c('weekday','weekend'))

iactivity$daytype[splitlist] <- 'weekend'
iactivity$daytype[!splitlist] <- 'weekday'

averagebydays <- rbind(averageweekday,averageweekend)
averagebydays$daytype <- factor(c('weekday','weekend'))
averagebydays$daytype[1:288] <- 'weekday'
averagebydays$daytype[289:576] <- 'weekend'

g <- ggplot(averagebydays,aes(x=Group.1,y=x)) + 
     geom_line(aes(group=1),colour="#000099") +
     labs(title = "5 minute interval versus daily average steps",
          x = "5 minute interval",
          y = "daily average steps")
g+facet_grid(.~daytype)


test <- activity


test$interval <- sprintf('%04d',test$interval)
test$interval <- gsub(pattern = '([0-9]{1,2})([0-9]{2})',
              replacement = '\\1:\\2',
              x = test$interval)

taveragesteps <- aggregate(list('mean' = test[,1])
                          ,list('interval' = test[,3]),
                          mean,
                          na.rm=TRUE)

g <- ggplot(taveragesteps,aes(x=interval,y=mean)) + 
     geom_line(aes(group=1),colour="blue") +
     labs(title = "5 minute interval versus daily average steps",
          y = "daily average steps")
g + scale_x_discrete(breaks = taveragesteps$interval[seq(1,288,12)])

