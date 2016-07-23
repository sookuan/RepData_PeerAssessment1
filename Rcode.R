1 Code for reading in the dataset and/or processing the data
> activity<-read.csv("C:/Users/hp/documents/activity.csv")
> activity$date<-as.Date(activity$date,"%Y-%m-%d")

2 Histogram of the total number of steps taken each day
> d1<-aggregate(x=activity[c("steps","interval")],FUN=sum,by=list(Group.date=activity$date),na.rm=TRUE)
> hist(d1$steps,col="grey",border="black",xlab="Interval",main="Histogram-Total Steps per day")

3 Mean and median number of steps taken each day
> d1mean<-setNames(aggregate(x=activity[c("steps")],FUN=mean,by=list(activity$date),na.rm=TRUE),c("Date","Mean"))
> d1median<-setNames(aggregate(x=activity[c("steps")],FUN=median,by=list(activity$date),na.rm=TRUE),c("Date","Median"))
> d1stats<-merge(d1mean,d1median,by="Date")
> print(d1stats)

4 Time series plot of the average number of steps taken
## aggregate table 
> d2mean<- setNames(aggregate(x= activity[c("steps")],
+ FUN = mean,
+ by = list(activity$interval),na.rm=TRUE),c("Interval","MeanStep"))

## Interval with Max Average Steps
> d2max<- subset(d2mean,d2mean$MeanStep==max(d2mean$MeanStep,na.rm=TRUE))

## Plot Histogram
> plot(d2mean$Interval,d2mean$MeanStep,type="l",main="Steps per Interval",xlab="Interval",ylab="Step(Mean)",col="green",lwd=1)
> abline(v=d2max$Interval,col ="blue",lwd=2)
> legend(x="topright", c("MaxSteps Interval"),col=c("blue"),lwd=c(2,2,2))

5 The 5 mins interval that, on average, contains the maximum number of steps
> d2max<-subset(d2mean,d2mean$MeanStep==max(d2mean$MeanStep,na.rm=TRUE))
> print(d2max)

6 Code to describe and show a strategy for inputing missing data
## create new variable
> d3<-activity
## Replace NA values in column "Steps" with global average
> d3$steps[which(is.na(d3$steps))]<-mean(d3$steps,na.rm=TRUE)
> heads (d3)

7 Histogram of the total number of steps taken each day after missing values are inputed
## aggregate data for histogram
> d4<- aggregate(x=d3[c("steps")],
+ FUN=sum,
+ by=list(Group.date=activity$date),na.rm=TRUE)

## stats
> d4mean <- setNames(aggregate(x = d4[c("steps")],
+ FUN = mean,
+ by = list(d4$Group.date),na.rm=TRUE),c("Date","Mean"))
> d4median<- setNames(aggregate(x=d4[c("steps")],
+ FUN = median,
+ by = list(d4$Group.date),na.rm=TRUE),c("Date","Median"))
> d4stats<- merge(d4mean,d4median,by="Date")
> print(d4stats)
## Plot Histogram
> hist(d4$steps,col="grey",border="black",xlab="SUM(steps)", main="Histogram Total Steps per day")
> abline(v=mean(d4$steps),col="red",lwd=2)
> abline(v=median(d4$steps),col="blue",lty=2,lwd=2)
> legend(x="topright",c("mean","median"),col = c("red","blue"),lwd=c(2,2,2))

8 Panel plot comparing the average number of steps taken per 5-mins interval across weekdays and weekends
> d5<-d3
> d5$day<- weekdays(d3$date)
> d5$day<- ifelse(d5$day=="Saturday"|d5$day=="Sunday","weekend","weekday")
> d5$day<-as.factor(d5$day)

## aggregate data 
> d5sum<-setNames(aggregate(x=d5[c("steps")],FUN=sum,by=list(d5$interval,d5$day),na.rm=TRUE),c("Interval","day","SumStep"))

## Subset data for graphics
> d5weekday<-subset(d5sum,d5sum$day=="weekday")
> d5weekend<- subset(d5sum,d5sum$day=="weekend")

## plot histogram
> par(mfrow=c(2,1))
> plot(d5weekday$Interval,d5weekday$SumStep,main="Interval vs SumStep [weekdays]",type="l",col="green",xlab="Interval",ylab="SumStep")
> plot(d5weekend$Interval,d5weekend$SumStep,main="Interval vs SumStep[weekend]",type="l",col="blue",xlab="Interval",ylab="SumStep")




