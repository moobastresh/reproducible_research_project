# read data
data1<-read.csv("activity.csv")
# compute total number of steps taken each day
attach(data1)
aggdata<-aggregate(steps, by=list(date), FUN=sum, na.rm=TRUE)
colnames(aggdata)<-c("date","sum_steps")
hist(aggdata$sum_steps,breaks=10,xlab="total number of steps",main="Histogram of total number of steps taken each day")
# compute mean and median of total steps taken each day
m1<-mean(aggdata$sum_steps)
m2<-median(aggdata$sum_steps)
abline(v=m1,lwd=2,col="red")
abline(v=m2,lwd=2,col="green")
# Time series plot of the average number of steps taken
avg_steps<-aggregate(steps, by=list(interval), FUN=mean, na.rm=TRUE)
colnames(avg_steps)<-c("interval","avgsteps")
plot(avg_steps$interval,avg_steps$avgsteps,type="l",xlab="interval",ylab="Average steps",main="average number of steps taken, averaged across all days")
# The 5-minute interval that, on average, contains the maximum number of steps
newdata<-avg_steps[order(-avg_steps$avgsteps),]
max_step_interval<-newdata$interval[1]
# Code to describe and show a strategy for imputing missing data
# Calculate and report the total number of missing values in the dataset
sum(is.na(data1$steps))
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that 5-minute interval
data2<-merge(data1,avg_steps,by="interval")
NAset<-which(is.na(data2$steps)==TRUE)
data2$steps[NAset]=data2$avgsteps[NAset]
# Create a new dataset that is equal to the original dataset
# but with the missing data filled in.
data3<-data.frame(data2$steps,data2$date,data2$interval)
colnames(data3)<-c("steps","date","interval")
# make histogram of total steps taken each day,
# compute the mean, median steps taken each day. Are they different from the original?
attach(data3)
aggdata2<-aggregate(steps,by=list(date),FUN=sum,na.rm=TRUE)
colnames(aggdata2)<-c("date","sum_steps")
hist(aggdata2$sum_steps,breaks=10,xlab="total number of steps",main="Histogram of total number of steps taken each day")
m3<-mean(aggdata2$sum_steps)
m4<-median(aggdata2$sum_steps)
abline(v=m3,lwd=2,col="red")
abline(v=m4,lwd=2,col="green")
# Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
d1<-as.Date(data3$date)
data3$day<-weekdays(d1)
r1<-which(data3$day==c("Saturday","Sunday"))
data3$weekd<-"weekday"
data3$weekd[r1]<-"weekend"
# make panel plots
# time series plot of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
library(lattice)
attach(data3)
aggdata3<-aggregate(steps, by=list(interval,weekd), FUN=mean, na.rm=TRUE)
colnames(aggdata3)<-c("interval","weekd","steps")
xyplot(steps~interval|weekd,type="l",data=aggdata3,layout=c(1,2))
png(filename="figures/panel_plot.png",width = 480, height = 480, units = "px")