# Reproducible Research: Peer Assessment 1



# Loading the Data

Let's go ahead and read in the CSV file of activity data and take a look at some quick statistics.


```r
activityData <- read.csv("~/RepData_PeerAssessment1/activity.csv")
summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?

In order to do this, we can use the aggregate function to find the sum of the number of steps taken per day.


```r
aggdata <- aggregate(activityData$steps, by=list(Date = activityData$date), FUN=sum, na.rm=TRUE)
```

Now let's create a simple chart to show this information.


```r
plot(aggdata$Date, aggdata$x, type = "l", xlab = "Date", ylab = "Total Steps Taken", 
     main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

We can calculate the mean and median number of steps taken with the following calculations.


```r
# Calculate the mean
meanSteps <- mean(aggdata$x)

#Calculate the median
medianSteps <- median(aggdata$x)
```

The mean total number of steps taken every day is 9354.2295082.
The median total number of steps taken every day is 10395.

## What is the average daily activity pattern?

Let's first aggregate the number of steps taken by the 5 second interval.


```r
aggdata5sec <- aggregate(activityData$steps, by=list(Interval = activityData$interval), FUN=sum, na.rm=TRUE)
```

Now let's plot it.


```r
plot(aggdata5sec$Interval, aggdata5sec$x, type = "l", xlab = "Interval", ylab = "Total Steps Taken", 
     main = "Total Steps per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

We can find the maximum number of steps taken by using the following calculation.


```r
max5sec <- aggdata5sec[aggdata5sec$x == max(aggdata5sec$x),]
```

We can then find that the maximum number of steps taken on any interval was 10927.
The interval in which that occured was 835.

## Inputing missing values

Let's find the number of missing values.


```r
missingRows <- nrow(activityData[is.na(activityData$steps) == TRUE,])
```

There are 2304 rows with NA in the steps column.

In order to fillin the rows, let's use a simple method. We will choose to use the mean value for that
particular interval. So let's first calculate the mean for each interval.


```r
meanData5sec <- aggregate(activityData$steps, by=list(Interval = activityData$interval), FUN=mean, na.rm=TRUE)
```

Now let's create a new dataframe that fills in the gaps using the mean number of steps for each interval.


```r
completeData <- activityData

for(i in 1:length(completeData$steps)){
  if(is.na(completeData$steps[i])){
    completeData$steps[i] <- meanData5sec$x[meanData5sec$Interval == completeData$interval[i]]
  }
}
```

Now like before, let's sum up the number of steps and create a histogram of the information.


```r
aggdataComplete <- aggregate(completeData$steps, by=list(Date = completeData$date), FUN=sum, na.rm=TRUE)
plot(aggdataComplete$Date, aggdataComplete$x, type = "l", xlab = "Date", ylab = "Total Steps Taken", 
     main = "Total Steps per Day (Complete Data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

Let's calculate the mean and the median.


```r
meanComplete <- mean(aggdataComplete$x)
medianComplete <- median(aggdataComplete$x)
```

Again, here is the information with the missing values:

Mean: 9354.23

Median: 10395

And here is the information again with the NA values filled in:

Mean: 10766.19

Median: 10766.19

It looks like filling in the NA values raised the mean and median values.

## Are there differences in activity patterns between weekdays and weekends?

Well let's first create a new vector that converts the date column with factors into a data type of date.


```r
tempDate <- weekdays(as.Date(as.character(completeData$date), format = "%Y-%m-%d"))
```

And now let's cycle through them and detect whether they are weekends or weekdays. We can then
append that column into the dataframe.


```r
for(i in 1:length(tempDate)){
  if(tempDate[i] == "Saturday" | tempDate[i] == "Sunday"){
    tempDate[i] <- "Weekend"
  }
  else{
    tempDate[i] <- "Weekday"
  }
}

completeData$dayOfWeek <- tempDate
```

Now we create a new dataframe that aggregates the steps by time interval and by day of week.


```r
aggDataDay <- aggregate(completeData$steps, 
                        by=list(Interval = completeData$interval, Day = completeData$dayOfWeek),
                        FUN=mean, na.rm=TRUE)
```

And now we plot it.


```r
par(mfrow = c(2,1))

plot(aggDataDay$Interval[aggDataDay$Day == "Weekday"], 
     aggDataDay$x[aggDataDay$Day == "Weekday"], 
     type = "l", xlab = "Interval", ylab = "Avg. Steps Taken", 
     main = "Average Steps per Interval on Weekdays")

plot(aggDataDay$Interval[aggDataDay$Day == "Weekend"], 
     aggDataDay$x[aggDataDay$Day == "Weekend"], 
     type = "l", xlab = "Interval", ylab = "Avg. Steps Taken", 
     main = "Average Steps per Interval on Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 

And now we're done!
