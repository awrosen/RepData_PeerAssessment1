Loading and preprocessing the data
----------------------------------

    setwd("/Users/Andreas/Desktop/Data Science/Reproducible Research")
    getwd()

    ## [1] "/Users/Andreas/Desktop/Data Science/Reproducible Research"

    dat<-read.csv("activity.csv",header=T)

### What is mean total number of steps taken per day?

    steps<-aggregate(steps~date,dat,sum)
    s<-hist(steps$steps,xlab="No. of steps per day",main=paste("Histogram of No. of steps per day"),breaks=20,col="palegreen4")

![](Figs/unnamed-chunk-2-1.png)<!-- -->

    print(s)

    ## $breaks
    ##  [1]     0  1000  2000  3000  4000  5000  6000  7000  8000  9000 10000
    ## [12] 11000 12000 13000 14000 15000 16000 17000 18000 19000 20000 21000
    ## [23] 22000
    ## 
    ## $counts
    ##  [1]  2  0  1  1  1  2  1  2  5  2 10  6  6  4  2  5  0  1  0  0  1  1
    ## 
    ## $density
    ##  [1] 3.773585e-05 0.000000e+00 1.886792e-05 1.886792e-05 1.886792e-05
    ##  [6] 3.773585e-05 1.886792e-05 3.773585e-05 9.433962e-05 3.773585e-05
    ## [11] 1.886792e-04 1.132075e-04 1.132075e-04 7.547170e-05 3.773585e-05
    ## [16] 9.433962e-05 0.000000e+00 1.886792e-05 0.000000e+00 0.000000e+00
    ## [21] 1.886792e-05 1.886792e-05
    ## 
    ## $mids
    ##  [1]   500  1500  2500  3500  4500  5500  6500  7500  8500  9500 10500
    ## [12] 11500 12500 13500 14500 15500 16500 17500 18500 19500 20500 21500
    ## 
    ## $xname
    ## [1] "steps$steps"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

    meansteps<-mean(steps$steps,na.rm=T)
    mediansteps<-median(steps$steps,na.rm=T)

The `mean` is 1.076618910^{4} while the `median` is 10765

What is the average daily activity pattern?
-------------------------------------------

    stepinterval<-aggregate(steps~interval,dat,mean,na.rm=T)
    s<-plot(stepinterval$interval,stepinterval$steps,type="l",xlab="Interval",ylab="Mean no. of steps",main=paste("Plot showing the mean no. of steps per 5 min. interval"),col="midnightblue")

![](Figs/unnamed-chunk-3-1.png)<!-- -->

    print(s)

    ## NULL

    which.max(stepinterval$steps)

    ## [1] 104

    stepinterval[104,]

    ##     interval    steps
    ## 104      835 206.1698

On average, the interval containing the most steps is 835 with
206.1698113 steps.

Imputing missing values
-----------------------

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

     comc<-!complete.cases(dat)
    sum(comc==TRUE)

    ## [1] 2304

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I choose to use tapply yo finde the mean no. of steps for a given
interval and inserting them into the noncomplete cases in a new data
frame.

### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

    datc<-dat
    avg_int<-with(dat,tapply(steps,interval,data=dat,mean,na.rm=T))
    datc$steps[comc]<-avg_int[as.character(datc$interval[comc])]
    sum(is.na(comc))

    ## [1] 0

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    steps<-aggregate(steps~date,datc,sum)
    s<-hist(steps$steps,xlab="No. of steps per day",main=paste("Histogram of No. of steps per day, modified data"),breaks=20,col="maroon")

![](Figs/unnamed-chunk-6-1.png)<!-- -->

    print(s)

    ## $breaks
    ##  [1]     0  1000  2000  3000  4000  5000  6000  7000  8000  9000 10000
    ## [12] 11000 12000 13000 14000 15000 16000 17000 18000 19000 20000 21000
    ## [23] 22000
    ## 
    ## $counts
    ##  [1]  2  0  1  1  1  2  1  2  5  2 18  6  6  4  2  5  0  1  0  0  1  1
    ## 
    ## $density
    ##  [1] 3.278689e-05 0.000000e+00 1.639344e-05 1.639344e-05 1.639344e-05
    ##  [6] 3.278689e-05 1.639344e-05 3.278689e-05 8.196721e-05 3.278689e-05
    ## [11] 2.950820e-04 9.836066e-05 9.836066e-05 6.557377e-05 3.278689e-05
    ## [16] 8.196721e-05 0.000000e+00 1.639344e-05 0.000000e+00 0.000000e+00
    ## [21] 1.639344e-05 1.639344e-05
    ## 
    ## $mids
    ##  [1]   500  1500  2500  3500  4500  5500  6500  7500  8500  9500 10500
    ## [12] 11500 12500 13500 14500 15500 16500 17500 18500 19500 20500 21500
    ## 
    ## $xname
    ## [1] "steps$steps"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

    meansteps<-mean(steps$steps,na.rm=T)
    mediansteps<-median(steps$steps,na.rm=T)

The new `mean` is 1.076618910^{4} and the new `median` is
1.076618910^{4}. The mean remains the same and the median has only
changed slightly with our approch, however this would be sensible to
which approch was choosen to replace the NAs.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    Sys.setlocale("LC_TIME", "en_US.UTF-8")

    ## [1] "en_US.UTF-8"

    datc$date<-as.Date(datc$date)
    datc$dayname<-weekdays(datc$date)

    weekend<-c("Saturday","Sunday")
    datc$daytype <- as.factor(ifelse(datc$dayname == weekend, "weekend", "weekday"))

### 2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    library(lattice)
    daytypeplot<-aggregate(steps~interval+daytype,datc,mean)
    s<-xyplot(steps~interval|factor(daytype),data=daytypeplot,aspect=1/3,type="l",col="deepskyblue4",xlab="5 min. time interval",ylab="Avg. no. of steps")
    print(s)

![](Figs/unnamed-chunk-8-1.png)<!-- -->
