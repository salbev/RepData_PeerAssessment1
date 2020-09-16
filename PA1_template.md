---
title: "Reproducible Research - Week 2 assignment"
author: "SBev"
date: "16/09/2020"
output: 
  html_document:
   keep_md: TRUE
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

### Introduction

It is now possible to collect a large amount of data about personal movement 
using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone 
Up. These type of devices are part of the “quantified self” movement – a group 
of enthusiasts who take measurements about themselves regularly to improve their
health, to find patterns in their behavior, or because they are tech geeks. But 
these data remain under-utilized both because the raw data are hard to obtain 
and there is a lack of statistical methods and software for processing and 
interpreting the data.

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data 
consists of two months of data from an anonymous individual collected during 
the months of October and November, 2012 and include the number of steps taken 
in 5 minute intervals each day.

The data for this assignment can be downloaded from the 
[course web site](https://d396qusza40orc.cloudfront.net/
repdata%2Fdata%2Factivity.zip).

### Part 1 - reading the data

The following code deals with Setting the working directory where the zip file
has been saved followed by reading the data in and formatting the date 
correctly.


```r
setwd("C:/Users/sally/Desktop/Coursera/5. Reproducible Research/Week 2 assign")
actdata<-read.csv("activity.csv", header=TRUE, sep=",", na.strings = "NA")
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
actdata$date<-ymd(actdata$date)
```
### The total mean number of steps taken per day

The step data has been grouped by date and a histogram of the total mean steps
taken per day is shown in Figure 1 below.


```r
 library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
actdataPD<-actdata %>%
    group_by(date) %>%
    summarise(totsteps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(actdataPD$totsteps, xlab="Total steps per day", main="Figure 1 - 
       Histogram of total steps taken each day over a two month period")
```

![](PA1_template_files/figure-html/plot 1-1.png)<!-- -->


The mean and median of the daily step data is shown here:

```r
library(xtable)
t1<-xtable(summary(actdataPD))
print(t1, type = "html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Wed Sep 16 13:26:34 2020 -->
<table border=1>
<tr> <th>  </th> <th>      date </th> <th>    totsteps </th>  </tr>
  <tr> <td align="right"> X </td> <td> Min.   :2012-10-01   </td> <td> Min.   :   41   </td> </tr>
  <tr> <td align="right"> X.1 </td> <td> 1st Qu.:2012-10-16   </td> <td> 1st Qu.: 8841   </td> </tr>
  <tr> <td align="right"> X.2 </td> <td> Median :2012-10-31   </td> <td> Median :10765   </td> </tr>
  <tr> <td align="right"> X.3 </td> <td> Mean   :2012-10-31   </td> <td> Mean   :10766   </td> </tr>
  <tr> <td align="right"> X.4 </td> <td> 3rd Qu.:2012-11-15   </td> <td> 3rd Qu.:13294   </td> </tr>
  <tr> <td align="right"> X.5 </td> <td> Max.   :2012-11-30   </td> <td> Max.   :21194   </td> </tr>
  <tr> <td align="right"> X.6 </td> <td>  </td> <td> NA's   :8   </td> </tr>
   </table>



### The average daily activity pattern

Figure 2 below shows the average number of steps taken per 5-minute interval 
across a 24 hour period. Data recorded as NA has been excluded from this plot.


```r
actdataPI<-actdata %>%
     group_by(interval) %>%
     summarise(intave=mean(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot(actdataPI$interval, actdataPI$intave, type="l", 
  xlab = "5 minute interval over a 24 hour period", 
  ylab = "Average number of steps", 
  main = "Figure 2 - Steps averaged over 5 minute intervals")
```

![](PA1_template_files/figure-html/plot 2-1.png)<!-- -->



```r
 maxint<-filter(actdataPI, intave>200)
  maxint
```

```
## # A tibble: 1 x 2
##   interval intave
##      <int>  <dbl>
## 1      835   206.
```

There is a clear peak in the data around the 800 interval, 
more precisely interval 835, 206 steps.

### Inputting missing values



```r
nas<-sum(is.na(actdata$steps))
pnas<-nrow(actdata[!complete.cases(actdata), ])/nrow(actdata)*100
```
 
There are a total of 2304 NA values (or missing step readings), 
which amounts to 13.11% of the data. 


The NA values are replaced by the interval average that was calculated in the 
previous step in order to see if there is an impact on the mean and median of 
the data


```r
actdataM<-merge(actdata, actdataPI, by="interval")
actdataAve<-transform(actdataM, Nsteps = ifelse(is.na(steps), intave, steps))
```
Figure 3 below shows the histogram with missing values replaced with interval 
averages


```r
actdataAvePD<-actdataAve %>%
     group_by(date) %>%
     summarise(totsteps=sum(Nsteps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(actdataAvePD$totsteps, xlab="Total steps per day", main="Figure 3 - 
       Histogram of total steps taken each day over a two month period 
     (missing values replaced)")
```

![](PA1_template_files/figure-html/plot 3-1.png)<!-- -->


And the recalculated mean and median are given in the summary below:


```r
t2<-xtable(summary(actdataAvePD))
print(t2, type="html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Wed Sep 16 13:26:35 2020 -->
<table border=1>
<tr> <th>  </th> <th>      date </th> <th>    totsteps </th>  </tr>
  <tr> <td align="right"> X </td> <td> Min.   :2012-10-01   </td> <td> Min.   :   41   </td> </tr>
  <tr> <td align="right"> X.1 </td> <td> 1st Qu.:2012-10-16   </td> <td> 1st Qu.: 9819   </td> </tr>
  <tr> <td align="right"> X.2 </td> <td> Median :2012-10-31   </td> <td> Median :10766   </td> </tr>
  <tr> <td align="right"> X.3 </td> <td> Mean   :2012-10-31   </td> <td> Mean   :10766   </td> </tr>
  <tr> <td align="right"> X.4 </td> <td> 3rd Qu.:2012-11-15   </td> <td> 3rd Qu.:12811   </td> </tr>
  <tr> <td align="right"> X.5 </td> <td> Max.   :2012-11-30   </td> <td> Max.   :21194   </td> </tr>
   </table>



In order to see if the missing values have had an impact on the data the 
previous
summary information which includes the NA values is given below:

```r
print(t1, type="html")
```

<!-- html table generated in R 4.0.2 by xtable 1.8-4 package -->
<!-- Wed Sep 16 13:26:35 2020 -->
<table border=1>
<tr> <th>  </th> <th>      date </th> <th>    totsteps </th>  </tr>
  <tr> <td align="right"> X </td> <td> Min.   :2012-10-01   </td> <td> Min.   :   41   </td> </tr>
  <tr> <td align="right"> X.1 </td> <td> 1st Qu.:2012-10-16   </td> <td> 1st Qu.: 8841   </td> </tr>
  <tr> <td align="right"> X.2 </td> <td> Median :2012-10-31   </td> <td> Median :10765   </td> </tr>
  <tr> <td align="right"> X.3 </td> <td> Mean   :2012-10-31   </td> <td> Mean   :10766   </td> </tr>
  <tr> <td align="right"> X.4 </td> <td> 3rd Qu.:2012-11-15   </td> <td> 3rd Qu.:13294   </td> </tr>
  <tr> <td align="right"> X.5 </td> <td> Max.   :2012-11-30   </td> <td> Max.   :21194   </td> </tr>
  <tr> <td align="right"> X.6 </td> <td>  </td> <td> NA's   :8   </td> </tr>
   </table>



The mean and median values are very similar between the two datasets which may 
indicate that the NA values have almost no impact on the data, or that the
method chosen to replace the NA values was suitable.

### Differences in activity patterns between week days and weekends

The dataset with that has had the NA values replaced was used in this analysis.
The data was labelled according to the day of week it was, which was then
simplified to either "weekday" or "weekend".

```r
actdataAve$day<-weekdays(actdataAve$date, abbreviate=TRUE)
actdataAve$W<-ifelse(actdataAve$day %in% c("Mon","Tue","Wed","Thu","Fri"), 
                     "Week", "Weekend")
```

This data was subsetted according to which category it fell into in order 
to access any differences in activity between the week days and the weekends. 

The results are shown in Figure 4

```r
actdataWeek<-subset(actdataAve, W=="Week")
actdataWeekend<-subset(actdataAve, W=="Weekend")
actdataWeekA<-actdataWeek %>%
     group_by(interval) %>%
     summarise(intave=mean(Nsteps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
actdataWeekendA<-actdataWeekend %>%
     group_by(interval) %>%
     summarise(intave=mean(Nsteps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
par(mfrow=c(2,1), mar = c(4, 2, 1, 2), oma=c(0, 0, 1, 0))
plot5<-plot(actdataWeekA$interval, actdataWeekA$intave, type="l", main= 
          "Figure 4a - Week activity data", xlab="", ylab="Average steps",
            ylim=c(0,225))
plot6<-plot(actdataWeekendA$interval, actdataWeekendA$intave, type="l", 
            main="Figure 4b - Weekend activity data", xlab="5 minute interval", 
            ylab="Average steps", ylim=c(0,225))
```

![](PA1_template_files/figure-html/plot 4-1.png)<!-- -->




From this plot it appears the activity over the weekend is spread across more of
the intervals when compared to the week day data which appears to spike 
occasionally. The activity also starts later in the day on the weekend. 


