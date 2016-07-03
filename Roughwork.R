library(Kmisc)
##install.packages("moments")
library(moments)
library(tibble)
library(dtplyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)
##install.packages("RColorBrewer")
library(RColorBrewer)
library(cowplot)

datafitbit<-read.csv("C:\\Users\\Peter\\Desktop\\RepData_PeerAssessment1\\Data\\activity.csv")
summary(datafitbit)
##preprocess the data create a second dataset with No NA's
datafitbitnonas<-datafitbit
head(datafitbit)
##what we need to do is sum all the steps by day and then calculate the average by day
##

##
datastepsbydate<-datafitbit %>%
    group_by(date) %>%
      summarise( sum_steps = sum(steps)) %>%
        arrange(sum_steps) %>% as.data.frame()
##now calculate the average number of steps per day

avgdatastepsbydate<-mean(datastepsbydate$sum_steps,na.rm=TRUE) 
mediandatastepsbydate <-median(datastepsbydate$sum_steps,na.rm=TRUE) 
standard.deviation.datastepsbydate <-sd(datastepsbydate$sum_steps,na.rm=TRUE) 
##lets look at the distribution
hist(datastepsbydate$sum_steps,main="Total number of steps \n taken per day",xlab="Sum of steps per day",ylab="Frequency")

qplot(datastepsbydate$sum_steps,
      geom="histogram",
      main = "Total number of steps \n taken per day",
      binwidth=5000,
      xlab="Sum of steps",
      ylab="Frequency"
      )

##alternatively look at the boxplot

boxplot(datastepsbydate$sum_steps,main="Distribution of steps by Day")



###
###### What is the average daily activity pattern?
###
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##and the average number of steps taken, averaged across all days (y-axis)

datafitbit.avg.daily.activity<-datafitbit %>%
  group_by(interval) %>%
  summarise( mean_steps = mean(steps,na.rm=TRUE)) %>%
  arrange(interval) %>% as.data.frame()

ggplot(datafitbit.avg.daily.activity, aes(x=interval, y=mean_steps)) +
  geom_line(color = "blue")+
  ggtitle("time series plot of the 5-minute interval (x-axis) \n and the average number of steps taken, \n averaged across all days (y-axis)")+
  xlab("interval")+
  ylab("mean number of \n steps across all days")


f0<-max(datafitbit.avg.daily.activity$mean_steps)

##where does the max average occur

datafitbit.avg.daily.activity[which(datafitbit.avg.daily.activity$mean_steps==eval(f0)),]



##


###Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as ????????). The presence of missing days may introduce bias into some calculations or summaries of the data.
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)

##one way of doing this is the summary function
summary(datafitbit)
##
##using dplyr

datafitbit %>%
    filter(is.na(steps)|is.na(date)|is.na(interval)) %>%
         nrow()
  
#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.

?replace
head(datafitbit)

datafitbit.per.interval.activity<-datafitbit %>%
  group_by(interval) %>%
  summarise( mean_steps_per_interval = mean(steps,na.rm=TRUE)) %>%
  arrange(interval) %>% as.data.frame()

##now merge on data to original dataset

datafitbit.with.daily.averages<-dplyr::left_join(datafitbit,datafitbit.per.interval.activity,by="interval")

##now replace with mean value where blank

datafitbit.with.daily.averages
head(datafitbit.with.daily.averages)

dplyr::coalesce(as.double(datafitbit.with.daily.averages$steps),datafitbit.with.daily.averages$mean_steps_per_interval)
##test works

dplyr::coalesce(as.double(datafitbit.with.daily.averages$steps),datafitbit.with.daily.averages$mean_steps_per_interval)

datafitbit.with.daily.averages$steps<-dplyr::coalesce(as.double(datafitbit.with.daily.averages$steps),datafitbit.with.daily.averages$mean_steps_per_interval)

#Make a histogram of the total number of steps taken each day 



head(datafitbit.with.daily.averages)


datastepswithimputedmissingdatebydate <- datafitbit.with.daily.averages %>%
  group_by(date) %>%
    summarise( sum_steps = sum(steps)) %>%
        arrange(sum_steps) %>% as.data.frame()


hist(datastepswithimputedmissingdatebydate$sum_steps, main="Total number of steps \n taken per day \n missing values imputed", xlab="Sum of steps per day",ylab="Frequency")

qplot(datastepswithimputedmissingdatebydate$sum_steps,
      geom="histogram",
      main = "Total number of steps \n taken per day \n with missing data imputed",
      binwidth=5000,
      xlab="Sum of steps",
      ylab="Frequency"
)

##and Calculate and report the mean and median total number of steps taken per day. 

avg.datasteps.bydate.withimputedmissingdata<-mean(datastepswithimputedmissingdatebydate$sum_steps,na.rm=TRUE) 
median.datasteps.bydate.withimputedmissingdata <-median(datastepswithimputedmissingdatebydate$sum_steps,na.rm=TRUE) 


##Do these values differ from the estimates from the first part of the assignment? 
library(cowplot)

ap<-qplot(datastepsbydate$sum_steps,
          geom="histogram",
          main = "Total number of steps \n taken per day",
          binwidth=5000,
          xlab="Sum of steps",
          ylab="Frequency"
)
bp<-qplot(datastepswithimputedmissingdatebydate$sum_steps,
          geom="histogram",
          main = "Total number of steps \n taken per day \n with missing data imputed",
          binwidth=5000,
          xlab="Sum of steps",
          ylab="Frequency"
)
  
plot_grid(ap, bp, ncol = 2, nrow = 1)

##
####
##


abxpwithmiss<-ggplot(datastepswithimputedmissingdatebydate, aes(factor(0),sum_steps))+geom_boxplot()+
  ggtitle("Total number of steps \n taken per day \n with missing data imputed")+
    xlab("")+
      ylab("Total number of steps taken per day")
  
abxpw<-ggplot(datastepsbydate, aes(factor(0),sum_steps))+geom_boxplot()+
  ggtitle("Total number of steps \n taken per day \n no missing data imputed")+
  xlab("")+
  ylab("Total number of steps taken per day")


plot_grid(abxpw, abxpwithmiss, ncol = 2, nrow = 1)

##What is the impact of imputing missing data on the estimates of the total daily number of steps?


library(moments)
summary(datastepsbydate)
?skewness

##calculates as Pearson's Kurtosis coefficient

skewness(datastepsbydate$sum_steps,na.rm=T)
kurtosis(datastepsbydate$sum_steps,na.rm=T)

skewness(datastepswithimputedmissingdatebydate$sum_steps,na.rm=T) ##no need to do this they (NA's) should be removed
kurtosis(datastepswithimputedmissingdatebydate$sum_steps,na.rm=T) ##no need to do this they  (NA's)  should be removed

##

##get weekday
##calculate mean by day of week and interval 
##create a ggplot heatmap

library(lubridate)
head(datafitbit)
datafitbit['Week_Day']<-lubridate::wday(datafitbit$date,label=T)

## we will use the dataset with out the imputed data
##inspired by https://rpubs.com/daattali/heatmapsGgplotVsLattice
datafitbit.per.interval.activity.by.weekday<-datafitbit %>%
  group_by(interval,Week_Day) %>%
  summarise( mean_steps_per_interval = mean(steps,na.rm=TRUE)) %>%
  arrange(Week_Day,interval) %>% as.data.frame()

##head(datafitbit.per.interval.activity.by.weekday)


jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
jBuPuPalette <- jBuPuFun(paletteSize)

ggplot(datafitbit.per.interval.activity.by.weekday, aes(x = interval, y = Week_Day, fill = mean_steps_per_interval)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  ggtitle("Heatmap of mean number of steps \n by time and day of Week")+
  xlab("Time of day \n 24 hour clock")+
  ylab("Day") +
  scale_fill_gradient2(low = jBuPuPalette[1],
                       mid = jBuPuPalette[paletteSize/2],
                       high = jBuPuPalette[paletteSize],
                       midpoint = (max(datafitbit.per.interval.activity.by.weekday$mean_steps_per_interval) + min(datafitbit.per.interval.activity.by.weekday$mean_steps_per_interval)) / 2,
                       name = "Mean steps per interval \n per day of week")

colnames(datafitbit)
as.character(datafitbit$Week_Day)

datafitbit$Week_day_Week_End <- sapply(datafitbit$Week_Day, function(x) switch(as.character(x),
                                                             "Sun" = "Weekend",
                                                             "Sat" = "Weekend",
                                                             "Weekday"
))

datafitbit.per.interval.activity.by.weekday.weekend<-datafitbit %>%
  group_by(interval,Week_day_Week_End) %>%
  summarise( mean_steps_per_interval = mean(steps,na.rm=TRUE)) %>%
  arrange(Week_day_Week_End,interval) %>% as.data.frame()


ggplot(datafitbit.per.interval.activity.by.weekday.weekend, aes(x = interval, y = Week_day_Week_End, fill = mean_steps_per_interval)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_tile() +
  ggtitle("Heatmap of mean number of steps \n by time and day of Week type")+
  xlab("Time of day \n 24 hour clock")+
  ylab("Day") +
  scale_fill_gradient2(low = jBuPuPalette[1],
                       mid = jBuPuPalette[paletteSize/2],
                       high = jBuPuPalette[paletteSize],
                       midpoint = (max(datafitbit.per.interval.activity.by.weekday$mean_steps_per_interval) + min(datafitbit.per.interval.activity.by.weekday$mean_steps_per_interval)) / 2,
                       name = "Mean steps per interval \n per day of week type")

head(datafitbit.per.interval.activity.by.weekday.weekend)

##pair plot

datafitbit['Week_Day']<-lubridate::wday(datafitbit$date,label=T)

## we will use the dataset with out the imputed data
##inspired by https://rpubs.com/daattali/heatmapsGgplotVsLattice

datafitbit$Week_day_Week_End <- sapply(datafitbit$Week_Day, function(x) switch(as.character(x),
                                                                               "Sun" = "Weekend",
                                                                               "Sat" = "Weekend",
                                                                               "Weekday"
))

datafitbit.per.interval.activity.by.weekday.weekend<-datafitbit %>%
  group_by(interval,Week_day_Week_End) %>%
  summarise( mean_steps_per_interval = mean(steps,na.rm=TRUE)) %>%
  arrange(Week_day_Week_End,interval) %>% as.data.frame()


datafitbit.per.interval.activity.by.weekday <- datafitbit.per.interval.activity.by.weekday.weekend %>% filter(Week_day_Week_End=="Weekday")
datafitbit.per.interval.activity.by.weekend <- datafitbit.per.interval.activity.by.weekday.weekend %>% filter(Week_day_Week_End=="Weekend")

###

gpwkday<-ggplot(datafitbit.per.interval.activity.by.weekday, aes(x=interval, y=mean_steps_per_interval)) +
  geom_line(color = "blue")+
  scale_y_continuous(limits = c(0, 300))+
  ggtitle("time series plot of the 5-minute interval (x-axis) \n and the average number of steps taken, \n averaged across all Weekdays (y-axis)")+
  xlab("interval")+
  ylab("mean number of \n steps across all weekdays")

gpwkend<-ggplot(datafitbit.per.interval.activity.by.weekend, aes(x=interval, y=mean_steps_per_interval)) +
  geom_line(color = "red")+
  scale_y_continuous(limits = c(0, 300))+
  ggtitle("time series plot of the 5-minute interval (x-axis) \n and the average number of steps taken, \n averaged across all Weekend days (y-axis)")+
  xlab("interval")+
  ylab("mean number of \n steps across all weekend days \n (Saturday,Sunday)")

plot_grid(gpwkday, gpwkend, ncol = 1, nrow = 2)

?ks.test

ks.test(datastepswithimputedmissingdatebydate$sum_steps, "pnorm", mean(datastepswithimputedmissingdatebydate$sum_steps), sd(datastepswithimputedmissingdatebydate$sum_steps)) ##with imputed missing values
##null hypothesis: the null hypothesis that the true distribution function of x is equal to, not less than or not greater than the hypothesized distribution function (one-sample case) or the distribution function of y (two-sample case), respectively
## the test is inconclusive thought we could not reject the null hypothesis at 0.05 level
##so lets perform a shapiro wilks test on our data
shapiro.test(datastepswithimputedmissingdatebydate$sum_steps)
##so from are test we reject the null the null hypothesis that the test data does not comes from a normal distribution
##and is infact from a normal distribution
##Finally we plot a qqnorm plot of our data, This is a plot 
qqnorm(datastepswithimputedmissingdatebydate$sum_steps, main="Quantile Quantile plot of \n number of step with imputed data")
## again the plot is fairly straight this would indicate normality
##The q-q plot provides a visual aid in comparing  the sample quantiles 
##to the corresponding theoretical quantiles.  
##if the points in a q-q plot deviate from a straight line, 
##then the assumed distribution is called into question 
##in our case this would be the normal distribution.

##############
####
##
####
#############
head(datastepsbydate)
ks.test(datastepsbydate$sum_steps, "pnorm", mean(datastepsbydate$sum_steps), sd(datastepsbydate$sum_steps)) ##with imputed missing values
##null hypothesis: the null hypothesis that the true distribution function of x is equal to, not less than or not greater than the hypothesized distribution function (one-sample case) or the distribution function of y (two-sample case), respectively
## the test is inconclusive thought we could not reject the null hypothesis at 0.05 level
##so lets perform a shapiro wilks test on our data
shapiro.test(datastepsbydate$sum_steps)
##so from are test we reject the null the null hypothesis that the test data does not comes from a normal distribution
##and is infact from a normal distribution
##Finally we plot a qqnorm plot of our data, This is a plot 
qqnorm(datastepsbydate$sum_steps, main="Quantile Quantile plot of \n number of step with missing data")
## again the plot is fairly straight this would indicate normality
##The q-q plot provides a visual aid in comparing  the sample quantiles 
##to the corresponding theoretical quantiles.  
##if the points in a q-q plot deviate from a straight line, 
##then the assumed distribution is called into question 
##in our case this would be the normal distribution.
