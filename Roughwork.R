library(Kmisc)
##install.packages("moments")
library(moments)
library(tibble)
library(dtplyr)
library(dplyr)
library(ggplot2)
library(magrittr)
install.packages("RColorBrewer")
library(RColorBrewer)

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

