
datafitbit<-read.csv("C:\\Users\\Peter\\Desktop\\RepData_PeerAssessment1\\Data\\activity.csv")
summary(datafitbit)
##preprocess the data create a second dataset with No NA's
datafitbitnonas<-datafitbit
head(datafitbit)
##what we need to do is sum all the steps by day and then calculate the average by day
##
library(dplyr)
library(magrittr)
##
datastepsbydate<-datafitbit %>%
    group_by(date) %>%
      summarise( sum_steps = sum(steps)) %>%
        arrange(sum_steps) %>% as.data.frame()
##now calculate the average number of steps per day

avgdatastepsbydate<-mean(datastepsbydate$sum_steps,na.rm=TRUE) 
##lets look at the distribution
?boxplot
boxplot(datastepsbydate$sum_steps,main="Distribution of steps by Day")

