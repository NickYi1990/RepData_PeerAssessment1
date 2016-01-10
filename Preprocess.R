library(dplyr)


########################  Download and unzip data  ######################
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip") 
unzip("activity.zip")

########################  Read data in csv format  ######################
Activity_Pre <- read.csv("activity.csv")
summary(Activity_Pre)

##Conver character to date format and see if i have done the right conversion
Activity_Pre$date <- as.Date(Activity_Pre$date)
sapply(Activity_Pre, class)

####################### Make plots of two dimension: day interval  ######################
#plot-1 total steps in each day

MakePlot_1 <- function(dataset, suffix_char, ...){
  dataset <- group_by(dataset, date)
  assign(paste("steps_sum", suffix_char, sep = ""), summarise(dataset, sum(steps, na.rm = TRUE)), pos = ".GlobalEnv")
  barplot(get(paste("steps_sum", suffix_char, sep = ""))$`sum(steps, na.rm = TRUE)`, names.arg = get(paste("steps_sum", suffix_char, sep = ""))$date, ... )
  assign(paste("steps_mean", suffix_char, sep = ""),summarise(dataset, mean(steps, na.rm = TRUE)), pos = ".GlobalEnv" )
  assign(paste("steps_median", suffix_char, sep = ""),summarise(dataset, median(steps,na.rm = TRUE)), pos = ".GlobalEnv")
}
MakePlot_1(Activity_Pre, "")
#plot-2  average steps in each interval
Activity_Pre <- group_by(Activity_Pre, interval)
timeseries <- summarise(Activity_Pre, mean(steps, na.rm = T))
names(timeseries) <- c("interval", "stepsInInterval")
with(timeseries, plot(interval, stepsInInterval, type = 'l', xaxt = 'n')) ## [KP] cancel x axis 
axis(1, seq(min(timeseries$interval), max(timeseries$interval),   ## [KP] add x axis
            by = (max(timeseries$interval)-min(timeseries$interval))/15))

##Get the interval contains the maxim average steps
timeseries$interval[which(timeseries$stepsInInterval == max(timeseries$stepsInInterval))]

######################  Imputing missing values  ######################
##MNAR: missing not at random
##MCAR: missing completely at random

############Missing value exploration
if (!require(mice)) { ##[KP] package look missing values in number
  install.packages("mice")
  library(mice)
}
##Calculated and report total number of missing values in each column
md.pattern(Activity_Pre)


if(!require("VIM")) { ##[KP] package look missing values in picture
  install.packages("VIM")
  library(VIM)
}
aggr_plot <- aggr(Activity_Pre, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE
                  ,labels=names(data), cex.axis=.7, gap=3
                  ,ylab=c("Histogram of missing data","Pattern"))

############Impute missing value by interval
Activity_Impute <- Activity_Pre
interval2steps <- function(interval) {
  timeseries[timeseries$interval == interval, ]$steps
}
count = 0  # Count the number of data filled in
for (i in 1:nrow(Activity_Impute)) {
  if (is.na(Activity_Impute[i, ]$steps)) {
    Activity_Impute[i, ]$steps <- interval2steps(Activity_Impute[i, ]$interval)
    count = count + 1
  }
}
cat("Total ", count, "NA values were filled.\n\r")

## Timeseries plot between Impute and Original
par(mfrow = c(2,1))
MakePlot_1(Activity_Impute, "_Impute", main="Imputed Data", col='red')
MakePlot_1(Activity_Pre, "", main="Oringinal Dta", col='green')



## Weekday Weekends
Activity_Impute$date <- as.Date(Activity_Impute$date)

work_day <- c("星期一","星期二","星期三","星期四","星期五")
Activity_Impute_week <- transform(Activity_Impute, weekOrweeken = ifelse(weekdays(Activity_Impute$date) %in% work_day, "weekday", "weekend"))  

library(lattice)
names(airquality)
Activity_Impute_week <- transform(Activity_Impute_week, weekOrweeken = factor(weekOrweeken))
Activity_Impute_week <- group_by(Activity_Impute_week, interval, weekOrweeken)
groups(Activity_Impute_week)
Activity_Impute_week_mean <- mutate(Activity_Impute_week, step_means = mean(steps))
xyplot(step_means~interval | weekOrweeken, data = Activity_Impute_week_mean, type='l', layout = c(1,2),ylab = "Number of steps")


