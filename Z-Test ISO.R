library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)

##### Do Requests that Involve ISO Cause Delay in Approvals?
##### Is the Mean Higher?
###  Only keep the rows that involve ISO from noTV
ISOonly <- noTV[!is.na(noTV$SS.AT),]
###  Display the Mean of Approval Time 
mean (ISOonly[["Duration.AT"]], na.rm = TRUE)
#####  End Display of Mean for Requests that Involve ISO

### Create new column that contains difference of max duration and SS (ISO) duration
ISOonly$Diff <- ISOonly$Duration.AT - ISOonly$SS.AT

### Calculate the mean for differences <= 2 and mean for differences > 2
WaitISOMean <- ISOonly$Diff[ISOonly$Diff <= 2]
WaitISOMean <- mean (WaitISOMean)
NoWaitISOMean <- ISOonly$Diff[ISOonly$Diff > 2]
NoWaitISOMean <- mean (NoWaitISOMean)
WaitISOMean
NoWaitISOMean

### Count the differences 
length(ISOonly$Diff)
length(which(ISOonly$Diff <= 2))
length(which(ISOonly$Diff > 2))

### Create histogram of all Differences
### If ISO duration = max duration for task then difference will be 0
ggplot(ISOonly) +
    aes(Diff) +
    geom_histogram(breaks=seq(0, 40, by =2),
                   col="red",
                   aes(fill=..count..)) +
    ggtitle("Differences Histogram Plot") + 
    labs(x="Differences between ISO and Max Durations", y="Frequency")

ggplot(ISOonly) +
    aes(x=seq(1,length(Diff)), y=Diff) +
    geom_point() +
    theme_stata() +
    ggtitle("Differences Scatter Plot") + 
    labs(x="Differences between ISO and Max Durations", y="Frequency")


ggplot(ISOonly) +
    aes(Diff) +
    geom_dotplot(breaks=seq(0, 40, by =2),
                 col="red",
                 aes(fill=..count..)) +
    theme_stata() +
    ggtitle("Differences Scatter Plot") + 
    labs(x="Differences between ISO and Max Durations", y="Frequency")


ggplot(ISOonly) +
    aes(x=seq(1,length(Diff)), y=Diff) +
    geom_jitter()

### Perform Hypothesis Test (Z-Test)
# zstat = xbar - mu / std/sqrt(n)
# xbar is mean of ISO diffs
# mu is mean of all diffs

# need to find ISO diffs and all diffs
alldiffs <- ISOonly$Diff
mu <- mean(alldiffs)
pop_sd <- sd(alldiffs)
pop_n <- length(alldiffs)

xbar <- mean(WaitISOMean)

# now calculate the z-stat
z <- xbar - mu / (pop_sd / sqrt(pop_n))

# what is the probability that we get that value? (in a normal distribution....)
# p-value
pnorm(z)

### Create subset of Differences <=2 and > 2
ISOWait <- ISOonly$Diff[ISOonly$Diff <= 2]
ISONoWait <- ISOonly$Diff[ISOonly$Diff > 2]

### T-test will compare the means of the two samples:  Waiting for ISO and Not Waiting for ISO
t.test(ISOWait, ISONoWait)

