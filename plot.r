***********************************************************
    Plot.r is a class that reads in a cleaned csv from source
    data and performs some pre-processing. It then creates 
    some plots mainly slicing on Assignee, Calendar duration,
    created year and month (which are calculated values)
***********************************************************
library(ggplot2)
library(scales)
library(ggthemes)
library(XML)
library(plyr)
library(stringr)
library(reshape2)
library(lubridate)

***********************************************************
*** Read in CSV into a df. Assumes that records are single rows
*** and that file has been cleansed with a :%s/^V^M/\r/g
*** Perform some pre-processing tasks.
***********************************************************
    
myData <- read.csv2("data/AppDataReq2010-2014.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE)
rownames(myData) <- myData$Key

# Convert empty values to NA
myData[myData == ""] <- NA
myData[myData == " "] <- NA # there are still some rows ending with an extra space

# Convert dates to posix date objects
myData$Created <- mdy_hm(myData$Created)
myData$Updated <- mdy_hm(myData$Updated)
myData$Resolved <- mdy_hm(myData$Resolved)
myData$Date.of.First.Response <- mdy_hm(myData$Date.of.First.Response)

# Reduce vector noise by removing noise text
myData$Summary <- str_replace(myData$Summary, "^App Data Request - (.*)", "\\1")

***********************************************************
*** Create calculated values
***********************************************************

# Year request created into year and month, with a numerical and text month

myData$year_created <- year(myData$Created)
myData$month_created <- month(myData$Created, label = TRUE)
myData$month_num_created <- month(myData$Created)
myData$year_updated <- year(myData$Updated)
myData$month_updated <- month(myData$Updated, label = TRUE)
myData$month_num_updated <- month(myData$Updated)
myData$year_resolved <- year(myData$Resolved)
myData$month_resolved <- month(myData$Resolved, label = TRUE)
myData$month_num_resolved <- month(myData$Resolved)

# Calendar duration of request

myData$project_duration <- (myData$Resolved - myData$Created)

***********************************************************
*** Create some plots
***********************************************************

ggplot(myData) +
    aes(x=month_created, y=X..of.Sub.Tasks) +
    geom_point() +
    facet_wrap(~year_created) +
    theme_stata() +
    ggtitle("Subtasks for each project created") +
    labs(x="Project Creation Month", y="Number of subtasks") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(axis.title.y = element_text(vjust=0.5)) +
    theme(axis.title.x = element_text(vjust=-0.1))