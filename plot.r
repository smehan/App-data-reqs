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
myData$Created <- dmy_hm(myData$Created)
myData$Updated <- dmy_hm(myData$Updated)
myData$Resolved <- dmy_hm(myData$Resolved)
myData$Date.of.First.Response <- dmy_hm(myData$Date.of.First.Response)

***********************************************************
*** Create calculated values
***********************************************************

# Year request created into year and month, with a numerical and text month

# Calendar duration of request
    
***********************************************************
*** Create some plots
***********************************************************