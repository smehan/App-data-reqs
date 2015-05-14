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
    
myData <- read.csv2("App-data-reqs.csv", header=TRUE, sep = ",")
myData <- rownames[myData$Key]

# Convert empty values to NA

# Convert dates to posix date objects

***********************************************************
*** Create calculated values
***********************************************************

# Year request created into year and month, with a numerical and text month

# Calendar duration of request
    
***********************************************************
*** Create some plots
***********************************************************