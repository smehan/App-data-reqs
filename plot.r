###########################################################
###    Plot.r is a class that reads in a cleaned csv from source
###    data and performs some processing. It then creates 
###    some plots mainly slicing on Assignee, Calendar duration,
###    created year and month (which are calculated values)
###########################################################
library(ggplot2)
library(scales)
library(ggthemes)
library(XML)
library(plyr)
library(stringr)
library(reshape2)
library(lubridate)

###########################################################
### Read CSV into a df. Assumes that records are single rows
### and that file has been cleansed with a :%s/^V^M/\r/g
### Perform some pre-processing tasks.
###########################################################

# First need to clean up Approver Names as it is such a long list, and needs to be
# added to as.is in read.csv2

approverNames <- "Preston Allen Start     Preston Allen End       Donna Amos Start        Donna Amos End  Alison Beug Start       Alison Beug End Martin Bragg Start      Martin Bragg End        Kacey Chun Start        Kacey Chun End  Anthony Colvard Start   Anthony Colvard End     Margie Coolidge Start   Margie Coolidge End     Beth Gallagher Start    Beth Gallagher End      Kimi Ikeda Start        Kimi Ikeda End  Al Liddicoat Start      Al Liddicoat End
        John Lyons Start        John Lyons End  Nelda Olvera Start      Nelda Olvera End        Jim Maraviglia Start    Jim Maraviglia End      Barbara Martinez Start  Barbara Martinez End    Theresa May Start       Theresa May End Craig Nelson Start      Craig Nelson End        Dave Ross Start Dave Ross End   Lori Serna Start        Lori Serna End  Mary Shaffer Start      Mary Shaffer End        Sharif Sharifi Start    Sharif Sharifi End      Craig Schultz Start     Craig Schultz  End      Patricia Stoneman Start Patricia Stoneman End   Mike Suess Start        Mike Suess End  Cem Sunata Start        Cem Sunata End  Terry Vahey Start       Terry Vahey End Joanne Williams Start   Joanne Williams End     June Serjeant Start     June Serjeant End       Lorlie Leetham Start    Lorlie Leetham End      Eumi Sprague Start      Eumi Sprague End        Trey Duffy Start        Trey Duffy End  Shannon Stephens Start  Shannon Stephens End    Susan Tripp Start       Susan Tripp End Stacey Breitenbach Start        Stacey Breitenbach End  Susan Sparling Start    Susan Sparling End      Sema Alptekin Start     Sema Alptekin End       Victor Brancart Start   Victor Brancart End     Philip Davis Start      Philip Davis End        Ryan Matteson Start     Ryan Matteson End       Melinda Rojo Start      Melinda Rojo End        Joyce Haratani Start    Joyce Haratani End      Marc Benadiba Start     Marc Benadiba End       Carter-Hammett McGarry Start    Carter-Hammett McGarry End      Joanne Mead Start       Joanne Mead End Cassie Carter Start     Cassie Carter End       Denise Gibbons Start    Denise Gibbons End
"
approverNames <- str_replace_all(approverNames, "(Start\\b)\\s+(\\b\\w+)", "\\1, \\2")
approverNames <- str_replace_all(approverNames, "(End\\b)\\s+(\\b\\w+)", "\\1, \\2")
approverNamesVec <- unlist(str_split(approverNames, ", "))

# now lets build the as.is list of columns including the approverNames

myData <- read.csv2("data/AppDataRequest2010-2015-clean.tsv", header=TRUE, sep = "\t", stringsAsFactors = TRUE,
                    as.is = c("Summary", "Description"))
rownames(myData) <- myData$Key

# Convert empty values to NA
myData[myData == ""] <- NA
myData[myData == " "] <- NA # there are still some rows ending with an extra space


# Convert dates to posix date objects
myData$Created <- mdy_hm(myData$Created)
myData$Updated <- mdy_hm(myData$Updated)
myData$Resolved <- mdy_hm(myData$Resolved)
myData$Date.of.First.Response <- mdy_hm(myData$Date.of.First.Response)
myData$Due.Date <- mdy(myData$Due.Date)

# need indicies for approver Columns and then convert them to date objects
approverCols <- grep('\\.Start$|\\.End$', names(myData))
myData[approverCols] <- lapply(myData[approverCols], mdy)


# Reduce vector noise by removing noise text
myData$Summary <- str_replace(myData$Summary, "^App Data Request - (.*)", "\\1")

###########################################################
### Create calculated values
###########################################################

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

myData$project_duration <- round((myData$Resolved - myData$Created), 3)

# First stab at approval duration
duration = ""

for(i in trycols){
    duration[i-62] <- (myData[i+1] - myData[i])
}

myData$approval_duration

###########################################################
### Finally, serialize the data frame for use in other scripts
###########################################################

saveRDS(myData, "data/App_Data_Reqs.rds")
###########################################################
### Create some plots
###########################################################

ggplot(myData) +
    aes(x=month_created, y=X..of.Sub.Tasks) +
    geom_point() +
    facet_wrap(~year_created) +
    theme_stata() +
    ggtitle("Subtasks for each project created") +
    labs(x="Project Creation Month", y="Number of subtasks") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(axis.title.y = element_text(vjust=0.5)) +
    theme(axis.title.x = element_text(vjust=-0.1)) +
    theme(plot.title = element_text(size=20, face="bold", vjust=2))

# change the level attributes for plot purposes
# this is kind of clunky but I haven't found a better way to do this on the fly.
levels(x=myData$Assignee)<-c("DW", "ER", "MR", "TMAY", "NONE")
ggplot(myData) +
    aes(myData$X..of.Sub.Tasks) +
    geom_histogram(fill="firebrick") +
    facet_grid(Assignee~year_created) +
    theme_stata() +
    ggtitle("Subtasks per assignee per year created") +
    labs(x="Subtasks", y="Frequency") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(axis.title.y = element_text(vjust=0.5)) +
    theme(axis.title.x = element_text(vjust=-0.1)) +
    theme(plot.title = element_text(size=20, face="bold", vjust=2)) +
    theme(strip.text.y = element_text(colour = "red", angle = 45, size = 10,
                                      hjust = 0.5, vjust = 0.5))
