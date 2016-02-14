###########################################################
### Approver.R is a class to make operations on Approver Data
### including durations, numbers of approvers, etc.
### Works off of dataset already cleansed from Load.R
###########################################################
library(qcc)
library(ggplot2)

# First read in data set
myData <- readRDS(file="data/App_Data_Reqs.rds")

# if we need to identify and remove Key dups
dups <- anyDuplicated(approverDF$Key)
approverDF <- approverDF[-dups, ]

# CONSTANT used to convert time deltas to days
SECINDAY <- (60*60*24)

# Subset out everything not an approval task
## TODO haven't taken care of three bad rows of times yet!, so they are lost next function

myData <- myData[grep("Get Approval", myData$Summary, perl = TRUE), ]

# Create the approver DF as a subset of myData
approverDF <- data.frame(Key = factor(myData$Key),
                 Creator = myData$Creator,
                 Assignee = myData$Assignee,
                 Created = myData$Created,
                 Resolved = myData$Resolved,
                 # Issue.Type = myData$Issue.Type,
                 Date.of.First.Response = myData$Date.of.First.Response,
                 PA.AT = as.integer(myData$Preston.Allen.End - myData$Preston.Allen.Start)/(SECINDAY),
                 DA.AT = as.integer(myData$Donna.Amos.End - myData$Donna.Amos.Start)/(SECINDAY),
                 AB.AT = as.integer(myData$Alison.Beug.End - myData$Alison.Beug.Start)/(SECINDAY),
                 MB.AT = as.integer(myData$Martin.Bragg.End - myData$Martin.Bragg.End)/(SECINDAY),
                 KC.AT = as.integer(myData$Kacey.Chun.End - myData$Kacey.Chun.Start)/(SECINDAY),
                 AC.AT = as.integer(myData$Anthony.Colvard.End - myData$Anthony.Colvard.Start)/(SECINDAY),
                 MC.AT = as.integer(myData$Margie.Coolidge.End - myData$Margie.Coolidge.Start)/(SECINDAY),
                 BG.AT = as.integer(myData$Beth.Gallagher.End - myData$Beth.Gallagher.Start)/(SECINDAY),
                 KI.AT = as.integer(myData$Kimi.Ikeda.End - myData$Kimi.Ikeda.Start)/(SECINDAY),
                 AL.AT = as.integer(myData$Al.Liddicoat.End - myData$Al.Liddicoat.Start)/(SECINDAY),
                 JL.AT = as.integer(myData$John.Lyons.End - myData$John.Lyons.Start)/(SECINDAY),
                 NO.AT = as.integer(myData$Nelda.Olvera.End - myData$Nelda.Olvera.Start)/(SECINDAY),
                 JM.AT = as.integer(myData$Jim.Maraviglia.End - myData$Jim.Maraviglia.Start)/(SECINDAY),
                 BM.AT = as.integer(myData$Barbara.Martinez.End - myData$Barbara.Martinez.Start)/(SECINDAY),
                 TM.AT = as.integer(myData$Theresa.May.End - myData$Theresa.May.Start)/(SECINDAY),
                 CN.AT = as.integer(myData$Craig.Nelson.End - myData$Craig.Nelson.Start)/(SECINDAY),
                 DR.AT = as.integer(myData$Dave.Ross.End - myData$Dave.Ross.Start)/(SECINDAY),
                 LS.AT = as.integer(myData$Lori.Serna.End - myData$Lori.Serna.Start)/(SECINDAY),
                 MS.AT = as.integer(myData$Mary.Shaffer.End - myData$Mary.Shaffer.Start)/(SECINDAY),
                 SS.AT = as.integer(myData$Sharif.Sharifi.End - myData$Sharif.Sharifi.Start)/(SECINDAY),
                 CS.AT = as.integer(myData$Craig.Schultz..End - myData$Craig.Schultz.Start)/(SECINDAY),
                 PS.AT = as.integer(myData$Patricia.Stoneman.End - myData$Patricia.Stoneman.Start)/(SECINDAY),
                 SUESS.AT = as.integer(myData$Mike.Suess.End - myData$Mike.Suess.Start)/(SECINDAY),
                 CEM.AT = as.integer(myData$Cem.Sunata.End - myData$Cem.Sunata.Start)/(SECINDAY),
                 TV.AT = as.integer(myData$Terry.Vahey.End - myData$Terry.Vahey.Start)/(SECINDAY),
                 JW.AT = as.integer(myData$Joanne.Williams.End - myData$Joanne.Williams.Start)/(SECINDAY),
                 JS.AT = as.integer(myData$June.Serjeant.End - myData$June.Serjeant.Start)/(SECINDAY),
                 LL.AT = as.integer(myData$Lorlie.Leetham.End - myData$Lorlie.Leetham.Start)/(SECINDAY),
                 ES.AT = as.integer(myData$Eumi.Sprague.End - myData$Eumi.Sprague.Start)/(SECINDAY),
                 TD.AT = as.integer(myData$Trey.Duffy.End - myData$Trey.Duffy.Start)/(SECINDAY),
                 SSTE.AT = as.integer(myData$Shannon.Stephens.End - myData$Shannon.Stephens.Start)/(SECINDAY),
                 ST.AT = as.integer(myData$Susan.Tripp.End - myData$Susan.Tripp.Start)/(SECINDAY),
                 SB.AT = as.integer(myData$Stacey.Breitenbach.End - myData$Stacey.Breitenbach.Start)/(SECINDAY),
                 SSPA.AT = as.integer(myData$Susan.Sparling.End - myData$Susan.Sparling.Start)/(SECINDAY),
                 SA.AT = as.integer(myData$Sema.Alptekin.End - myData$Sema.Alptekin.Start)/(SECINDAY),
                 VB.AT = as.integer(myData$Victor.Brancart.End - myData$Victor.Brancart.Start)/(SECINDAY),
                 PD.AT = as.integer(myData$Philip.Davis.End - myData$Philip.Davis.Start)/(SECINDAY),
                 RM.AT = as.integer(myData$Ryan.Matteson.End - myData$Ryan.Matteson.Start)/(SECINDAY),
                 MR.AT = as.integer(myData$Melinda.Rojo.End - myData$Melinda.Rojo.Start)/(SECINDAY),
                 JH.AT = as.integer(myData$Joyce.Haratani.End - myData$Joyce.Haratani.Start)/(SECINDAY),
                 MBEN.AT = as.integer(myData$Marc.Benadiba.End - myData$Marc.Benadiba.Start)/(SECINDAY),
                 CHM.AT = as.integer(myData$Carter.Hammett.McGarry.End - myData$Carter.Hammett.McGarry.Start)/(SECINDAY),
                 JMEA.AT = as.integer(myData$Joanne.Mead.End - myData$Joanne.Mead.Start)/(SECINDAY),
                 CCA.AT = as.integer(myData$Cassie.Carter.End - myData$Cassie.Carter.Start)/(SECINDAY),
                 DG.AT = as.integer(myData$Denise.Gibbons.End - myData$Denise.Gibbons.Start)/(SECINDAY))


start <- myData[990,51]
if (is.na(start)){
    start <- ymd("2015-12-25")
}
tmp <- ymd("2015-12-25")
for (i in 53:100){
    thisDate <- myData[990,i]
    if (!is.na(thisDate)){
        tmp <- thisDate
    }
    if (tmp < start){
        start <- tmp
    }
}



# TODO Calculate the longest interval and call that the approval duration
# TODO Remove timestamps hhmm
# Remove duplicates (3 of) and add all times into the one row
dups <- anyDuplicated(approverDF$Key)

while (dups != 0){
    target <- which(approverDF$Key == approverDF$Key[dups])
    for (i in 7:ncol(approverDF)){
        approverDF[target, i] <- (approverDF[target, i] + approverDF[dups, i])
    }
    approverDF <- approverDF[-dups, ]
    dups <- anyDuplicated(approverDF$Key)
}

# Now take same form and clean up small and 0 days intervals.
for (r in 1:nrow(approverDF)){
    for (i in 7:ncol(approverDF)){
        if (!is.na(approverDF[r,i]) && approverDF[r,i] == 0){
            approverDF[r,i] <- 0.5
        } else if (!is.na(approverDF[r,i]) && approverDF[r,i] < 0.1){
            approverDF[r,i] <- 0.1
        } 
    } 
}

# Retrieve max of the ATs
approverDF$Duration.AT <- apply(approverDF[sapply(approverDF,is.numeric)],1,max,na.rm=TRUE)

# Now clean up -Inf in Duration.AT
# TODO
# This simply removes the -Inf, but still need to decide what to do with these
# missing values.
approverDF <- approverDF[-approverDF$Duration.AT < 0, ]

### Finally, serialize the data frame for use in other scripts
###########################################################

saveRDS(approverDF, "data/Approvals.rds")
##########################################################
### Add some plots
##########################################################
ggplot(approverDF) +
    aes(x=Key, y=as.numeric(project_duration)) +
    geom_point(aes(color=factor(Assignee))) +
    facet_wrap(~year_created) +
    ggtitle("Project duration by \nmonth project created by year created")


ggplot(approverDF) +
    aes(x=Key, y=Duration.AT) +
    geom_point(aes(color=factor(Assignee))) +
    geom_hline(yintercept=mean(approverDF$Duration.AT), color="red") +
    geom_hline(yintercept=mean(approverDF$Duration.AT)+2.66*sd(approverDF$Duration.AT), linetype=2, color="Navy Blue") +
    geom_hline(yintercept=mean(approverDF$Duration.AT)-2.66*sd(approverDF$Duration.AT), linetype=2, color="Navy Blue") +
    theme_stata() +
    ggtitle("Approval Duration of \nApp Data Requests") +
    labs(x="Requests", y="Duration (days)") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(strip.text.x = element_text(colour = "red", angle = 45, size = 10, hjust = 0.5, vjust = 0.5)) +
    theme(axis.title.y = element_text(vjust=1.0)) +
    theme(axis.title.x = element_text(vjust=-0.1)) +
    theme(plot.title = element_text(size=20, face="bold", vjust=2))
    
# Using qcc to get a simple XmR
# Get the vector of observations in which are interested
my.xmr.raw <- approverDF$Duration.AT

# Create the individuals chart and qcc object
my.xmr.x <- qcc(my.xmr.raw, type = "xbar.one", plot = T)

# Create a process capability analysis of the xbar.one
process.capability(my.xmr.x, spec.limits = c(-25.02,53.69))

# Create a moving range chart as a qcc object. This takes a 2-col matrix that is used
# to calculate the moving range.
my.xmr.raw.r  <- matrix(cbind(my.xmr.raw[1:length(my.xmr.raw)-1],
                              my.xmr.raw[2:length(my.xmr.raw)]),
                        ncol = 2)
# Make the XmR plot
my.xmr.mr <- qcc(my.xmr.raw.r, type="R", plot = T, 
                 add.stats = T,
                 title = "Approval Process XmR Chart",
                 xlab = "Approval Tasks",
                 ylab = "Duration (days)",
                 axes.las = 0)

# And now identify which observations are violating runs and which are out of control
beyond.limits(my.xmr.mr, limits = my.xmr.mr$limits)
violating.runs(my.xmr.mr, run.length = qcc.options("run.length"))

# density plot for the approval time
ggplot(approverDF, aes(x=approverDF$Duration.AT)) +
    geom_density(na.rm=T) +
    geom_vline(xintercept=mean(approverDF$Duration.AT), color="red") +
    ggtitle("Density Plot for Approval Process Duration Times") +
    labs(x="Duration (days)", y="Kernel Density")


