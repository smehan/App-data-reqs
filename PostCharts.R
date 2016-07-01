###########################################################
### PostCharts.R is a class to make cojmparisons
### on pre and post AppData changes in respect of XMR.
### Uses old UCL,LCL from pre-appdata dataset.
###########################################################
library(qcc)
library(ggplot2)
library(lubridate)

# assemble the main df
PostDataDF <- read.csv2("data/AppDataPostDuration.csv", 
                        header=TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

# Using qcc to get a simple XmR
# Get the vector of observations in which are interested

# Create the individuals chart and qcc object
my.xmr.x <- qcc(my.xmr.raw, type = "xbar.one", plot = T)

# Create a process capability analysis of the xbar.one
##process.capability(my.xmr.x, spec.limits = c(-30.471,52.2711))
process.capability(my.xmr.x, spec.limits = c(0,52.2711), target = 14)
process.capability(my.xmr.x, spec.limits = c(0,52.2711))

# Create an ImR object for process capability calculation.
nq <- qcc(PostDataDF$Duration.AT, type="xbar.one", nsigmas=3)
process.capability(nq, spec.limits=c(-28.98913,51.38913), target = 14)

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

