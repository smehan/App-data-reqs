###########################################################
### Creates a paretoDFa frame of counts for Duration.AT from
### approver.R paretoDFa and makes a pareto plot of durations
### and times.
###########################################################

library(ggplot2)
library(plyr)

# First read in paretoDFa set
paretoDF <- readRDS(file="data/Approvals.rds")

# Subset only durations
paretoDF <- paretoDF[c("Key", "Duration.AT")]
paretoDF <- count(paretoDF, vars = "Duration.AT")

# rewrite Duration with factors and levels
paretoDF$Duration.AT <- factor(paretoDF$Duration.AT, levels=paretoDF$Duration.AT)

# Change colnames to more useful labels
colnames(paretoDF)[2] <- "Count"

# pareto.chart(approverDF$Duration.AT, ylab="Something")

# reorder on decreasing count
# this creates a bad order for cumsum so don't execute it
# paretoDF <- paretoDF[order(paretoDF$Count, decreasing=TRUE), ]

# Create cumsums of counts in new column
paretoDF$cum <- cumsum(paretoDF$Count)

# output the DF
paretoDF

###########################################################
### Build some plots
###########################################################
ggplot(paretoDF, aes(x=Duration.AT)) +
    geom_bar(aes(y=Count), fill="blue", stat="identity") +
    geom_point(aes(y=cum)) +
    geom_path(aes(y=cum, group=1)) +
    ggtitle("Pareto Chart of Approval Cycle Times") +
    labs(x="Duration (days)", y="Counts")    
