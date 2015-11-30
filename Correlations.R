###########################################################
### Correlations.R is a class to make correlation measurements
### including correlations between various Approvers' times and
### Duration.AT. Adds codes for whether approvals were subject
### to questions after submission.
### Works off of dataset already cleansed from Approver.R
###########################################################

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)

# First read in data set
approvalsDF <- readRDS(file="data/Approvals.rds")

#########
### Read in codes for question after approval submission
### Y means that there were subsequent questions after submission
### N means that there were no subsequent questions presented after submission
### of approval. NA is indeterminate
#########
questionCodes <- read.csv2("data/AppData-Questions-or-not.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)

# remove EOR.INDICATOR text column. 
# TODO questionCodes <- questionCodes[questionCodes$Summary]

# rename questions column
colnames(questionCodes)[3] <- "Questions"

# remove Summary text column.
keep <- c("Key", "Questions")
questionCodes <- questionCodes[keep]

# Reduce noise in Key column to match other Key
questionCodes$Key <- str_replace(questionCodes$Key, "DW-(.\\d*)", "\\1")

# Clean up Question Flags, including NA where none
questionCodes$Questions <- ifelse(questionCodes$Questions == "y", "Y",
                                  ifelse(questionCodes$Questions == "n", "N", NA))

# Join Questions onto Approvals
approvalsQuestDF <- left_join(approvalsDF, questionCodes)

# clean up
rm(questionCodes, keep)

# need to make the Question code a numeric map
approvalsQuestCoded <- approvalsQuestDF
approvalsQuestCoded$QCode <- ifelse(approvalsQuestDF$Questions == "Y", 1, 0)
approvalsQuestCoded <- approvalsQuestCoded[c(-33)]

### Following is for building out an approval time factor correlation heatmap
noTV <- approvalsQuestCoded[is.na(approvalsQuestCoded$TV.AT),]
### Following is for building out an approval time factor correlation heatmap with QuestionsCoded
corout <- cor(approvalsQuestCoded[,7:33], use = "pairwise.complete", method = "spearman")
corout <- melt(data = corout, varnames = c("x", "y"), value.name = "Correlations")

#now order the result for plotting
colcorsOrdered <- corout[order(corout$Correlations), ]

#now plot as a heat map
ggplot(colcorsOrdered) +
    aes(x=x, y=y) +
    geom_tile(aes(fill=Correlations)) +
    scale_fill_gradient2(low=muted("red"), mid="white", high="steelblue",
                         guide=guide_colorbar(ticks = FALSE, barheight = 12),
                         limits=c(-1,1)) +
    theme_minimal() +
    labs(x=NULL, y=NULL) +
    ggtitle("Correlation Heat Map of Approval Times")
