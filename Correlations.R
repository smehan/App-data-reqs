###########################################################
### Correlations.R is a class to make correlation measurements
### including correlations between various Approvers' times and
### Duration.AT. Adds codes for whether approvals were subject
### to questions after submission.
### Works off of dataset already cleansed from Approver.R
###########################################################

library(dplyr)
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
left_join(approvalsDF, questionCodes)

