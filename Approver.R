###########################################################
### Approver.R is a class to make operations on Approver Data
### including durations, numbers of approvers, etc.
### Works off of dataset already cleansed from Load.R
###########################################################

# First read in data set
myData <- readRDS(file="data/App_Data_Reqs.rds")

# some scratch
# trial <- data.frame(id = factor(1:4), 
#                     A1 = c(1, 2, 1, 2), 
#                     A2 = c(2, 1, 2, 1),
#                     B1 = c(3, 3, 3, 3))
# trialm <- melt(trial)

# if we need to identify and remove Key dups
dups <- anyDuplicated(approverDF$Key)
approverDF <- approverDF[-dups, ]

# Create the approver DF as a subset of myData
approverDF <- data.frame(Key = factor(myData$Key),
                 Project = myData$Project,
                 Creator = myData$Creator,
                 Assignee = myData$Assignee)

# convert to long form
approverDF <- melt(approverDF, 
                   id.vars = c("Key"),
                   measured.vars = c("Project, Creator, Assignee"),
                   na.rm = TRUE,
                   variable.name = "stuff",
                   value.name = "values",
                   factorsAsStrings = T)


