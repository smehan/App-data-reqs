###########################################################
### Approver.R is a class to make operations on Approver Data
### including durations, numbers of approvers, etc.
### Works off of dataset already cleansed from Load.R
###########################################################

# First read in data set
readRDS(file="data/App_Data_Reqs.rds")

# some scratch
trial <- data.frame(id = factor(1:4), 
                    A1 = c(1, 2, 1, 2), 
                    A2 = c(2, 1, 2, 1),
                    B1 = c(3, 3, 3, 3))
trialm <- melt(trial)

df <- melt(myData, id.vars = c("Key"),
           measured.vars = c("Assignee"),
           na.rm = TRUE,
           factorsAsStrings = F)

