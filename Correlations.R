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
library(stringr)

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
# now remove the text coded column
approvalsQuestCoded <- approvalsQuestCoded[c(-53)]

### Following is for building out an approval time factor correlation heatmap
# this will remove all of the rows with a duration (and hence participation) for TV
noTV <- approvalsQuestCoded[is.na(approvalsQuestCoded$TV.AT),]
# this is getting used more, so going to serialize it to disk for other modules to access
saveRDS(noTV, "data/noTV.rds")

#################################################
### Working with correlation matrix
#################################################
# Original attempt uses a subset of the total dataframe, removing the columns that
# have no place in the final correlation matrix.
# Uses pairwise complete because of sparse nature of combinations,
# and spearman for same reason.
# Calculate the correlation matrix, then melt data and pass to appropriate correlogram
corout <- cor(approvalsQuestCoded[,7:53], use = "pairwise.complete", method = "spearman")
corout <- melt(data = corout, varnames = c("x", "y"), value.name = "Correlations")

# Create subsets for Questions and NoQuestions
# in an effort to separate these two dimensions cleanly
Questions <- subset(noTV, QCode == 1)
NoQuestions <- subset(noTV, QCode == 0)
# subset removes rows that don't meet the second param as a test, so splitting into two subsets.
### modified to only include those with questions (Qcode == 1) from noTV
### Following is for building out an approval time factor correlation heatmap with QuestionsCoded

# now repeat the correlation matrix
corout <- cor(Questions[,7:53], use = "pairwise.complete", method = "spearman")

# but this method leaves all of the extra columns in and makes it hard to interpret.
# therefore, need to reduce the data through elimination of the columns with all NA
# that can not contribute to the correlation calculation.
# start over with sets into correlation matrix.
ReducedQuestions <- Questions[, colSums(is.na(Questions))<nrow(Questions)]
# now proceed with corout, melt, etc.
corout <- cor(ReducedQuestions, use = "pairwise.complete", method = "spearman")
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
    ggtitle("Correlation Heat Map of Approval Times\n Requests with Questions")


##########################################################

### SSAT vs MaxDurationAT group by were there questions
ggplot(approvalsQuestDF) +
    aes(x = SS.AT, y = Duration.AT, color = Questions) +
    geom_point(na.rm = TRUE)


p1 <- ggplot(approvalsQuestDF) +
    aes(x = approvalsQuestDF[,25], y = Duration.AT, color = Questions) +
    geom_point()

p2 <- ggplot(approvalsQuestDF) +
    aes(x = approvalsQuestDF[,24], y = Duration.AT, color = Questions) +
    geom_point()

p3 <- ggplot(approvalsQuestDF) +
    aes(x = approvalsQuestDF[,23], y = Duration.AT, color = Questions) +
    geom_point()

p4 <- ggplot(approvalsQuestDF) +
    aes(x = approvalsQuestDF[,22], y = Duration.AT, color = Questions) +
    geom_point()

### http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
multiplot(p1, p2, p3, p4, cols = 2)

######  Create a csv showing only those items that have correlation data
######  remove rows with 'na'
CorOnly <- corout[!is.na(corout$Correlations),]
###  create csv file for exporting
write.csv(CorOnly, file = 'AppDataCorrelations-Questions.csv')
#####  End of csv creation

