########################################################
# Perform Multiple Regression Analysis on noTVCoded
# Also create Box Plot Series on codes
########################################################
library(ggplot2)
library(reshape2)
library(dplyr)

# Load in the data

# Need to read in noTVCoded from disk
noTVCoded <- readRDS(file="data/noTVCoded.rds")

# Box Plot Series

# First create boxplots of 1 category each
ggplot(noTVCoded) +
    aes(x = Security, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "skyblue3", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="Security Type Request?",y="Duration Days") +
    geom_hline(yintercept=14, color="red") +
    coord_flip()

ggplot(noTVCoded) +
    aes(x = Modification, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "palegreen4", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="Modification Type Request?",y="Duration Days") +
    coord_flip()

ggplot(noTVCoded) +
    aes(x = ITS, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "coral3", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="ITS Type Request?",y="Duration Days") +
    coord_flip()

ggplot(noTVCoded) +
    aes(x = Complex, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "darkorange", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="Complex Type Request?",y="Duration Days") +
    coord_flip()

ggplot(noTVCoded) +
    aes(x = Population, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "plum4", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="Population Type Request?",y="Duration Days") +
    coord_flip()


# Create boxplots with 2 categories
ggplot(noTVCoded) +
    aes(x = Security, y = Duration.AT) +
    geom_boxplot(notch = TRUE, na.rm = TRUE,
                 aes(fill = Complex)) +
    ggtitle("All Application Data Requests") +
    labs(x="Security Type Request?",y="Duration Days") +
    geom_hline(yintercept=14, color="red") +
    scale_fill_manual(name = "Complex?", 
                      values = c("darkslategray", "lightyellow1")) +
    coord_flip()
    

#################################################
### Working with correlation matrix
#################################################

# Original attempt uses a subset of the total dataframe, removing the columns that
# have no place in the final correlation matrix.
# Uses pairwise complete because of sparse nature of combinations,
# and spearman for same reason.

# Change y/n to 1/0 integers to prepare for correlation matrix

CodesOnly <- noTVCoded[,52:59]

to_nums <- function(value){
    if (is.na(value)){
        return(value)
    } else if (value == "y"){
        return("1")
    } else if (value == "n")  {
        return("0")
    }
}

CodesOnly$Modification <- unlist(lapply(CodesOnly$Modification, function(s) to_nums(s)))
CodesOnly$Modification <- unlist(lapply(CodesOnly$Modification, as.integer))

CodesOnly$Complex <- unlist(lapply(CodesOnly$Complex, function(s) to_nums(s)))
CodesOnly$Complex <- unlist(lapply(CodesOnly$Complex, as.integer))

CodesOnly$Population <- unlist(lapply(CodesOnly$Population, function(s) to_nums(s)))
CodesOnly$Population <- unlist(lapply(CodesOnly$Population, as.integer))

CodesOnly$Security <- unlist(lapply(CodesOnly$Security, function(s) to_nums(s)))
CodesOnly$Security <- unlist(lapply(CodesOnly$Security, as.integer))

CodesOnly$ITS <- unlist(lapply(CodesOnly$ITS, function(s) to_nums(s)))
CodesOnly$ITS <- unlist(lapply(CodesOnly$ITS, as.integer))

CodesOnly$GenReq <- unlist(lapply(CodesOnly$GenReq, function(s) to_nums(s)))
CodesOnly$GenReq <- unlist(lapply(CodesOnly$GenReq, as.integer))

# Calculate the correlation matrix, then melt data and pass to appropriate correlogram
corout <- cor(CodesOnly, use = "pairwise.complete", method = "spearman")
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
    ggtitle("Correlation Heat Map of Approval Times\n Requests by Type")

##########################################################
### End correlation plot
##########################################################


# Multiple Regression Model

