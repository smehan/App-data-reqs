########################################################
# Perform Multiple Regression Analysis on noTVCoded
# Also create Box Plot Series on codes
########################################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(qcc)

# Load in the data

#Need to read in noTVCoded from disk
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
    geom_hline(yintercept=14, color="red") +
    coord_flip()

ggplot(noTVCoded) +
    aes(x = ITS, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "lightskyblue1", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="ITS Type Request?",y="Duration Days") +
    geom_hline(yintercept=14, color="red") +
    coord_flip()

ggplot(noTVCoded) +
    aes(x = Complex, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "darkorange", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="Complex Type Request?",y="Duration Days") +
    geom_hline(yintercept=14, color="red") +
    coord_flip()

ggplot(noTVCoded) +
    aes(x = Population, y = Duration.AT) +
    geom_boxplot(notch = TRUE, fill = "plum4", na.rm = TRUE) +
    ggtitle("All Application Data Requests") +
    labs(x="Population Type Request?",y="Duration Days") +
    geom_hline(yintercept=14, color="red") +
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
    ##scale_fill_gradient2(low=muted("red"), mid="white", high="steelblue",
    scale_fill_gradient2(low = "red", mid = "white", high = "steelblue",
        guide=guide_colorbar(ticks = FALSE, barheight = 12),
        limits=c(-1,1)) +
    theme_minimal() +
    labs(x=NULL, y=NULL) +
    ggtitle("Correlation Heat Map of Approval Times\n Requests by Type")

###  create csv file for exporting
write.csv(colcorsOrdered, file = 'App Data Correlations by Type.csv')
#####  End of csv creation

##########################################################
### End correlation heatmap
##########################################################

#########################################################
### 2-way interaction plots
########################################################

interaction.plot(noTVCoded$Security, noTVCoded$QCode, noTVCoded$Duration.AT,
                 type='b', col=c("red","blue"), legend=F,
                 lty=c(1,2), lwd=2, pch=c(18,24),
                 xlab="Security and QCode Factors",
                 ylab="Mean of Approval Duration",
                 main="Interaction Plot")
            legend("bottomright", c("Security","QCode"), bty="n",lty=c(1,2),
                   lwd=2,pch=c(18,24), col=c("red","blue"), title="Legend Title", inset = .02)
 
interaction.plot(noTVCoded$Security,noTVCoded$Complex, noTVCoded$Modification,
                 noTVCoded$Population,noTVCoded$ITS, col=c("red","blue"), lty=1, lwd=2,
                 trace.label = "Request Type", xlab="factors", ylab="Mean Duration")            
                       
interaction.plot(noTVCoded$Security, noTVCoded$Modification, noTVCoded$Duration.AT)
interaction.plot(noTVCoded$Security, noTVCoded$Population, noTVCoded$Duration.AT)
interaction.plot(noTVCoded$Security, noTVCoded$ITS, noTVCoded$Duration.AT)

#########################################################
### 2-way anova
########################################################

model1a <- lm(Duration.AT ~ Security + Complex + Modification + Population + ITS + 
                  Security*Complex + Security*Modification + Security*Population + Security*ITS +
                  Complex*Modification + Complex*Population + Complex*ITS +
                  Modification*Population + Modification*ITS +
                  Population*ITS,
              noTVCoded)


anova(model1a)

##########################################################
# Multiple Regression Model 1 with all predictor variables
##########################################################

model1 <- lm(Duration.AT ~ Security + Complex + Modification + Population + ITS, noTVCoded)

summary(model1)
coef(model1)

library(coefplot)
coefplot(model1)

#######  End Model 1

##########################################################
# Multiple Regression Model 2 with Security and Complex 
#  predictor variables only
##########################################################

model2 <- lm(Duration.AT ~ Security + Complex, noTVCoded)

summary(model2)
coef(model2)

library(coefplot)
coefplot(model2)

#######  End Model 2

#####  Set noTVCoded to only incude security requests
SecOnly <- subset(CodesOnly, Security == 1)

###  Get the mean for the entire population and for security requests only
mean(CodesOnly$Duration.AT)
mean(SecOnly$Duration.AT)

### T-test will compare the means of the entire population of requests and
### the mean of only security requests
### Ho: Mean of entire population = Mean of security requests
### H1: Mean of entire population <> Mean of security requests
### Significance Level = .05

# assumes variations between pools are different
t.test(CodesOnly$Duration.AT ~ CodesOnly$Security) 

### Create boxplot - security requests vs all requests
boxplot(CodesOnly$Duration.AT ~ CodesOnly$Security, 
        main = "Application Data Requests",
        xlab = "Security Request - 0 = no and 1 = yes",
        ylab = "Approval Cycle Time/Days",
        col = "lightblue")

var(CodesOnly$Duration.AT)
var(CodesOnly$Duration.AT[CodesOnly$Security == 1], na.rm = TRUE)
var(CodesOnly$Duration.AT[CodesOnly$Security == 0], na.rm = TRUE)

##############################################
## Create the xbar and r charts
##############################################

q <- qcc(SecOnly, type="R", nsigmas=3)
q2 <- qcc(SecOnly, type="xbar", nsigmas=3)
###process.capability(q2, spec.limits=c(0,13.98297))
process.capability(q2, spec.limits=c(-6.432969,13.98297))

# Take the column of durations as a data frame. N samples with with 1 measurement each
# Now run an ImR as an individuals chart (xbar.one) against each 1 size sample
q2 <- qcc(CodesOnly$Duration.AT, type="xbar.one", nsigmas=3)
# Now run a process capability on the ImR data object
process.capability(q2, spec.limits=c(-27.67967,57.05846))

# try the same thing with NoTV
q2 <- qcc(noTVCoded$Duration.AT, type="xbar.one", nsigmas=3)
# Now run a process capability on the ImR data object
# With UCL moved to 0 to remove non-physical measurements
process.capability(q2, spec.limits=c(-27.67967,57.05846))

### Create histogram 
ggplot(noTVCoded) +
    aes(Duration.AT) +
       geom_histogram(fill="darkblue") +
    ggtitle("Histogram Plot Application Data Requests") +
    labs(x="Duration", y="Count")

########################################################################
### Plot the Requests and Color the Data Points Based on Questions y/n
########################################################################

ggplot(noTVCoded) +
    aes(x=Key, y=Duration.AT) +
    ##geom_point(aes(color=factor(noTV$QCode))) +
    geom_hline(yintercept=mean(noTVCoded$Duration.AT), color="red") +
    geom_hline(yintercept=mean(noTVCoded$Duration.AT)+2.66*sd(noTVCoded$Duration.AT), linetype=2, color="Navy Blue") +
    geom_hline(yintercept=mean(noTVCoded$Duration.AT)-2.66*sd(noTVCoded$Duration.AT), linetype=2, color="Navy Blue") +
    theme_stata() +
    ggtitle("Max Durations - Questions/No Questions") +
    ###    labs(x="Requests", y="Max Duration (days)") +http://127.0.0.1:28569/graphics/plot_zoom_png?width=921&height=643
    labs(x="Requests", y="Max Duration (days)") +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(strip.text.x = element_text(colour = "red", angle = 45, size = 10, hjust = 0.5, vjust = 0.5)) +
    theme(axis.title.y = element_text(vjust=1.0)) +
    theme(axis.title.x = element_text(vjust=-0.1)) +
    theme(plot.title = element_text(size=20, face="bold", vjust=2))


