########################################################
# Perform Multiple Regression Analysis on noTVCoded
# Also create Box Plot Series on codes
########################################################
library(ggplot2)

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
    
    

# Correlation Matrix - Heat Map


# Multiple Regression Model


