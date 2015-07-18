###########################################################
### Models for durations
###########################################################

# First read in data set
readRDS(file="data/App_Data_Reqs.rds")

ggplot(myData) +
    aes(x=month_num_created, y=as.numeric(project_duration)) +
    geom_point(aes(color=factor(Assignee))) +
    facet_wrap(~year_created) +
    ggtitle("Project duration by \nmonth project created by year created")
