###########################################################
### Fishbone.R is a class to make fishbone diagrams.
### It leverages the qcc package.
###########################################################

library(qcc)

# This is the constructor for the fishbone diagram. There are two sets of inputs.
# Causes have major and minor branches. Effects has a single value.

cause.and.effect(cause=list(Measurements=c("Fred", "Dan", "Sam"),
                            Materials=c("Alloys", "Lubricants", "Suppliers"),
                            Personnel=c("Shifts", "Supervisors", "Training", "Operators"), 
                            Environment=c("Condensation", "Moisture"), 
                            Methods=c("Brake", "Engager", "Angle"),
                            Machines=c("Speed", "Lathes", "Bits", "Sockets")),
                 effect="Surface Flaws")
