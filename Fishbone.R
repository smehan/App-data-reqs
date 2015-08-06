###########################################################
### Fishbone.R is a class to make fishbone diagrams.
### It leverages the qcc package.
###########################################################

library(qcc)

# This is the constructor for the fishbone diagram. There are two sets of inputs.
# Causes have major and minor branches. Effects has a single value.

cause.and.effect(cause=list(Technology=c("Revised Form Confusing",
                                         "Sits in email inbox\n if approver out",
                                         "manual reminders",
                                         "form outdated",
                                         "third party"),
                            Policy=c("Confusion of authority\n to enforce policy",
                                     "Who can say no",
                                     "Lose control of data\n once released"),
                            People=c("Don't understand single\n source of truth",
                                     "Turnover of requestors",
                                     "No training",
                                     "Don't understand data levels",
                                     "Clarification of roles\n and responsibilities",
                                     "Requestor doesn't know\n what to ask for"), 
                            Process=c("Don't understand what\n requestor trying to do",
                                      "Unlcear on why form received",
                                      "Level of data requested (sensitive)",
                                      "Director level approval",
                                      "Why population is necessary",
                                      "Unknown priority",
                                      "No direct tie to XXXX process",
                                      "Can't see big pic of data use / need",
                                      "No single place to go to start process")), 
                 effect="Delay in Approvals")
