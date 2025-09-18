

require(tidyverse)
require(lubridate)  ## for today()
require(Hmisc) ## for stat_summary

########################
### Get Original file###
########################

dataFileName <- paste0(dataDirectory, "Analogy3_150Ps_9.16.25.csv") 
dateExpr <- "\\d{1,2}.\\d{1,2}.\\d{4}"  ## super simple, easy to be wrong so check
dataDate <- str_match(dataFileName, dateExpr)
cat("This data was finished collected on", dataDate, fill=TRUE)



Analogy3.df <- SafeReadCSV(dataFileName)


########################
### Wonky Participants #
########################

ParticipantsToRemove <- NULL
ParticipantsToRemove <- c(ParticipantsToRemove)  ## attention check


if (!is.null(ParticipantsToRemove)) {
    cat("** Removing participant(s):  ")
    cat(ParticipantsToRemove, fill=TRUE)
}

Analogy3.df <- RemoveParticipants(Analogy3.df, ParticipantsToRemove, report=TRUE)






