

require(tidyverse)
require(lubridate)  ## for today()
require(Hmisc) ## for stat_summary

########################
### Get Original file###
########################

dataFileName <- paste0(dataDirectory, "Analogy2_50Ps_9.16.25.csv") 
dateExpr <- "\\d{1,2}.\\d{1,2}.\\d{4}"  ## super simple, easy to be wrong so check
dataDate <- str_match(dataFileName, dateExpr)
cat("This data was finished collected on", dataDate, fill=TRUE)



Analogy2.df <- SafeReadCSV(dataFileName)


########################
### Wonky Participants #
########################

ParticipantsToRemove <- NULL
ParticipantsToRemove <- c(ParticipantsToRemove, 9, 16, 23, 47)  ## attention check


if (!is.null(ParticipantsToRemove)) {
    cat("** Removing participant(s):  ")
    cat(ParticipantsToRemove, fill=TRUE)
}

Analogy2.df <- RemoveParticipants(Analogy2.df, ParticipantsToRemove, report=TRUE)






