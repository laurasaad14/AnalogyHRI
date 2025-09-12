########################
### Experiment setup ###
########################

experimentName <- "Analogy1"
whoami <- Sys.info()[["user"]]
if (whoami == "trafton") {
    workingDirectory <- "~/Documents/graphics/SenseOfAgency/"
} else if (whoami == "saad-admin") {
  workingDirectory <- "~/AnalogyHRI/"
}

source(paste0(workingDirectory, "R/helper.R"))
source(paste0(workingDirectory, "R/SummarizeParticipants.R"))
graphSaveDirectory <- paste0(workingDirectory, "graphs/", experimentName, "/")
dataDirectory <- paste0(workingDirectory, "data/processed/", experimentName, "/")
setwd(workingDirectory)
VerifyPathIsSafe(graphSaveDirectory)
VerifyPathIsSafe(dataDirectory)

source(paste0(workingDirectory, "R/GetData", experimentName, ".R"))


#############################
## every new set go through #
#############################

### completed at <FIXME> ran!
IgnorePrevious <- TRUE  ## FALSE
if (IgnorePrevious) {
    MaxSubject <- 0 ## 

    Analogy1.df <- Analogy1.df %>%
        filter(subjID > MaxSubject)

    # Analogy1.wide <- Analogy1.wide %>% # data already in wide format
    #     filter(Subject > MaxSubject)

}
showSummary <- TRUE  ## FALSE 
showAttentionCheck <- TRUE ## FALSE

####################################################################################
## Prints out trial summaries (not needed in SoA) and final experiment summaries  ##
## I find that our attention check captures P who do not do task and is defensible #
####################################################################################

if (showSummary) {

    df.summaryVideo <- Analogy1.df %>%
        group_by(subjID) %>%
        slice_head(n=1) %>%
        dplyr::select(subjID, language, feedback)

#     for (S in unique(df.summaryVideo$Subject)) {
#         df.participant <- df.summaryVideo %>% filter(Subject == S)
#         cat("Participant =", S, "(Language:", unique(df.participant$Language), ")", fill=TRUE)
# ### not needed because no instance based feedback in SoA1
#         ## for (v in df.participant$Video) {
#         ##     cat("Video:", v, ": ")
#         ##     cat(as.character(subset(df.participant, Video == v)$VideoFeedback), fill=TRUE)
#         ## }
#         cat("ExperimentFeedback: ")
#         cat(unique(as.character(df.participant$ExperimentFeedback)), fill=TRUE)
#         cat(fill=TRUE)
#     }
}

####################
## Attention check #
####################

if (showAttentionCheck) {
    AttentionCheck.df <- Analogy1.df %>%
        group_by(subjID) %>%
        dplyr::select(subjID, 'attn_check_missed')  
    Problems.df <- subset(AttentionCheck.df, attn_check_missed > 0)
    if (nrow(Problems.df) > 0)
        print(as.data.frame(subset(AttentionCheck.df, attn_check_missed > 0)))
    else {
        cat("All participants passed all attention checks!", fill=TRUE)
    }
}


SummarizedInfo.df <- SummarizeParticipants(Analogy1.df)

## I run this script, first checking for attention check problems
## Then I look at summaries to do a quick check about what P think about the experiment.
## If there is something very very odd there, I look carefully but don't do much if they pass attention check 
## I remove Ps who miss attention check 
## To remove Ps I put them in the ParticipantsToRemove in GetData*.R


