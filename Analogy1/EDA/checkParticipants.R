########################
### Experiment setup ###
########################

experimentName <- "SoA1"
whoami <- Sys.info()[["user"]]
if (whoami == "trafton") {
    workingDirectory <- "~/Documents/graphics/SenseOfAgency/"
} else if (whoami == "saad-admin") {
  workingDirectory <- "~/SenseofAgency/"
}

source(paste0(workingDirectory, "R/helper.R"))
source(paste0(workingDirectory, "R/SummarizeParticipants.R"))
graphSaveDirectory <- paste0(workingDirectory, "graphs/", experimentName, "/")
dataDirectory <- paste0(workingDirectory, "data/raw/", experimentName, "/")
setwd(workingDirectory)
VerifyPathIsSafe(graphSaveDirectory)
VerifyPathIsSafe(dataDirectory)
UseSplitConditions <- FALSE  ## FALSE

source(paste0(workingDirectory, "R/GetData", experimentName, ".R"))


#############################
## every new set go through #
#############################

### completed at <FIXME> ran!
IgnorePrevious <- TRUE  ## FALSE
if (IgnorePrevious) {
    MaxSubject <- 0 ## 

    SoA1.df <- SoA1.df %>%
        filter(Subject > MaxSubject)

    SoA1.wide <- SoA1.wide %>%
        filter(Subject > MaxSubject)

}
showSummary <- TRUE  ## FALSE
showAttentionCheck <- TRUE ## FALSE
showSplitConditionInfo <- TRUE ## FALSE

####################################################################################
## Prints out trial summaries (not needed in SoA) and final experiment summaries  ##
## I find that our attention check captures P who do not do task and is defensible #
####################################################################################

if (showSummary) {

    df.summaryVideo <- SoA1.df %>%
        group_by(Subject) %>%
##        filter(Question=="experiences an inner life") %>%  ## only get 1 instance each
        slice_head(n=1) %>%
        dplyr::select(Subject, Language, ExperimentFeedback)

    for (S in unique(df.summaryVideo$Subject)) {
        df.participant <- df.summaryVideo %>% filter(Subject == S)
        cat("Participant =", S, "(Language:", unique(df.participant$Language), ")", fill=TRUE)
### not needed because no instance based feedback in SoA1
        ## for (v in df.participant$Video) {
        ##     cat("Video:", v, ": ")
        ##     cat(as.character(subset(df.participant, Video == v)$VideoFeedback), fill=TRUE)
        ## }
        cat("ExperimentFeedback: ")
        cat(unique(as.character(df.participant$ExperimentFeedback)), fill=TRUE)
        cat(fill=TRUE)
    }
}

####################
## Attention check #
####################

if (showAttentionCheck) {
    AttentionCheck.df <- SoA1.df %>%
        group_by(Subject) %>%
        slice_tail(n=1) %>%
        dplyr::select(Subject, 'TotalAttentionChecksMissed')  ## ask malcolm to keep consistent
    Problems.df <- subset(AttentionCheck.df, TotalAttentionChecksMissed > 0)
    if (nrow(Problems.df) > 0)
        print(as.data.frame(subset(AttentionCheck.df, TotalAttentionChecksMissed > 0)))
    else {
        cat("All participants passed all attention checks!", fill=TRUE)
    }
}

### show some details about whether all P in all conditions noticed manipulations
if (showSplitConditionInfo) {
  ### PAUO had the wrong item (not selected) go into the shopping cart
  ### if a P in the PAUO condition said "yes" we assume they did not notice
  ### so performance should be similar to PAPO
  ### we will call these PAUO-notNoticed when P did not notice item was not what they selected

  ### first check to see how often it happened and whether other conditions were OK

  cat(fill=TRUE)
  cat("resp to item chose in cart? PAPO should be all yes; UAPO should be all yes; PAUO should be all no", fill=TRUE)
  print(table(SoA1.wide$Condition, SoA1.wide$ItemInCart))

  ### UAPO put the item in the cart before the click happened
  ### if a P in the UAPO condition clicked on the cart, we assume they did not notice the item going into the cart without the click
  ### so performance should be similar to PAPO
  ### we will call these UAPO-notNoticed when P clicked the button (unnecessarily) 

  ### first check to see how often it happened and whether other conditions were OK
##   Only1Line <- SoA1.df %>%
##     group_by(Subject, Trial) %>%
##     slice_head(n=1)

##   cat(fill=TRUE)
##   cat("add to cart button pushes. PAPO should all be ~ 1; PAUO should all be ~1; UAPO should be all 0", fill=TRUE)
##   print(table(Only1Line$Condition, Only1Line$AddToCartButtonPushes))
}

SummarizedInfo.df <- SummarizeParticipants(SoA1.df)

## I run this script, first checking for attention check problems
## Then I look at summaries to do a quick check about what P think about the experiment.
## If there is something very very odd there, I look carefully but don't do much if they pass attention check 
## I remove Ps who miss attention check 
## To remove Ps I put them in the ParticipantsToRemove in GetData*.R


