library(stringr)

MakeLine <- function() {
    cat("*********************************", fill=TRUE)
}

SummarizeParticipants <- function(df, ShowVideos=FALSE, ShowItems=FALSE) {

    NumberP <- length(unique(df$Subject))
    # NumberV <- length(unique(df$Video))
    NumberC <- length(unique(df$Condition))
    

    # AllVideos <- unique(df$Video)
    AllItems <- unique(df$Question)

    P.df <- df %>%
        group_by(Subject, Condition) %>%
        slice_head(n=1) 

    ExperimentTime <- mean(P.df$ExperimentMinutesDuration)
    ParticipantAge <- mean(P.df$Age, na.rm = TRUE)
    age_sd <- sd(P.df$Age, na.rm = TRUE)
    ScreenTime <- mean(P.df$TimeOnScreen)
    ## CondCount1 <- str_count(P.df$Condition,"PAPO")
    ## CondCount2 <- str_count(P.df$Condition,"UAPO")
    ## CondCount3 <- str_count(P.df$Condition,"PAUO")
    

    MakeLine()
    cat("Number of participants: ", NumberP, fill=TRUE)
    cat("Average age of participants", ParticipantAge, fill=TRUE)
    cat("SD of participant ages", age_sd, fill=TRUE)
    # cat("Number of videos: ", NumberV, fill=TRUE)
    cat("Average length of experiment (min)", ExperimentTime, fill=TRUE)
    cat("Average length of trial (s)", ScreenTime, fill=TRUE)
  ### this approach works great!  But if we have different conditions
  ### it will need to be changed.  See the table approach below, which
  ### should generalize to different conditions.
    ## cat("Num of Ps in PAPO", sum(CondCount1), fill=TRUE)
    ## cat("Num of Ps in UAPO", sum(CondCount2), fill=TRUE)
    ## cat("Num of Ps in PAUO", sum(CondCount3), fill=TRUE)

  MakeLine()
  cat("Number of Ps in each condition:", fill=TRUE)
  print(table(P.df$Condition))
  
  # MakeLine()
  # cat("Number of Ps asked each Q:", fill=TRUE)
  # print(table(P.df$Question))

    if (ShowVideos) {
        MakeLine()
        cat("Videos:", fill=TRUE)
        print(AllVideos)
    }

    if (ShowItems) {
        MakeLine()
        cat("Items:", fill=TRUE)
        print(AllItems)
    }

    MakeLine()
    cat("Reported gender:", fill=TRUE)
    print(table(P.df$Gender))
    MakeLine()
    cat("Reported language:", fill=TRUE)
    print(table(P.df$Language))
    MakeLine()
    cat("Reported ethnicity:", fill=TRUE)
    print(table(P.df$Race))
    MakeLine()
    cat("Reported education:", fill=TRUE)
    print(table(P.df$Education))
    MakeLine()
    return(P.df)


}
