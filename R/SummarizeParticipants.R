library(stringr)

MakeLine <- function() {
    cat("*********************************", fill=TRUE)
}

SummarizeParticipants <- function(df, ShowVideos=FALSE, ShowItems=FALSE) {

    NumberP <- length(unique(df$subjID))
    NumberC <- length(unique(df$condition))
    


    P.df <- df %>%
        group_by(subjID) %>% #, condition
        slice_head(n=1) 

    ParticipantAge <- mean(P.df$age, na.rm = TRUE)
    age_sd <- sd(P.df$age, na.rm = TRUE)

    

    MakeLine()
    cat("Number of participants: ", NumberP, fill=TRUE)
    cat("Average age of participants", ParticipantAge, fill=TRUE)
    cat("SD of participant ages", age_sd, fill=TRUE)


  MakeLine()
  cat("Number of Ps in each condition:", fill=TRUE)
  print(table(P.df$condition))
  

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
    print(table(P.df$sex))
    MakeLine()
    cat("Reported language:", fill=TRUE)
    print(table(P.df$language))
    MakeLine()
    cat("Reported ethnicity:", fill=TRUE)
    print(table(P.df$race))
    MakeLine()
    cat("Reported education:", fill=TRUE)
    print(table(P.df$education))
    MakeLine()
    return(P.df)


}
