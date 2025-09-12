VerifyPathIsSafe <- function(path) {

  if (!file.exists(path))
    dir.create(path, recursive=T)

}

SafeReadCSV <- function(fileName) {
    if (file.exists(fileName)) {
        return(read.csv(fileName))
    } else {
        stop(paste0(fileName, " does not exist"))
    }
}

RemoveParticipants <- function(original.df, ParticipantsToRemove, report=TRUE) {

    OriginallyRunN <- length(unique(original.df$Subject))
    pruned.df <- original.df %>%
        filter(!Subject %in% ParticipantsToRemove)

    FinalN <- length(unique(pruned.df$Subject))

    if (report) {
        cat("Original n =", OriginallyRunN, fill=TRUE)
        cat("Final n =", FinalN, fill=TRUE)
        cat("(deleted", OriginallyRunN - FinalN, "[", round((OriginallyRunN - FinalN)/OriginallyRunN * 100, 2), "%]", "participants)", fill=TRUE)
    }
    return(pruned.df)

}

### Facets helper functions
### NumberRatersToUse can be 23 or less for trial version
### this is long format
### groupNum is for Group anchoring
### PrintOutFacetsInfo(df, 4, groupNum=1)
PrintOutFacetsInfoORIG <- function(df, NumberRatersToUse, groupNum=NULL, printLineNums=FALSE) {

    if (NumberRatersToUse >= length(unique(df$Subject)))
        RatersUsed <- unique(df$Subject)
    if (NumberRatersToUse < length(unique(df$Subject))) {
        RatersUsed <- sample(unique(df$Subject), size=NumberRatersToUse)
        df <- filter(df, Subject %in% RatersUsed)
    }

    cat("Raters Used (put in 'Raters' component)", fill=TRUE)
    RatersUsed <- unique(df$Subject)
    if (is.null(groupNum)) {
        for (Rater in sort(RatersUsed)) {
            cat(Rater, fill=TRUE)
        }
    } else {
        for (Rater in sort(RatersUsed)) {
            cat(Rater, "=", Rater, ",0,", groupNum, sep="", fill=TRUE)
            }
    }
    cat(fill=TRUE)
    cat("Items used (put in 'Items' component) [if more than 1 group should be identical]", fill=TRUE)
    for (itemNum in unique(df$QuestionNum)) {
        cat(itemNum, "=", sep="")
        cat(unique(filter(df, QuestionNum == itemNum)$Question), fill=TRUE)
    }
    cat(fill=TRUE)
    cat("Data Used (put in 'data' component)", fill=TRUE)
    for (Line in 1:(nrow(df))) {
        if (printLineNums)
            cat(Line, ":", sep="")
        cat(df$VideoNum[Line],",",
            df$Subject[Line],",",
            df$QuestionNum[Line],",",
            df$Response[Line], " ;; ", sep="")
        cat(df$Video[Line],",",
            "R", df$Subject[Line],",",
            df$Question[Line],",",
            df$Response[Line], fill=TRUE, sep="")
    }

}

PrintOutRaters <- function(df, groupNum=NULL, printInstructions=FALSE, DEBUG=FALSE) {
##     if (DEBUG) {
## ##        cat("NumberP:", NumberP, fill=TRUE)
##         cat("groupNum:", groupNum, fill=TRUE)
##     }
    if (printInstructions)
        cat("Raters Used (put in 'Raters' component)", fill=TRUE)
    RatersUsed <- unique(df$Subject)
    if (is.null(groupNum)) {
        for (Rater in sort(RatersUsed)) {
            cat(Rater, fill=TRUE)
        }
    } else {
        cat(";; group:", groupNum, fill=TRUE)
        for (Rater in sort(RatersUsed)) {
            cat(Rater, "=", Rater, ",0,", groupNum, sep="", fill=TRUE)
        }
    }
    cat(fill=TRUE)
}

PrintOutItems <- function(df, printInstructions=FALSE, DEBUG=FALSE) {
    if (DEBUG) {
##        cat("NumberI:", NumberI, fill=TRUE)
##        cat("groupNum:", groupNum, fill=TRUE)
    }
    if (printInstructions)
        cat("Items used (put in 'Items' component)", fill=TRUE)
    for (itemNum in unique(df$QuestionNum)) {
        cat(itemNum, "=", sep="")
        cat(unique(filter(df, QuestionNum == itemNum)$Question), fill=TRUE)
    }
    cat(fill=TRUE)
}

### stupidly, ended up using cat with a file name which was not a great decision
### works but ugly
PrintOutData <- function(df, outputFile=NULL, groupNum=NULL, printInstructions=FALSE, comments=TRUE, DEBUG=FALSE) {
    if (DEBUG) {
        ## cat("NumberQ:", NumberI, fill=TRUE)
        ## cat("groupNum:", groupNum, fill=TRUE)
    }
    if (printInstructions)
        if (is.null(outputFile))
            cat("Data Used (put in 'data' component)", fill=TRUE)
        else {
            cat("Data is in file", outputFile, fill=TRUE)
            cat("put 'data = fileName' in facets file.", outputFile, fill=TRUE)
        }
    if (!is.null(outputFile))
        cat(";; group =", groupNum, fill=TRUE, file=outputFile, append=TRUE)
    for (Line in 1:(nrow(df))) {
        if (DEBUG)
            if (is.null(outputFile))
                cat(Line, ":", sep="")
            else
                cat(Line, ":", sep="", file=outputFile, append=TRUE)
        if (is.null(outputFile))
            cat(df$VideoNum[Line],",",
                df$Subject[Line],",",
                df$QuestionNum[Line],",",
                df$Response[Line],
                sep="")
        else
            cat(df$VideoNum[Line],",",
                df$Subject[Line],",",
                df$QuestionNum[Line],",",
                df$Response[Line],
                sep="",file=outputFile, append=TRUE)
        if (comments)
            if (is.null(outputFile))
                cat(" ;; ",
                    df$Video[Line],",",
                    "R", df$Subject[Line],",",
                    df$Question[Line],",",
                    df$Response[Line], sep="")
            else
                cat(" ;; ",
                    df$Video[Line],",",
                    "R", df$Subject[Line],",",
                    df$Question[Line],",",
                    df$Response[Line], sep="", file=outputFile, append=TRUE)
        if (is.null(outputFile))
            cat(fill=TRUE)
        else
            cat(fill=TRUE, file=outputFile, append=TRUE)
    }

}

### Facets helper functions
### NumberRatersToUse can be 23 or less for trial version
### this is long format
### groupNum is for Group anchoring
### PrintOutFacetsInfo(df, 4, groupNum=1)
### refactor:  print out all participants but divide by
PrintOutFacetsInfo <- function(df, NumberRatersToUse, groupNum=NULL, printLineNums=FALSE) {

    if (NumberRatersToUse >= length(unique(df$Subject)))
        RatersUsed <- unique(df$Subject)
    if (NumberRatersToUse < length(unique(df$Subject))) {
        RatersUsed <- sample(unique(df$Subject), size=NumberRatersToUse)
        df <- filter(df, Subject %in% RatersUsed)
    }

    cat("Raters Used (put in 'Raters' component)", fill=TRUE)
    RatersUsed <- unique(df$Subject)
    if (is.null(groupNum)) {
        for (Rater in sort(RatersUsed)) {
            cat(Rater, fill=TRUE)
        }
    } else {
        for (Rater in sort(RatersUsed)) {
            cat(Rater, "=", Rater, ",0,", groupNum, sep="", fill=TRUE)
            }
    }
    cat(fill=TRUE)
    cat("Items used (put in 'Items' component) [if more than 1 group should be identical]", fill=TRUE)
    for (itemNum in unique(df$QuestionNum)) {
        cat(itemNum, "=", sep="")
        cat(unique(filter(df, QuestionNum == itemNum)$Question), fill=TRUE)
    }
    cat(fill=TRUE)
    cat("Data Used (put in 'data' component)", fill=TRUE)
    for (Line in 1:(nrow(df))) {
        if (printLineNums)
            cat(Line, ":", sep="")
        cat(df$VideoNum[Line],",",
            df$Subject[Line],",",
            df$QuestionNum[Line],",",
            df$Response[Line], " ;; ", sep="")
        cat(df$Video[Line],",",
            "R", df$Subject[Line],",",
            df$Question[Line],",",
            df$Response[Line], fill=TRUE, sep="")
    }

}

### Facets helper functions
### NumberRatersToUse can be 23 or less for trial version
### this is wide format
PrintOutFacetsInfoWide <- function(df, NumberRatersToUse, printLineNums=FALSE) {

    if (NumberRatersToUse >= length(unique(df$Subject)))
        RatersUsed <- unique(df$Subject)
    if (NumberRatersToUse < length(unique(df$Subject))) {
        RatersUsed <- sample(unique(df$Subject), size=NumberRatersToUse)
        df <- filter(df, Subject %in% RatersUsed)
    }

    cat("Raters Used (put in 'Raters' component)", fill=TRUE)
    RatersUsed <- unique(df$Subject)
    for (Rater in sort(RatersUsed)) {
        cat(Rater, fill=TRUE)
    }
    cat(fill=TRUE)
    cat("Data Used (put in 'data' component)", fill=TRUE)

    maxQuestion <- max(as.numeric(df$QuestionNum))
    df$range <- paste0("1-", maxQuestion)
    output.df <- df %>%
        ## rename(V = VideoNum) %>%
        ## rename(S = Subject) %>%
        select(VideoNum, Subject, range, QuestionNum, Response) %>%
        pivot_wider(names_from = QuestionNum,
                    values_from = Response)
    ##    for (Line in 1:(nrow(df))) {
    ## for (Line in 1:5) {
    ##     if (printLineNums)
    ##         cat(Line, ":", sep="")
    ## }
    output.df$VideoNum <- as.numeric(output.df$VideoNum)
##    print(as.data.frame(output.df[1,]))
#    print(as.data.frame(output.df), row.names=FALSE)
    write.table(output.df, row.names=FALSE, col.names=FALSE, sep=",")

    ##     cat(df$VideoNum[Line],",",
    ##         df$Subject[Line],",",
    ##         df$QuestionNum[Line],",",
    ##         df$Response[Line], " ;; ", sep="")
    ##     cat(df$Video[Line],",",
    ##         "R", df$Subject[Line],",",
    ##         df$Question[Line],",",
    ##         df$Response[Line], fill=TRUE, sep="")
    ## }

}

ConvertToNewRange <- function(Value, OriginalMin, OriginalMax, NewMin, NewMax) {

## NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin

    NewValue <- (((Value - OriginalMin) * (NewMax - NewMin)) / (OriginalMax - OriginalMin)) + NewMin
    return(NewValue)
}

### which estimator?
### https://link.springer.com/article/10.3758/s13428-018-1055-2
### suggests that ML (default) is not appropriate for Likert data but DWLS is
### of course thresholds may be slightly different too (point of above paper)
### cfi want > .9; rmsea want <= .05
### following paper suggests to use srmr since it does not change based on estimation method
### https://journals.sagepub.com/doi/10.1177/0013164419885164
RunCFA <- function(df, description, estimator="DWLS") {

##    estimator <- "ML"
    cfa.model <- cfa(description, data=df,estimator=estimator)
    print(summary(cfa.model))

    print(fitmeasures(cfa.model, c('cfi', 'rmsea', 'tli', 'bic', 'srmr')))
    cat("cfi >= .95; rmsea <= .06; tli >= .95; srmr <= .08", fill=TRUE)
#    print(fitmeasures(cfa.model))

}

