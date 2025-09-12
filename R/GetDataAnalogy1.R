

require(tidyverse)
require(lubridate)  ## for today()
require(Hmisc) ## for stat_summary

########################
### Get Original file###
########################

dataFileName <- paste0(dataDirectory, "Analogy1_50Ps_9.12.2025.csv") 
dateExpr <- "\\d{1,2}.\\d{1,2}.\\d{4}"  ## super simple, easy to be wrong so check
dataDate <- str_match(dataFileName, dateExpr)
cat("This data was finished collected on", dataDate, fill=TRUE)



Analogy1.df <- SafeReadCSV(dataFileName)

# # adding empty column to deal with split conditions issues
# if (!"AddToCartButtonPushes" %in% names(SoA1.df)) {
#   SoA1.df[,'AddToCartButtonPushes'] = NA
# }

########################
### Wonky Participants #
########################

ParticipantsToRemove <- NULL
ParticipantsToRemove <- c(ParticipantsToRemove)  ## first trial , 6, 11,  13, 21
# ParticipantsToRemove <- c(ParticipantsToRemove)  ## attention check
ParticipantsToRemove <- c(ParticipantsToRemove)  ## language: other

if (!is.null(ParticipantsToRemove)) {
    cat("** Removing participant(s):  ")
    cat(ParticipantsToRemove, fill=TRUE)
}

SoA1.df <- RemoveParticipants(SoA1.df, ParticipantsToRemove, report=TRUE)

### malcolm could do this too but just decided to do it here.
### notice that could put question of any version here to be ActionControl or OutcomeControl or whatever ...
SoA1.df <- SoA1.df %>%
  mutate(Question = case_when(Question == "Is the item you chose currently in your cart?" ~ "ItemInCart",
                              Question == "How much control did you feel you had over your action putting the item in the cart?" ~ "ActionControl",
                              Question == "How much control did you feel over which item went into the cart?" ~ "OutcomeControl",
                              Question == "How much control did you feel over the entire shopping trip?" ~ "OverallControl",
                              Question == "Participant's stated goal" ~ "StatedGoal",
                              TRUE ~ Question))


########################
### common dataframes  #
########################

### df.wide is primarily for psych and correlations
### added Trial to selection criteria since it's important
SoA1.wide <- SoA1.df %>%
    dplyr::select(Subject, Condition, Trial, Question, Response) %>% 
    pivot_wider(
        names_from = Question,
        values_from = Response
    )
SoA1.long <- SoA1.wide %>%
    pivot_longer(!c(Subject, Condition, Trial),
                 names_to = "Question",
                 values_to = "Response")

######################################
### Any other datasets needed?   #####
######################################

### PAUO had the wrong item (not selected) go into the shopping cart
### if a P in the PAUO condition said "yes" we assume they did not notice
### so performance should be similar to PAPO
### we will call these PAUO-notNoticed when P did not notice item was not what they selected

### UAPO put the item in the cart before the click happened
### if a P in the UAPO condition clicked on the cart, we assume they did not notice the item going into the cart without the click
### so performance should be similar to PAPO
### we will call these UAPO-notNoticed when P clicked the button (unnecessarily) 

### See checkParticipants.R or SplitConditions.R to check on the numbers

### Tidyverse does not love long column names with quotes (see SplitConditions.R)
### so doing it the ugly way
## SoA1Split.wide <- SoA1.df %>%
##   mutate(NewCondition = case_when(Condition == "PAUO" &
##                                  Question == "Is the item you chose currently in your cart?" &
##                                  Response == "yes" ~ "PAUO-notNoticed", # what to change it to if true
## #                                 Condition == "UAPO" &
## #                                 AddToCartButtonPushes > 0 ~ "UAPO-notNoticed", # what to change it to if true
##                                  TRUE ~ Condition)) %>% # value to output when condition is false
## #  dplyr::select(Subject, Condition, Trial, Question, Response) %>%

##   dplyr::select(Subject, Condition, NewCondition, Trial, AddToCartButtonPushes, Question, Response) %>%
##   pivot_wider(
##     names_from = Question,
##     values_from = Response
##   )

## SoA1Split.wide$InCart <- SoA1Split.wide$"Is the item you chose currently in your cart?"

## SoA1Split.wide <- SoA1Split.wide %>%
##  mutate(Condition = case_when(Condition == "PAUO" &
##                                 InCart == "yes" ~ "PAUO-notNoticed",
##                                 Condition == "UAPO" &
##                                 AddToCartButtonPushes > 0 ~ "UAPO-notNoticed",
##                                 TRUE ~ Condition)) %>%
##  dplyr::select(-c(AddToCartButtonPushes, InCart))  ## clean up

## SoA1Split.long <- SoA1Split.wide %>%
##     pivot_longer(!c(Subject, Condition, Trial),
##                  names_to = "Question",
##                  values_to = "Response")

## above is replaced by below versions which seem to work.

SoA1Split.wide <- SoA1.df %>%
  dplyr::select(Subject, Condition, Trial, AddToCartButtonPushes, Question, Response) %>%
  pivot_wider(
    names_from = Question,
    values_from = Response
  ) %>%
  mutate(Condition = case_when(Condition == "PAUO" &
                                    ItemInCart == "yes" ~ "PAUO-notNoticed",
                                  TRUE ~ Condition))

SoA1Split.long <- SoA1Split.wide %>%
    pivot_longer(!c(Subject, Condition, Trial),
                 names_to = "Question",
                 values_to = "Response")

cat("NOTE that for within experiment trials are 1-15 not just 1-5", fill=TRUE)

##stop('halt')

### if you want to use the not-noticed conditions (see details in SplitConditions.R and checkParticipants.R)
### s

if (!exists("UseSplitConditions")) {
  cat(fill=TRUE)
  cat("Assuming you do not want to use split conditions...", fill=TRUE)
  cat("Set UseSplitConditions to TRUE if you do want to use it.", fill=TRUE)
  cat(fill=TRUE)
  UseSplitConditions <- FALSE
}
cat(fill=TRUE)
if (UseSplitConditions) {
  cat("** Using Split Conditions for all analyses", fill=TRUE)
  SoA1.wide <- SoA1Split.wide
  SoA1.long <- SoA1Split.long
} else {
  cat("** Not using Split Conditions for any analyses", fill=TRUE)
}
cat(fill=TRUE)

### convert to numeric

SoA1.wide <- SoA1.wide %>%
  mutate_at(c('ActionControl', 'OutcomeControl', 'OverallControl'), as.numeric)

cat("NOTE:  If you use the .long version you need to convert Responses to numeric", fill=TRUE)
