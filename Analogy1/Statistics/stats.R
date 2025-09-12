####################################
## EDA:  Exploratory Data Analysis #
####################################

########################
### Experiment setup ###
########################

experimentName <- "SoA1"
whoami <- Sys.info()[["user"]]
if (whoami == "trafton") {
    workingDirectory <- "~/Documents/graphics/SenseOfAgency/"
}
source(paste0(workingDirectory, "R/helper.R"))
graphSaveDirectory <- paste0(workingDirectory, "graphs/", experimentName, "/")
dataDirectory <- paste0(workingDirectory, "data/raw/", experimentName, "/")
setwd(workingDirectory)
VerifyPathIsSafe(graphSaveDirectory)
VerifyPathIsSafe(dataDirectory)
cat("Must not use split conditions for statistics", fill=TRUE)
cat("OR must remove participants who did not notice since within stats missing data", fill=TRUE)

UseSplitConditions <- FALSE ## TRUE 

source(paste0(workingDirectory, "R/GetData", experimentName, ".R"))
require(rstatix)  ## remember rstatix does not work with tibbles so have to convert to df

### whatever stats you like 
SoA1.wide$Subject <- factor(SoA1.wide$Subject)
SoA1.wide$Condition <- factor(SoA1.wide$Condition)
SoA1Trial1.wide <- SoA1.wide %>%
  ### NOTE that with a within subject trials range from 1-15 not 1-5
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Condition != "PAUO-notNoticed") %>%
  filter(Trial == 1)

SoA1Trial5.wide <- SoA1.wide %>%
  ### NOTE that with a within subject trials range from 1-15 not 1-5
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Condition != "PAUO-notNoticed") %>%
  filter (Trial == 5)

SoA1Trialall.wide <- SoA1.wide %>%
  ### NOTE that with a within subject trials range from 1-15 not 1-5
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Condition != "PAUO-notNoticed") %>%
  filter(Trial == 1 | Trial ==5)


########################################
### trying to collapse across trials ###
########################################

## make new df that is just mean of trials 1 and 5 resp per participant
# ACTIONS
avg_act_resp <- data.frame(matrix(NA, nrow = 43, ncol = 1))
for (i in 1:258) {
  a <- c(SoA1Trial1.wide$ActionControl[i],SoA1Trial5.wide$ActionControl[i])
  avg_act_resp[i,] <- mean(a)
}

# OUTCOMES
avg_out_resp <- data.frame(matrix(NA, nrow = 43, ncol = 1))
for (i in 1:258) {
  o <- c(SoA1Trial1.wide$OutcomeControl[i],SoA1Trial5.wide$OutcomeControl[i])
  avg_out_resp[i,] <- mean(o)
}

# OVERALL
avg_all_resp <- data.frame(matrix(NA, nrow = 43, ncol = 1))
for (i in 1:258) {
  al <- c(SoA1Trial1.wide$OverallControl[i],SoA1Trial5.wide$OverallControl[i])
  avg_all_resp[i,] <- mean(al)
}

# CREATE THE NEW DATAFRAME TO USE FOR ANALYSIS
avg_resp <- data.frame(avg_act_resp, avg_out_resp, avg_all_resp)
SoA1Trialavg.wide <- data.frame(SoA1Trial1.wide$Subject, SoA1Trial1.wide$Condition, avg_resp)
colnames(SoA1Trialavg.wide) <- c("Subject", "Condition", "ActionControl","OutcomeControl","OverallControl")

##########################
### checking normality ###
##########################

shapiro_test(SoA1Trialall.wide$ActionControl)
hist_act <- ggplot(SoA1Trialall.wide, aes(x=ActionControl)) + geom_histogram()
print(hist_act)

qqnorm(SoA1Trialall.wide$ActionControl, pch = 1, frame = FALSE)
qqline(SoA1Trialall.wide$ActionControl, col = "steelblue", lwd = 2)

shapiro_test(SoA1Trialall.wide$OutcomeControl)
hist_out <- ggplot(SoA1Trialall.wide, aes(x=OutcomeControl)) + geom_histogram()
print(hist_out)

qqnorm(SoA1Trialall.wide$OutcomeControl, pch = 1, frame = FALSE)
qqline(SoA1Trialall.wide$OutcomeControl, col = "steelblue", lwd = 2)

shapiro_test(SoA1Trialall.wide$OverallControl)
hist_all <- ggplot(SoA1Trialall.wide, aes(x=OverallControl)) + geom_histogram()
print(hist_all)

qqnorm(SoA1Trialall.wide$OverallControl, pch = 1, frame = FALSE)
qqline(SoA1Trialall.wide$OverallControl, col = "steelblue", lwd = 2)


####################
### Run analyses ###
####################

cat("* Trial 1", fill=TRUE)
SoA1Trial1.wide <- as.data.frame(SoA1Trial1.wide)
for (currentDV in c("ActionControl", "OutcomeControl", "OverallControl")) {
  cat("\n**", currentDV, fill=TRUE)
  res.aov <- anova_test(data = SoA1Trial1.wide, dv = currentDV, wid = Subject, within = Condition)
  print(get_anova_table(res.aov))

  pw <- SoA1Trial1.wide %>%
    pairwise_t_test(
      as.formula(paste0(currentDV, " ~ Condition")), paired = TRUE,
      p.adjust.method = "bonferroni"  ## big hammer for fun
    )
  print(pw)
}

cat("* Trial 5", fill=TRUE)
SoA1Trial5.wide <- as.data.frame(SoA1Trial5.wide)
for (currentDV in c("ActionControl", "OutcomeControl", "OverallControl")) {
  cat("\n**", currentDV, fill=TRUE)
  res.aov <- anova_test(data = SoA1Trial5.wide, dv = currentDV, wid = Subject, within = Condition)
  print(get_anova_table(res.aov))

  pw <- SoA1Trial5.wide %>%
    pairwise_t_test(
      as.formula(paste0(currentDV, " ~ Condition")), paired = TRUE,
      p.adjust.method = "bonferroni"  ## big hammer for fun
    )
  print(pw)
}

cat("* Collapsed Across Trials", fill=TRUE)
SoA1Trialavg.wide <- as.data.frame(SoA1Trialavg.wide)
for (currentDV in c("ActionControl", "OutcomeControl", "OverallControl")) {
  cat("\n**", currentDV, fill=TRUE)
  res.aov <- anova_test(data = SoA1Trialavg.wide, dv = currentDV, wid = Subject, within = c(Condition))
  print(get_anova_table(res.aov))
  
  pw <- SoA1Trialavg.wide %>%
    pairwise_t_test(
      as.formula(paste0(currentDV, " ~ Condition")), paired = TRUE,
      p.adjust.method = "bonferroni"  ## big hammer for fun
    )
  print(pw)
}

##################
## get averages ##
##################
actions <- SoA1Trialavg.wide %>%
  group_by(Condition) %>%
  dplyr::summarize(act_mean = mean(ActionControl, na.rm = TRUE), act_sd = sd(ActionControl, na.rm = TRUE), n = n()) 

outcomes <- SoA1Trialavg.wide %>%
  group_by(Condition) %>%
  dplyr::summarize(out_mean = mean(OutcomeControl, na.rm = TRUE), out_sd = sd(OutcomeControl, na.rm = TRUE), n = n()) 

overall <- SoA1Trialavg.wide %>%
  group_by(Condition) %>%
  dplyr::summarize(over_mean = mean(OverallControl, na.rm = TRUE), act_sd = sd(OverallControl, na.rm = TRUE), n = n()) 


# #######################################
# ### friedman tests just for funsies ###
# #######################################
# 
# #########
# # ACTIONS
# #########
# # trial 1
# friedman.test(y=SoA1Trial1.wide$ActionControl, groups=SoA1Trial1.wide$Condition, blocks=SoA1Trial1.wide$Subject)
# pairwise.wilcox.test(SoA1Trial1.wide$ActionControl, SoA1Trial1.wide$Condition, p.adj = "bonf")
# 
# # trial 5
# friedman.test(y=SoA1Trial5.wide$ActionControl, groups=SoA1Trial5.wide$Condition, blocks=SoA1Trial5.wide$Subject)
# pairwise.wilcox.test(SoA1Trial5.wide$ActionControl, SoA1Trial5.wide$Condition, p.adj = "bonf")
# 
# ##########
# # OUTCOMES
# ##########
# # trial 1
# friedman.test(y=SoA1Trial1.wide$OutcomeControl, groups=SoA1Trial1.wide$Condition, blocks=SoA1Trial1.wide$Subject)
# pairwise.wilcox.test(SoA1Trial1.wide$OutcomeControl, SoA1Trial1.wide$Condition, p.adj = "bonf")
# 
# # trial 5
# friedman.test(y=SoA1Trial5.wide$OutcomeControl, groups=SoA1Trial5.wide$Condition, blocks=SoA1Trial5.wide$Subject)
# pairwise.wilcox.test(SoA1Trial5.wide$OutcomeControl, SoA1Trial5.wide$Condition, p.adj = "bonf")
# 
# #########
# # OVERALL
# #########
# # trial 1
# friedman.test(y=SoA1Trial1.wide$OverallControl, groups=SoA1Trial1.wide$Condition, blocks=SoA1Trial1.wide$Subject)
# pairwise.wilcox.test(SoA1Trial1.wide$OverallControl, SoA1Trial1.wide$Condition, p.adj = "bonf")
# 
# # trial 5
# friedman.test(y=SoA1Trial5.wide$OverallControl, groups=SoA1Trial5.wide$Condition, blocks=SoA1Trial5.wide$Subject)
# pairwise.wilcox.test(SoA1Trial5.wide$OverallControl, SoA1Trial5.wide$Condition, p.adj = "bonf")

# ######################
# # AVERAGED OVER TRIALS
# ######################
# 
# ## THIS DOESN'T WORK!
# #  actions
# friedman.test(y=SoA1Trialavg.wide$ActionControl, groups=SoA1Trialavg.wide$Condition, blocks=SoA1Trialavg.wide$Subject)
# pairwise.wilcox.test(SoA1Trialavg.wide$ActionControl, SoA1Trialavg.wide$Condition, p.adj = "bonf")
# 
# #  outcomes
# friedman.test(y=SoA1Trialavg.wide$OutcomeControl, groups=SoA1Trialavg.wide$Condition, blocks=SoA1Trialavg.wide$Subject)
# pairwise.wilcox.test(SoA1Trialavg.wide$OutcomeControl, SoA1Trialavg.wide$Condition, p.adj = "bonf")
# 
# #  overall
# friedman.test(y=SoA1Trialavg.wide$OverallControl, groups=SoA1Trialavg.wide$Condition, blocks=SoA1Trialavg.wide$Subject)
# pairwise.wilcox.test(SoA1Trialavg.wide$OverallControl, SoA1Trialavg.wide$Condition, p.adj = "bonf")