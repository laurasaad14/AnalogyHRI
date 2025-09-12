####################################
## EDA:  Exploratory Data Analysis #
####################################

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
graphSaveDirectory <- paste0(workingDirectory, "graphs/", experimentName, "/")
dataDirectory <- paste0(workingDirectory, "data/processed/", experimentName, "/")
setwd(workingDirectory)
VerifyPathIsSafe(graphSaveDirectory)
VerifyPathIsSafe(dataDirectory)

source(paste0(workingDirectory, "R/GetData", experimentName, ".R"))


library(dplyr)
library(tidyr)
library(ggplot2)

# PLOT PA SCALE ACROSS CONDITIONS

PA_scale <- c("PA_1", "PA_2", "PA_3", "PA_4", "actor")

Analogy1.df <- Analogy1.df %>%
  rowwise() %>%
  mutate(PA_mean = mean(c_across(PA_scale), na.rm = TRUE)) %>%
  ungroup()

Analogy1.df %>%
  ggplot(aes(x=condition, y=PA_mean)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  # facet_wrap (~Trial) +
  ggtitle("Perceived Agency")
ggsave(paste0(graphSaveDirectory, "PA_byCondition", dataDate, ".pdf"))


# plot emotion (SSRA) across conditions
SSRA_scale <- c("SSRA_1", "SSRA_2", "SSRA_3")

Analogy1.df <- Analogy1.df %>%
  rowwise() %>%
  mutate(SSRA_mean = mean(c_across(SSRA_scale), na.rm = TRUE)) %>%
  ungroup()

Analogy1.df %>%
  ggplot(aes(x=condition, y=SSRA_mean)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  # facet_wrap (~Trial) +
  ggtitle("Emotion")
ggsave(paste0(graphSaveDirectory, "SSRA_byCondition", dataDate, ".pdf"))



# plot custom items across conditions
custom_scale <- c("smell", "taste", "touch", "see", "hear", "walk")

Analogy1.df <- Analogy1.df %>%
  rowwise() %>%
  mutate(custom_mean = mean(c_across(custom_scale), na.rm = TRUE)) %>%
  ungroup()

Analogy1.df %>%
  ggplot(aes(x=condition, y=custom_mean)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  # facet_wrap (~Trial) +
  ggtitle("Custom Items")
ggsave(paste0(graphSaveDirectory, "custom_byCondition", dataDate, ".pdf"))



# plot whether Ps chose connected robot as more similar to human 

dat_first <- Analogy1.df %>%
  group_by(subjID) %>%
  slice_head(n = 1) %>%
  ungroup()

# Count TRUE/FALSE per condition
counts_connected <- table(dat_first$conPos1)
print(counts_connected)

# Plot 
counts_df <- as.data.frame(counts_connected)
names(counts_df) <- c("conPos1", "Frequency")

ggplot(counts_df, aes(x = conPos1, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +  # counts above bars
  theme_minimal() +
  labs(title = "P Chose Connected Robot More Similar to Human", x = "Connected in Position 1?", y = "Frequency")
ggsave(paste0(graphSaveDirectory, "DidPChooseConnected", dataDate, ".pdf"))
















#################################
## everyone answered questions ##
#################################

cat(fill=TRUE)
cat("Number of questions that each P answered:", fill=TRUE)
print(rowSums(table(SoA1.long$Subject, SoA1.long$Response)))

##cat("Slightly funky different numbers for P; fixed next round I think...", fill=TRUE)
totalItems <- 14  ## have to change it for every experiment
cat("Participants that did not answer", totalItems, "questions:", fill=TRUE)
rowSums(table(SoA1.long$Subject, SoA1.long$Response))[which(rowSums(table(SoA1.long$Subject, SoA1.long$Response)) != totalItems)]

##############################
## Average Ranking (graphs) ##
##############################

### no ranking graphs in SoA1

############################
### tidy ggplot of above ###
############################

### probably reasonable way of doing this -- have to do it for each question and/or trial though
Questions_v1 = c("How much control did you feel over your actions as you put the item in the cart?",
                  "How much control did you feel over the item you wanted going into the cart?",
                  "To what degree did you feel like you were in control during the above shopping trip?")

Questions_v2 = c("How much control did you feel over your actions (for example: your mouse clicks, hand movements, mouse movements)?",
               "How much control did you feel over the item you selected going into the cart?",
               "How much control did you feel over the entire shopping trip?")

Questions_v3 = c("How much control did you feel you had over your action putting the item in the cart?",
                 "How much control did you feel over which item went into the cart?",
                 "How much control did you feel over the entire shopping trip?")

SoA1.long %>%
  # filter(Question == "How much control did you feel over your actions (for example: your mouse clicks, hand movements, mouse movements)?") %>%
##  filter(Question == Questions_v3[1]) %>% ########## CHANGE THIS 
  filter(Question == "ActionControl") %>% 
  ### NOTE that with a within subject trials range from 1-15 not 1-5
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  facet_wrap (~Trial) +
  ggtitle("ACTIONS")
ggsave(paste0(graphSaveDirectory, "ActionControl", dataDate, ".pdf"))

SoA1.long %>%
  # filter(Question == "How much control did you feel over the item you selected going into the cart?") %>%
##  filter(Question == Questions_v3[2]) %>% ########## CHANGE THIS 
  filter(Question == "OutcomeControl") %>% 
  ### NOTE that with a within subject trials range from 1-15 not 1-5
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  facet_wrap (~Trial) +
  ggtitle("OUTCOMES")
ggsave(paste0(graphSaveDirectory, "OutcomeControl", dataDate, ".pdf"))


SoA1.long %>%
  # filter(Question == "How much control did you feel over the entire shopping trip?") %>%
##  filter(Question == Questions_v3[3]) %>% ########## CHANGE THIS 
  filter(Question == "OverallControl") %>% 
  ### NOTE that with a within subject trials range from 1-15 not 1-5
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  facet_wrap (~Trial) +
  ggtitle("OVERALL")

ggsave(paste0(graphSaveDirectory, "OverallControl-notnoticed", dataDate, ".pdf"))

####### same plots just collapsed over trial to more easily summarize across samples
SoA1.long %>%
##  filter(Question == Questions_v3[1]) %>% ########## CHANGE THIS 
  filter(Question == "ActionControl") %>% 
  #filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  #facet_wrap (~Trial) + 
  ggtitle("ACTIONS")
  #ggtitle("Average Response to Action Control Question")
ggsave(paste0(graphSaveDirectory, "ActionControl_collovertrial", dataDate, ".pdf"))

SoA1.long %>%
  # filter(Question == "How much control did you feel over the item you selected going into the cart?") %>%
##  filter(Question == Questions_v3[2]) %>% ########## CHANGE THIS 
  filter(Question == "OutcomeControl") %>% 
  #filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  #facet_wrap (~Trial) +
  ggtitle("OUTCOMES")
  #ggtitle("Average Response to Outcome Control Question")
ggsave(paste0(graphSaveDirectory, "OutcomeControl_collovertrial", dataDate, ".pdf"))

SoA1.long %>%
  # filter(Question == "How much control did you feel over the entire shopping trip?") %>%
##  filter(Question == Questions_v3[3]) %>% ########## CHANGE THIS 
  filter(Question == "OverallControl") %>% 
  #filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=22)) + theme(plot.title = element_text(hjust=0.5)) +
  #facet_wrap (~Trial) +
  ggtitle("OVERALL")
  #ggtitle("Average Response to Overall Control Question")
ggsave(paste0(graphSaveDirectory, "OverallControl_collovertrial", dataDate, ".pdf"))


###############
## for model ##
###############

avgs_SoA1 <- SoA1.long %>%
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Trial == 1 | Trial == 5) %>%
  mutate(Response = as.numeric(Response)) %>%
  group_by(Condition) %>%
  filter(Question == "OverallControl") %>%
  summarise(avg = mean(na.omit(Response))) %>%
  # pivot_wider(names_from = experiment, values_from = avg) %>%
  filter(Condition != "PAUO-notNoticed")



#######################
### plot for cogsci ###
#######################
install.packages("viridis")
library(viridis)
#library(mapproj)
#data(unemp, package = "viridis")

SoA1.long %>%
  filter(Question == "OverallControl" | Question == "ActionControl" | Question == "OutcomeControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response, fill = Condition)) +
  #scale_fill_viridis(discrete=TRUE, option="viridis") +
  scale_fill_manual(values=c("#666666",
                             "#999999",
                             "#cccccc")) + #"#152A55","#FFC000", "#7AADDC"
  stat_summary(fun = "mean", geom = "bar") +  ##    shape = 24, fill = "gray"
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  xlab("Condition") +
  #ylab("Average Response") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=21, family="serif")) + 
  theme(plot.title = element_text(hjust=0.5)) +
  theme(strip.text.x = element_text(face = "bold"), strip.background = element_rect(fill = "white")) + 
  facet_wrap(~Question) +
  ggtitle("Experiment 1: EA by Condition") + 
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")
ggsave(paste0(graphSaveDirectory, "exp1_cogsci_overall", dataDate, ".jpg"))

## in case we like colors
# "#88027A",
# "#7A8802",
# "#027A88"

######### plots for cognition paper

SoA1.long %>%
  filter(Question == "OverallControl" | Question == "ActionControl" | Question == "OutcomeControl") %>%
  mutate(Response = as.numeric(Response),
         Condition = factor(Condition, levels = c("PAPO", "UAPO", "PAUO"))) %>%
  ggplot(aes(x=Condition, y=Response, fill = Condition)) +
  scale_fill_manual(values=c("#666666",
                             "#999999",
                             "#cccccc"),
                    labels=c("Total Control", "Unpredictable Action", "Unpredictable Outcome")) + 
  stat_summary(fun = "mean", geom = "bar") +  ##    shape = 24, fill = "gray"
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  xlab("Condition") +
  #ylab("Average Response") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text=element_text(size=21, family="serif")) + 
  theme(plot.title = element_text(hjust=0.5)) +
  theme(strip.text.x = element_text(face = "bold"), strip.background = element_rect(fill = "white")) + 
  facet_grid(~Question, switch = "x") +
  ggtitle("Experiment 1: Agency by Condition") + 
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")
ggsave(paste0(graphSaveDirectory, "exp1_Cognition", dataDate, ".pdf"))

########### new version with raw data CONSCIOUSNESS AND COGNITION #############


SoA1.long %>%
  filter(Question == "OverallControl" | Question == "ActionControl" | Question == "OutcomeControl") %>%
  mutate(Response = as.numeric(Response),
         Condition = factor(Condition, levels = c("PAPO", "UAPO", "PAUO"))) %>%
  ggplot(aes(x = Condition, y = Response, fill = Condition)) +
  scale_fill_manual(values = c("#4d4d4d", "#808080", "#b3b3b3"),
                    labels = c("Total Control", "Unpredictable Action", "Unpredictable Outcome")) + 
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width = 0.2) +
  geom_jitter(position = position_jitter(width = 0.25, height = 0.2), size = 1.5, alpha = 0.4, color = "gray16") +
  xlab("Condition") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(text = element_text(size = 21, family = "serif")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text.x = element_text(face = "bold"), strip.background = element_rect(fill = "white")) + 
  facet_grid(~Question, switch = "x") +
  ggtitle("Experiment 1: Agency by Condition") + 
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")
ggsave(paste0(graphSaveDirectory, "exp1_CC", dataDate, ".tiff"))


## RT for model
SoA1RT.df <- SoA1.df %>%
  select(Subject, Trial, Condition, Question, Response, AddToCartRT) %>%
  mutate(experiment = "cd_within")

write.csv(SoA1RT.df, file = "SoA1_RTdata.csv")

################
### raw data ###
################

# plot responses across trials to overall control question
# per person with conditions labeled in order

SoA1.long %>%
  filter(Question == "OverallControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Trial, y = Response, color = Condition)) +
  geom_point() +
  scale_color_manual(values=c('#00BA38','#F8766D','#619CFF')) +
  facet_wrap(~Subject) +
  ylim(0, 7) + xlim(0,16) +
  ggtitle("Raw Responses to Overall Control Question")
ggsave(paste0(graphSaveDirectory, "raw_responses", dataDate, ".pdf"))


# plot responses across trials to all three control questions
# per person with conditions labeled in order

SoA1.long %>%
  filter(Question == "OverallControl" | Question == "ActionControl" | Question == "OutcomeControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Trial, y = Response, color = Condition, shape = Question)) +
  geom_point() +
  scale_color_manual(values=c('#00BA38','#F8766D','#619CFF')) +
  scale_shape(solid = FALSE) +
  facet_wrap(~Subject) +
  ylim(0, 7) + xlim(0,16) +
  ggtitle("Raw Responses to Control Questions")
ggsave(paste0(graphSaveDirectory, "raw_responses_allctrl", dataDate, ".pdf"))


# NOT NOTICED OVERALL CONTROL

SoA1.long %>%
  filter(Question == "OverallControl" | Question == "ActionControl" | Question == "OutcomeControl" & is.na(Response) == FALSE) %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x = Trial, y = Response, color = Condition, shape = Question)) +
  geom_point() +
  facet_wrap(~Subject) +
  scale_color_manual(values=c('#009E73','#000000','#D55E00','#619CFF')) +
  scale_shape_manual(values = c(1, 2, 0)) +
  ylim(0, 7) +
  ggtitle("SOA1 - Responses to Control Questions (only not-noticed Ps)")
ggsave(paste0(graphSaveDirectory, "notnoticed_allControlQs", dataDate, ".pdf"))



###############
### RT data ###
###############

# create new variable that track the RT of just the task portion of the experiment (not responding to EAQs)
# SoA1.df <- SoA1.df %>%
#   mutate(taskRT = AddToCartRT - StartButtonRT)

# check for weird vals

SoA1.df %>%
  filter(Condition == "PAPO") %>%
  ggplot(aes(x = AddToCartRT)) +
  geom_histogram() +
  ggtitle("hist of task RT in total control condition - exp 1 (click and drag within)")

SoA1.df %>%
  filter(Condition == "UAPO") %>%
  ggplot(aes(x = AddToCartRT)) +
  geom_histogram() +
  ggtitle("hist of task RT in weird action condition - exp 1 (click and drag within)")

SoA1.df %>%
  filter(Condition == "PAUO") %>%
  ggplot(aes(x = AddToCartRT)) +
  geom_histogram() +
  ggtitle("hist of task RT in weird outcome condition - exp 1 (click and drag within)")

# plot difference in RT across conditions (should be PAPO = PAUO < UAPO)


SoA1.df %>%
  ggplot(aes(x = Condition, y = AddToCartRT)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  xlab("Condition") + ylab("Task RT (sec)") +
  scale_x_discrete(labels = c("Total Control", "Unpredictable Outcome", "Unpredicable Action")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # theme(axis.text.x = element_text(angle = 45)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(strip.text.x = element_text(face = "bold"), strip.background = element_rect(fill = "white")) + 
  ggtitle("SOA1 - Task RT by Condition")
ggsave(paste0(graphSaveDirectory, "taskRT_bycond", dataDate, ".pdf")) 


SoA1.df %>%
  filter(Question == "OverallControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x = Condition, y = QuestionsRT, fill = Condition)) +
  scale_fill_manual(values=c("#666666",
                             "#999999",
                             "#cccccc")) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  xlab("Condition") + ylab("Time to Respond to EA Qs (sec)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(strip.text.x = element_text(face = "bold"), strip.background = element_rect(fill = "white")) + 
  ggtitle("Response time to EA Questions by Condition") + 
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")
ggsave(paste0(graphSaveDirectory, "EAQ_RT_bycond", dataDate, ".pdf"))
  
# relationship between Q RT and response

SoA1.df%>%
  filter(Question == "OverallControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Response, y = QuestionsRT)) +
  geom_point() +
  geom_smooth(method=lm, color="red",se=TRUE) +
  facet_wrap(~Condition) +
  #ylim(0, 100) + xlim(0,7) +
  ggtitle("Relationship between Q RT and Response")
ggsave(paste0(graphSaveDirectory, "EAQRT_EAQresp_scatter", dataDate, ".pdf"))

# relationship between Q RT and response collapsed over conditions

SoA1.df%>%
  filter(Question == "OverallControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Response, y = QuestionsRT)) +
  geom_point() +
  geom_smooth(method=lm, color="red",se=TRUE) +
  #facet_wrap(~Condition) +
  ylim(0, 200) + xlim(0,7) +
  ggtitle("Relationship between Q RT and Response")
ggsave(paste0(graphSaveDirectory, "EAQRT_EAQresp_scatter_all", dataDate, ".pdf"))

# # add to cart RT (trial length) by condition - UAPO should be much longer
# 
# 
# SoA1.df %>%
#   filter(Question == "OverallControl") %>%
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x = Condition, y = AddToCartRT, fill = Condition)) +
#   scale_fill_manual(values=c("#666666",
#                              "#999999",
#                              "#cccccc")) +
#   stat_summary(fun = "mean", geom = "bar") +
#   stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
#   xlab("Condition") + ylab("Add to Cart RT (sec)") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   theme(plot.title = element_text(hjust=0.5)) +
#   theme(strip.text.x = element_text(face = "bold"), strip.background = element_rect(fill = "white")) + 
#   ggtitle("Response time to add to cart by Condition") + 
#   scale_x_discrete(labels = NULL, breaks = NULL) + 
#   labs(x = "")
# ggsave(paste0(graphSaveDirectory, "trialRT_EAQresp_bycond", dataDate, ".pdf"))

# relationship between add to cart RT (trial length) and EAQ response

SoA1.df%>%
  filter(Question == "OverallControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Response, y = AddToCartRT)) +
  geom_point() +
  geom_smooth(method=lm, color="red",se=TRUE) +
  facet_wrap(~Condition) +
  #ylim(0, 100) + xlim(0,7) +
  ggtitle("Relationship between Add to Cart RT and Response")
ggsave(paste0(graphSaveDirectory, "trialRT_EAQresp_scatter", dataDate, ".pdf"))

# relationship between add to cart RT (trial length) and EAQ response collapsed over condition

SoA1.df%>%
  filter(Question == "OverallControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Response, y = AddToCartRT)) +
  geom_point() +
  geom_smooth(method=lm, color="red",se=TRUE) +
  #facet_wrap(~Condition) +
  #ylim(0, 50) + xlim(0,7) +
  ggtitle("Relationship between Add to Cart RT and Response")
ggsave(paste0(graphSaveDirectory, "trialRT_EAQresp_scatter_all", dataDate, ".pdf"))

# # add to cart and response in UAPO only
# SoA1.df%>%
#   filter(Question == "OverallControl" & Condition == "UAPO") %>%
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Response, y = AddToCartRT)) +
#   geom_point() +
#   geom_smooth(method=lm, color="red",se=TRUE) +
#   #facet_wrap(~Subject) +
#   #ylim(0, 50) + xlim(0,7) +
#   ggtitle("Click and Drag within - Relationship between Add to Cart RT and Response in UAPO only")
# ggsave(paste0(graphSaveDirectory, "UAPO_overallcontrolVtrialRT", dataDate, ".pdf"))

# # add to cart RT overtime. trial 1 and trial 5 within UAPO only plotting response to overall control
# 
# SoA1.df %>%
#   filter(Condition == "UAPO" & Question == "OverallControl") %>%
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Trial, y = AddToCartRT, size = Response)) +
#   geom_point() +
#   scale_size(range = c(1, 5)) +
#   #geom_smooth(method=lm, color="red",se=TRUE) +
#   facet_wrap(~Subject) +
#   ylim(0, 60) + xlim(0,15) +
#   ylab("Trial RT (seconds)") +
#   ggtitle("Click and Drag Within - Trial RT and Response to Overall Agency Q Over Time in UAPO")
# ggsave(paste0(graphSaveDirectory, "UAPO_trialRTandResp_byP", dataDate, ".pdf"))


#######
# do people accumulate information about weird actions and outcomes differently? answer = no

SoA1.long %>%
  filter(Question == "OverallControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  mutate(Trial = case_when(Trial == 6 ~ 1,
                           Trial == 10 ~ 5,
                           Trial == 11 ~ 1,
                           Trial == 15 ~ 5,
                           TRUE ~ Trial)) %>%
  filter(Trial == 1 | Trial == 5) %>%
  ggplot(aes(x=Trial, y=Response)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  ylim(0, 7) + 
  theme_minimal() + 
  ggtitle("SOA1 Click and Drag Within: \nAverage Responses to Overall Control") + 
  theme(plot.title = element_text(hjust=0.5)) +
  theme(text=element_text(size=20)) +
  scale_x_discrete(limits = c(1,5)) +
  #theme(axis.text.x = element_text(angle = 45), panel.grid.major.x = element_blank()) +
  facet_wrap(~Condition)
ggsave(paste0(graphSaveDirectory, "resp_overtrials_bycond", dataDate, ".pdf"))

####################
## previous trial ##
####################

# PAUO performance based on what previous block was
# bar chart comparing PAUO responses when prev block was practice,
# when prev block was PAPO, and when previous block was UAPO

# create new variable that has just what we need for this analysis
response_test <- SoA1Split.long %>%
  filter(Question == "OverallControl") %>%
  select(Subject, Trial, Condition, Question, Response)

# create empty data frames for each of the variables I want
PAUO_prPAPO <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was PAPO
PAUO_prUAPO <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was UAPO
PAUO_prPract <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was practice

# put data in variables
for (i in 2:nrow(response_test)) {
  if (response_test$Condition[i] == "PAUO" & response_test$Condition[i-1] == "PAPO") {
    PAUO_prPAPO = rbind(PAUO_prPAPO, data.frame(response = as.numeric(response_test[i,5])))
    # PAUO_prPAPO[i] = response_test[i,5] 
    #stop()
  } else if (response_test$Condition[i] == "PAUO" & response_test$Condition[i-1] == "UAPO") {
    PAUO_prUAPO = rbind(PAUO_prUAPO, data.frame(response = as.numeric(response_test[i,5])))
    #PAUO_prUAPO[i] = response_test[i,5]
  } else if (response_test$Condition[i] == "PAUO" & response_test$Trial[i] == 1) {
    PAUO_prPract = rbind(PAUO_prPract, data.frame(response = as.numeric(response_test[i,5])))
  }
}

colnames(PAUO_prPAPO) <- ('prPAPO')
colnames(PAUO_prUAPO) <- ('prUAPO')
colnames(PAUO_prPract) <- ('prPract')
# 
# PAUO_prPAPO = cbind("id"=rownames(PAUO_prPAPO), PAUO_prPAPO)
# PAUO_prUAPO = cbind("id"=rownames(PAUO_prUAPO), PAUO_prUAPO)
# PAUO_prPract = cbind("id"=rownames(PAUO_prPract), PAUO_prPract)

library(plyr)
PAUO_prev_trial <- rbind.fill(PAUO_prPAPO, PAUO_prUAPO)
PAUO_prev_trial <- rbind.fill(PAUO_prev_trial, PAUO_prPract)

PAUO_prev_trial_long <- gather(PAUO_prev_trial, "condition", "response", "prPAPO", "prUAPO", "prPract", factor_key = TRUE)

# plot
PAUO_prev_trial_long %>%
  ggplot(aes(x = condition, y = response)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  ylim(0, 7) +
  theme_minimal() +
  ggtitle("SOA1 - Effect of Previous Trial on Responses in PAUO") + 
  theme(plot.title = element_text(hjust=0.5)) 
ggsave(paste0(graphSaveDirectory, "PAUO_previoustrial", dataDate, ".pdf"))

################ UAPO 

# create empty data frames for each of the variables I want
UAPO_prPAPO <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was PAPO
UAPO_prPAUO <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was UAPO
UAPO_prPract <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was practice

# put data in variables
for (i in 2:nrow(response_test)) {
  if (response_test$Condition[i] == "UAPO" & response_test$Condition[i-1] == "PAPO") {
    UAPO_prPAPO = rbind(UAPO_prPAPO, data.frame(response = as.numeric(response_test[i,5])))
    # PAUO_prPAPO[i] = response_test[i,5] 
    #stop()
  } else if (response_test$Condition[i] == "UAPO" & response_test$Condition[i-1] == "PAUO") {
    UAPO_prPAUO = rbind(UAPO_prPAUO, data.frame(response = as.numeric(response_test[i,5])))
    #PAUO_prUAPO[i] = response_test[i,5]
  } else if (response_test$Condition[i] == "UAPO" & response_test$Trial[i] == 1) {
    UAPO_prPract = rbind(UAPO_prPract, data.frame(response = as.numeric(response_test[i,5])))
  }
}

colnames(UAPO_prPAPO) <- ('prPAPO')
colnames(UAPO_prPAUO) <- ('prPAUO')
colnames(UAPO_prPract) <- ('prPract')


library(plyr)
UAPO_prev_trial <- rbind.fill(UAPO_prPAPO, UAPO_prPAUO)
UAPO_prev_trial <- rbind.fill(UAPO_prev_trial, UAPO_prPract)

UAPO_prev_trial_long <- gather(UAPO_prev_trial, "condition", "response", "prPAPO", "prPAUO", "prPract", factor_key = TRUE)

# plot
UAPO_prev_trial_long %>%
  ggplot(aes(x = condition, y = response)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  ylim(0, 7) +
  theme_minimal() +
  ggtitle("SOA1 - Effect of Previous Trial on Responses in UAPO") + 
  theme(plot.title = element_text(hjust=0.5)) 
ggsave(paste0(graphSaveDirectory, "UAPO_previoustrial", dataDate, ".pdf"))
######################## PAPO

# create empty data frames for each of the variables I want
PAPO_prUAPO <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was PAPO
PAPO_prPAUO <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was UAPO
PAPO_prPract <- NULL #data.frame(matrix(NA, nrow = nrow(response_test), ncol = 1)) # previous trial was practice

# put data in variables
for (i in 2:nrow(response_test)) {
  if (response_test$Condition[i] == "PAPO" & response_test$Condition[i-1] == "UAPO") {
    PAPO_prUAPO = rbind(PAPO_prUAPO, data.frame(response = as.numeric(response_test[i,5])))
    # PAUO_prPAPO[i] = response_test[i,5] 
    #stop()
  } else if (response_test$Condition[i] == "PAPO" & response_test$Condition[i-1] == "PAUO") {
    PAPO_prPAUO = rbind(PAPO_prPAUO, data.frame(response = as.numeric(response_test[i,5])))
    #PAUO_prUAPO[i] = response_test[i,5]
  } else if (response_test$Condition[i] == "PAPO" & response_test$Trial[i] == 1) {
    PAPO_prPract = rbind(PAPO_prPract, data.frame(response = as.numeric(response_test[i,5])))
  }
}

colnames(PAPO_prUAPO) <- ('prUAPO')
colnames(PAPO_prPAUO) <- ('prPAUO')
colnames(PAPO_prPract) <- ('prPract')


library(plyr)
PAPO_prev_trial <- rbind.fill(PAPO_prUAPO, PAPO_prPAUO)
PAPO_prev_trial <- rbind.fill(PAPO_prev_trial, PAPO_prPract)

PAPO_prev_trial_long <- gather(PAPO_prev_trial, "condition", "response", "prUAPO", "prPAUO", "prPract", factor_key = TRUE)

# plot
PAPO_prev_trial_long %>%
  ggplot(aes(x = condition, y = response)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, conf.int = 0.95, geom = "errorbar", width=.2) +
  ylim(0, 7) +
  theme_minimal() +
  ggtitle("SOA1 - Effect of Previous Trial on Responses in PAPO") + 
  theme(plot.title = element_text(hjust=0.5)) 
ggsave(paste0(graphSaveDirectory, "PAPO_previoustrial", dataDate, ".pdf"))



####################
### violin plots ###
####################

SoA1.long %>%
  filter(Question == "OverallControl" | Question == "ActionControl" | Question == "OutcomeControl") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  geom_violin() + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom = "pointrange", color = "black") +
  ylim(0, 7) + 
  theme_minimal() + 
  ggtitle("Experiment 1: \nAverage Responses to EA Questions") + 
  theme(plot.title = element_text(hjust=0.5)) +
  theme(text=element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 45), panel.grid.major.x = element_blank()) +
  facet_wrap(~Question)
ggsave(paste0(graphSaveDirectory, "Violin_all", dataDate, ".jpg"))

############################
## chosen item per person ##
############################

# checking to see if each p picks an item in the same location on every trial

SoA1.df %>%
   group_by(Subject, Trial) %>%
   ggplot(aes(x=ChosenItem)) + geom_histogram() + facet_wrap(~Subject) +
   xlab("Location of Chosen Grocery Item") + ggtitle("Location of Item Choices Per Participant")
ggsave(paste0(graphSaveDirectory, "ItemLocationCount", dataDate, ".pdf"))

# see if on trials where P had to abandon initial choice - their overall control goes down

item_match.df <- SoA1.df %>%
  filter(Question == "OverallControl", ChosenItem == ActualItem) %>%
  mutate(Response = as.numeric(Response))

item_mismatch.df <- SoA1.df %>%
  filter(Question == "OverallControl", ChosenItem != ActualItem) %>%
  mutate(Response = as.numeric(Response))

mean_match <- mean(item_match.df$Response)
mean_mismatch <- mean(item_mismatch.df$Response)


#################################################
# checking UAPO for add to cart button presses ##
#################################################

# SoA1.df %>%
#   filter(Condition == "UAPO") %>%
#   filter(Trial == 1 | Trial == 5) %>% 
#   ggplot(aes(x=AddToCartButtonPushes)) + geom_histogram() +
#   ggtitle("Add to Cart Button Pushes in UAPO") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) + xlab("Number of Button Presses")
# 
# # save file
# if (dataFileName == "~/SenseofAgency/data/raw/SoA1/SOA1_10Ps_firstpilot_11.8.2023.csv") {
#   ggsave(paste0(graphSaveDirectory, "UAPO_button_press_ct_11.8.jpg"))
# } else if (dataFileName == "~/SenseofAgency/data/raw/SoA1/SOA1_9Ps_11.9.2023.csv") {
#   ggsave(paste0(graphSaveDirectory, "UAPO_button_press_ct_11.9.jpg"))
# } else if (dataFileName == "~/SenseofAgency/data/raw/SoA1/SOA1_10Ps_pilot3_11.15.2023.csv") {
#   ggsave(paste0(graphSaveDirectory, "UAPO_button_press_ct_11.15.jpg"))
# } else {
# }

#############################
### demographic summaries ###
#############################

short_summary.df <- SummarizedInfo.df %>%
  group_by(Subject) %>%
  slice_head(n=1) %>%
  dplyr::select(Subject, Age, Gender, Language, Race, Education)


cat("Reported Age Mean and SD:", fill=TRUE)
print(mean(short_summary.df$Age)) 
print(sd(short_summary.df$Age))

cat("Reported gender:", fill=TRUE)
print(table(short_summary.df$Gender))

cat("Reported language:", fill=TRUE)
print(table(short_summary.df$Language))

cat("Reported ethnicity:", fill=TRUE)
print(table(short_summary.df$Race))

cat("Reported education:", fill=TRUE)
print(table(short_summary.df$Education))


#################
### OLD PLOTS ###
#################

# # # responses to questions within conditions across participants

# h_3 <- ggplot(SoA1.df, aes(x=Response_num, fill=Question, color=Question)) + 
#   geom_histogram(position="dodge")
# h_3 <- h_3 + facet_wrap(vars(SoA1.df$Condition)) +
#   xlab("Likert Response (1 = No Control, 6= Total Control)") + ylab("count") +
#   ggtitle("Response to Questions by Condition") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_x_continuous(breaks = seq(1,6, by = 1)) +
#   scale_y_continuous(breaks = seq(0,20, by = 3)) #+ scale_x_discrete(breaks=c("How much control did you feel over your action putting the item in the cart?",
#                                                                              "How much control did you feel over which item went into the cart?",
#                                                                              "How much control did you feel over the entire shopping trip?"),
#                                                                     labels=c("Action","Outcome","Overall"))
# print(h_3)

# # save file
# if (dataFileName == "~/SenseofAgency/data/raw/SoA1/SOA1_10Ps_firstpilot_11.8.2023.csv") {
#   ggsave(paste0(graphSaveDirectory, "hist_per_condandquest_11.8.jpg"))
# } else if (dataFileName == "~/SenseofAgency/data/raw/SoA1/SOA1_9Ps_11.9.2023.csv") {
#   ggsave(paste0(graphSaveDirectory, "hist_per_condandquest_11.9.jpg"))
# } else if (dataFileName == "~/SenseofAgency/data/raw/SoA1/SOA1_10Ps_pilot3_11.15.2023.csv") {
#   ggsave(paste0(graphSaveDirectory, "hist_per_condandquest_11.15.jpg"))
# } else if (dataFileName == "~/SenseofAgency/data/raw/SoA1/SOA1_drag_10Ps_12.1.2023.csv") {
#   ggsave(paste0(graphSaveDirectory, "hist_per_condandquest_12.1.jpg"))
# } else {
# }

# # responses by condition and participant - can't actually see anything in this one
# h_2 <- ggplot(SoA1.df, aes(x=Response_num)) + geom_histogram() +
#   facet_wrap(vars(SoA1.df$Condition,SoA1.df$Subject)) + 
#   scale_x_continuous(breaks = seq(1,6, by = 1)) +
#   scale_y_continuous(breaks = seq(0,15, by = 3)) +
#   xlab("Likert Response (1 = No Control, 6= Total Control)") + ylab("count") +
#   ggtitle("Response by Condition and Participant") +
#   theme(plot.title = element_text(hjust = 0.5))
# print(h_2)
# ggsave(paste0(graphSaveDirectory, "hist_per_condandP", dataDate, ".pdf"))

# ################################################
# ## plots comparing questions within condition ##
# ################################################
# 
# # ### earlier pilot version of Questions:
# # ## Questions <- c("To what degree did you feel like you were in control during the above shopping trip?","How much control did you feel over the item you wanted going into the cart?", "How much control did you feel over your actions as you put the item in the cart?")
# # 
# # Questions <- c("How much control did you feel over your actions (for example: your mouse clicks, hand movements, mouse movements)?",
# #                "How much control did you feel over the item you selected going into the cart?",
# #                "How much control did you feel over the entire shopping trip?")
# 
# 
# # PAPO expect to see high responses across the board
# SoA1.long %>%
#   filter(Condition == "PAPO") %>%
#   ##  filter(Question %in% Questions_v3) %>% ########## CHANGE THIS 
#   filter(Question %in% c("ActionControl", "OutcomeControl", "OverallControl")) %>% 
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Question, y=Response)) +
#   stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
#   xlab("Question") +
#   ylim(0, 7) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   #facet_wrap (~Trial) +
#   ggtitle("Predictable Action Predictable Outcome (Control Condition)") + 
#   scale_x_discrete(breaks=c(Questions_v3[2],Questions_v3[1],Questions_v3[3]), ## still need to change this
#                    labels=c("Action","Outcome","Overall"))
# ggsave(paste0(graphSaveDirectory, "PAPO_resp_all", dataDate, ".pdf"))
# 
# # PAUO expect to see high action, low outcome, low overall
# 
# SoA1.long %>%
#   filter(Condition == "PAUO") %>%
#   ##  filter(Question %in% Questions_v3) %>% ########## CHANGE THIS 
#   filter(Question %in% c("ActionControl", "OutcomeControl", "OverallControl")) %>% 
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Question, y=Response)) +
#   stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
#   ylim(0, 7) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   #facet_wrap (~Question) +
#   ggtitle("Predictable Action Unpredictable Outcome (Wrong Item in Cart)") + 
#   scale_x_discrete(breaks=c(Questions_v3[2],Questions_v3[1],Questions_v3[3]), ## still need to change this
#                    labels=c("Action","Outcome","Overall"))
# ggsave(paste0(graphSaveDirectory, "PAUO_resp_all", dataDate, ".pdf"))
# 
# # UAPO expect to see low action, high outcome, mid overall
# SoA1.long %>%
#   filter(Condition == "UAPO") %>%
#   ##  filter(Question %in% Questions_v3) %>% ##### CHANGE THIS
#   filter(Question %in% c("ActionControl", "OutcomeControl", "OverallControl")) %>% 
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Question, y=Response)) +
#   stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
#   ylim(0, 7) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   #facet_wrap (~Question) +
#   ggtitle("Unpredictable Action Predictable Outcome (Wrong Item in Cart)") + 
#   scale_x_discrete(breaks=c(Questions_v3[2],Questions_v3[1],Questions_v3[3]), ## still need to change this
#                    labels=c("Action","Outcome","Overall"))
# ggsave(paste0(graphSaveDirectory, "UAPO_resp_all", dataDate, ".pdf"))

# #############################################################
# ## plots comparing questions within condition and by trial ##
# ############################################################
# 
# # PAPO expect to see high responses across the board
# SoA1.long %>%
#   filter(Condition == "PAPO") %>%
#   ### NOTE that with a within subject trials range from 1-15 not 1-5
#   mutate(Trial = case_when(Trial == 6 ~ 1,
#                            Trial == 10 ~ 5,
#                            Trial == 11 ~ 1,
#                            Trial == 15 ~ 5,
#                            TRUE ~ Trial)) %>%
#   filter(Trial == 1 | Trial == 5) %>%
#   ##  filter(Question %in% Questions_v3) %>% ##### CHANGE THIS
#   filter(Question %in% c("ActionControl", "OutcomeControl", "OverallControl")) %>% 
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Question, y=Response)) +
#   facet_wrap (~Trial) +
#   stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2)  +
#   xlab("Question") +
#   ylim(0, 7) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   ggtitle("Predictable Action Predictable Outcome (Control) by Trial") + 
#   scale_x_discrete(breaks=c(Questions_v3[2],Questions_v3[1],Questions_v3[3]), ## still need to change this
#                    labels=c("Action","Outcome","Overall"))
# ggsave(paste0(graphSaveDirectory, "PAPO_respbytrial", dataDate, ".pdf"))
# 
# # PAUO expect to see high action, low outcome, low overall
# 
# SoA1.long %>%
#   filter(Condition == "PAUO") %>%
#   ### NOTE that with a within subject trials range from 1-15 not 1-5
#   mutate(Trial = case_when(Trial == 6 ~ 1,
#                            Trial == 10 ~ 5,
#                            Trial == 11 ~ 1,
#                            Trial == 15 ~ 5,
#                            TRUE ~ Trial)) %>%
#   filter(Trial == 1 | Trial == 5) %>%
#   ##  filter(Question %in% Questions_v3) %>%
#   filter(Question %in% c("ActionControl", "OutcomeControl", "OverallControl")) %>% 
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Question, y=Response)) +
#   stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
#   xlab("Question") +
#   ylim(0, 7) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   facet_wrap (~Trial) +
#   ggtitle("Predictable Action Unpredictable Outcome (Wrong Item) by Trial") + 
#   scale_x_discrete(breaks=c(Questions_v3[2],Questions_v3[1],Questions_v3[3]), ## still need to change this
#                    labels=c("Action","Outcome","Overall"))
# ggsave(paste0(graphSaveDirectory, "PAUO_respbytrial", dataDate, ".pdf"))
# 
# # UAPO expect to see low action, high outcome, mid overall
# SoA1.long %>%
#   filter(Condition == "UAPO") %>%
#   ### NOTE that with a within subject trials range from 1-15 not 1-5
#   mutate(Trial = case_when(Trial == 6 ~ 1,
#                            Trial == 10 ~ 5,
#                            Trial == 11 ~ 1,
#                            Trial == 15 ~ 5,
#                            TRUE ~ Trial)) %>%
#   filter(Trial == 1 | Trial == 5) %>%
#   ##  filter(Question %in% Questions_v3) %>%
#   filter(Question %in% c("ActionControl", "OutcomeControl", "OverallControl")) %>% 
#   mutate(Response = as.numeric(Response)) %>%
#   ggplot(aes(x=Question, y=Response)) +
#   stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
#   xlab("Question") +
#   ylim(0, 7) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   facet_wrap (~Trial) +
#   ggtitle("Unpredictable Action Predictable Outcome (Mouse) by Trial") + 
#   scale_x_discrete(breaks=c(Questions_v3[2],Questions_v3[1],Questions_v3[3]), ## still need to change this
#                    labels=c("Action","Outcome","Overall"))
# ggsave(paste0(graphSaveDirectory, "UAPO_respbytrial", dataDate, ".pdf"))



## cor matrix of all vars 
#round(cor(dplyr::select(MPScore.wide, !c(Subject, Video))), digits=2)

### pairwise correlations of all variables can be fun
## require(GGally)
## ggpairs(dplyr::select(SoA1.wide, !c(Subject, Video)))
## ggsave(paste0(graphSaveDirectory, "SoApairs.pdf"))

### add any overall means here if needed
## Means.df <- MPScore.wide %>%
##     group_by(Video) %>%
##     summarise(RSIV = mean(TTCMItemMean),
##               InnerLife = mean(PCInnerLife),
##               Reflect = mean(PCReflect),
##               DirectC = mean(PCDirectC)) %>%
##     arrange(RSIV)

## print(Means.df)