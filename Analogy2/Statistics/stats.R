###########
## STATS ##
###########

########################
### Experiment setup ###
########################

experimentName <- "Analogy2"
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



# plot whether Ps chose connected robot as more similar to human 

dat_first <- Analogy2.df %>%
  group_by(subjID) %>%
  slice_head(n = 1) %>%
  ungroup()

# Count TRUE/FALSE per condition
counts_connected <- table(dat_first$conPos1)
print(counts_connected)

# Plot 
counts_df <- as.data.frame(counts_connected)
names(counts_df) <- c("conPos1", "Frequency")


# Convert frequency column into a vector
counts_vector <- counts_df$Frequency
names(counts_vector) <- counts_df$conPos1

# Run chi-square test
chi_result <- chisq.test(counts_vector)

# Print results
print(chi_result)


# emotion ranking
counts_connectedEMO <- table(dat_first$conPos1EMO)
print(counts_connectedEMO)

countsEMO_df <- as.data.frame(counts_connectedEMO)
names(countsEMO_df) <- c("conPos1EMO", "Frequency")


# Convert frequency column into a vector
countsEMO_vector <- countsEMO_df$Frequency
names(countsEMO_vector) <- countsEMO_df$conPos1EMO

chi_resultEMO <- chisq.test(countsEMO_vector)
print(chi_resultEMO)


############# correlations between trait level measures and rank order data

# recode true/false
rank_sim_recode <- as.numeric(dat_first$conPos1) # similarity ranking - trues = connected in position 1
rank_emo_recode <- as.numeric(dat_first$conPos1EMO) # likely to have emotions - trues = connected in position 1

# get means of all trait level variables
#GATORS P-
dat_first <- dat_first %>%
  mutate(GATORS_mean = rowMeans(select(., starts_with("GATORS.")), na.rm = TRUE))

# NARS
dat_first <- dat_first %>%
  mutate(NARS_mean = rowMeans(select(., starts_with("NARS")), na.rm = TRUE))

# RAS
dat_first <- dat_first %>%
  mutate(RAS_mean = rowMeans(select(., starts_with("RAS")), na.rm = TRUE))


# what is relationship between GATORS P- and whether Ps picked connected as more similar to Sarah

cor_GP_sim <- cor.test(rank_sim_recode, dat_first$GATORS_mean, method = "pearson")
print(cor_GP_sim)

# plot
ggplot(dat_first, aes(x = rank_sim_recode, y = GATORS_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Rank Similarity (Recode)",
    y = "GATORS P- Mean",
    title = paste0("Correlation Between GATORS and Similarity Ranking = ", round(cor_GP_sim$estimate, 2),
                   ", p = ", signif(cor_GP_sim$p.value, 3))
  )
ggsave(paste0(graphSaveDirectory, "cor_GP_similarity", dataDate, ".pdf"))

# what is relationship between NARS and whether Ps picked connected as more similar to Sarah

cor_NARS_sim <- cor.test(rank_sim_recode, dat_first$NARS_mean, method = "pearson")
print(cor_NARS_sim)

# plot
ggplot(dat_first, aes(x = rank_sim_recode, y = NARS_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Rank Similarity (Recode)",
    y = "NARS Mean",
    title = paste0("Correlation Between NARS and Similarity Ranking = ", round(cor_NARS_sim$estimate, 2),
                   ", p = ", signif(cor_NARS_sim$p.value, 3))
  )
ggsave(paste0(graphSaveDirectory, "cor_NARS_similarity", dataDate, ".pdf"))

# what is relationship between RAS and whether Ps picked connected as more similar to Sarah

cor_RAS_sim <- cor.test(rank_sim_recode, dat_first$RAS_mean, method = "pearson")
print(cor_RAS_sim)

# plot
ggplot(dat_first, aes(x = rank_sim_recode, y = RAS_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Rank Similarity (Recode)",
    y = "RAS Mean",
    title = paste0("Correlation Between RAS and Similarity Ranking = ", round(cor_RAS_sim$estimate, 2),
                   ", p = ", signif(cor_RAS_sim$p.value, 3))
  )
ggsave(paste0(graphSaveDirectory, "cor_RAS_similarity", dataDate, ".pdf"))


# what is relationship between GATORS P- and whether Ps picked connected as more likely to have emotions

cor_GP_emo <- cor.test(rank_emo_recode, dat_first$GATORS_mean, method = "pearson")
print(cor_GP_emo)

# plot
ggplot(dat_first, aes(x = rank_emo_recode, y = GATORS_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Rank Emotion (Recode)",
    y = "GATORS P- Mean",
    title = paste0("Correlation Between GATORS and Emotion Ranking = ", round(cor_GP_emo$estimate, 2),
                   ", p = ", signif(cor_GP_emo$p.value, 3))
  )
ggsave(paste0(graphSaveDirectory, "cor_GP_emotion", dataDate, ".pdf"))

# what is relationship between NARS and whether Ps picked connected as more likely to have emotions

cor_NARS_emo <- cor.test(rank_emo_recode, dat_first$NARS_mean, method = "pearson")
print(cor_NARS_emo)

# plot
ggplot(dat_first, aes(x = rank_emo_recode, y = NARS_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Rank Emotion (Recode)",
    y = "NARS Mean",
    title = paste0("Correlation Between NARS and Emotion Ranking = ", round(cor_NARS_emo$estimate, 2),
                   ", p = ", signif(cor_NARS_emo$p.value, 3))
  )
ggsave(paste0(graphSaveDirectory, "cor_NARS_emotion", dataDate, ".pdf"))

# what is relationship between RAS and whether Ps picked connected as more likely to have emotions

cor_RAS_emo <- cor.test(rank_emo_recode, dat_first$RAS_mean, method = "pearson")
print(cor_RAS_emo)

# plot
ggplot(dat_first, aes(x = rank_emo_recode, y = RAS_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Rank Emotion (Recode)",
    y = "RAS Mean",
    title = paste0("Correlation Between RAS and Emotion Ranking = ", round(cor_RAS_emo$estimate, 2),
                   ", p = ", signif(cor_RAS_emo$p.value, 3))
  )
ggsave(paste0(graphSaveDirectory, "cor_RAS_emotion", dataDate, ".pdf"))


cor.test(dat_first$GATORS_mean, dat_first$RAS_mean, type = "pearson")
cor.test(dat_first$GATORS_mean, dat_first$NARS_mean, type = "pearson")
cor.test(dat_first$NARS_mean, dat_first$RAS_mean, type = "pearson")

############# which scale predicts the ranking better?


# similarity first
model_Gators_sim <- glm(rank_sim_recode ~ GATORS_mean, data = dat_first, family = binomial)
model_RAS_sim <- glm(rank_sim_recode ~ RAS_mean, data = dat_first, family = binomial)
model_NARS_sim <- glm(rank_sim_recode ~ NARS_mean, data = dat_first, family = binomial)

# model comparison - lower is better
AIC(model_Gators_sim, model_RAS_sim, model_NARS_sim)
BIC(model_Gators_sim, model_RAS_sim, model_NARS_sim)

# variance explained - look at r2CU value - higher is better
library(pscl)
pR2(model_Gators_sim)
pR2(model_RAS_sim)
pR2(model_NARS_sim)

# plot area under the curve - greater AUC = better fit
library(pROC)

roc1 <- roc(rank_sim_recode, fitted(model_Gators_sim))
roc2 <- roc(rank_sim_recode, fitted(model_RAS_sim))
roc3 <- roc(rank_sim_recode, fitted(model_NARS_sim))

plot(roc1, col="red")
plot(roc2, col="blue", add=TRUE)
plot(roc3, col="green", add=TRUE)

auc(roc1); auc(roc2); auc(roc3)


# emotion ranking
model_Gators_emo <- glm(rank_emo_recode ~ GATORS_mean, data = dat_first, family = binomial)
model_RAS_emo <- glm(rank_emo_recode ~ RAS_mean, data = dat_first, family = binomial)
model_NARS_emo <- glm(rank_emo_recode ~ NARS_mean, data = dat_first, family = binomial)

# model comparison - lower is better
AIC(model_Gators_emo, model_RAS_emo, model_NARS_emo)
BIC(model_Gators_emo, model_RAS_emo, model_NARS_emo)

# variance explained - look at r2CU value - higher is better
pR2(model_Gators_emo)
pR2(model_RAS_emo)
pR2(model_NARS_emo)

# plot area under the curve - greater AUC = better fit

roc1 <- roc(rank_emo_recode, fitted(model_Gators_emo))
roc2 <- roc(rank_emo_recode, fitted(model_RAS_emo))
roc3 <- roc(rank_emo_recode, fitted(model_NARS_emo))

plot(roc1, col="red")
plot(roc2, col="blue", add=TRUE)
plot(roc3, col="green", add=TRUE)

auc(roc1); auc(roc2); auc(roc3)

