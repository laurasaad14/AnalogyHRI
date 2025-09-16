####################################
## EDA:  Exploratory Data Analysis #
####################################

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

ggplot(counts_df, aes(x = conPos1, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +  # counts above bars
  theme_minimal() +
  labs(title = "P Chose Connected Robot More Similar to Human", x = "Connected in Position 1?", y = "Frequency")
ggsave(paste0(graphSaveDirectory, "DidPChooseConnected", dataDate, ".pdf"))




# Convert frequency column into a vector
counts_vector <- counts_df$Frequency
names(counts_vector) <- counts_df$conPos1

# Run chi-square test
chi_result <- chisq.test(counts_vector)

# Print results
print(chi_result)



# plot whether Ps chose connected robot as more likely to have emotion 

# Count TRUE/FALSE per condition
counts_connectedEMO <- table(dat_first$conPos1EMO)
print(counts_connectedEMO)

# Plot 
countsEMO_df <- as.data.frame(counts_connectedEMO)
names(countsEMO_df) <- c("conPos1EMO", "Frequency")

ggplot(countsEMO_df, aes(x = conPos1EMO, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +  # counts above bars
  theme_minimal() +
  labs(title = "P Chose Connected Robot More Likely to have Emotion", x = "Connected in Position 1?", y = "Frequency")
ggsave(paste0(graphSaveDirectory, "DidPChooseConnectedEMO", dataDate, ".pdf"))


# Convert frequency column into a vector
countsEMO_vector <- countsEMO_df$Frequency
names(countsEMO_vector) <- countsEMO_df$conPos1EMO

# Run chi-square test
chi_resultEMO <- chisq.test(countsEMO_vector)

# Print results
print(chi_resultEMO)


# plot custom items across conditions
custom_scale <- c("smell", "taste", "touch", "see", "hear", "walk")

Analogy2.df <- Analogy2.df %>%
  rowwise() %>%
  mutate(custom_mean = mean(c_across(custom_scale), na.rm = TRUE)) %>%
  ungroup()

Analogy2.df %>%
  filter(condition == "connected" | condition == "disconnected") %>%
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

# plot response to each custom item

custom_scale <- c("smell", "taste", "touch", "see", "hear", "walk")

Analogy2.long <- Analogy2.df %>%
  pivot_longer(
    cols = all_of(custom_scale),
    names_to = "item",
    values_to = "score"
  )

Analogy2.long %>%
  filter(condition == "connected" | condition == "disconnected") %>%
  ggplot(aes(x = condition, y = score)) +
  stat_summary(fun = "mean", geom = "bar", fill = "gray") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2) +
  xlab("Condition") +
  ylim(0, 7) +
  facet_wrap(~ item) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    text = element_text(size = 22),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Custom Items")

ggsave(paste0(graphSaveDirectory, "custom_byConditionByItem", dataDate, ".pdf"))









