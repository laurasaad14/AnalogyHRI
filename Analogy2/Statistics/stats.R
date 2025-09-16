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
