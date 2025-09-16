
library(pwr)

# RUN stats.R before this to pull in relevant info

N <- sum(countsEMO_vector)   # e.g., TRUE + FALSE counts
wEMO <- sqrt(chi_resultEMO$statistic / N)
wEMO # effect size for emotion ranking

# Chi-square power calculation
pwr.chisq.test(w = wEMO, df = 1, sig.level = 0.05, power = 0.8)


N <- sum(counts_vector)   # e.g., TRUE + FALSE counts
w <- sqrt(chi_result$statistic / N)
w # effect size for emotion ranking

# Chi-square power calculation
pwr.chisq.test(w = w, df = 1, sig.level = 0.05, power = 0.8)
