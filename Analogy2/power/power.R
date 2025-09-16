
library(pwr)

# effect size
w <- 0.3

# Chi-square power calculation
pwr.chisq.test(w = w, df = 1, sig.level = 0.05, power = 0.8)