
library(pwr)

# Set parameters
alpha <- 0.05  # Significance level
power <- 0.80  # Desired power
#effect_size <- 0.2  # Cohen's d effect size (small effect)
effect_size <- 0.5  # Cohen's d effect size (medium effect)
#effect_size <- 0.8  # Cohen's d effect size (large effect)


# Number of groups (sentiments)
num_groups <- 3

# Perform power analysis for one-way ANOVA
result <- pwr.anova.test(k = num_groups, f = effect_size, sig.level = alpha, power = power)

# Display the result
result
"""
Small effect size:
  n = 81.296 = 82 participants in each group 
  246 in total.

Medium effect size:
  n = 13.895 = 14 in each group
  42 in total.
  
Large effect size:
  n = 6.136 = 7 in each group
  21 in total.
"""