import statsmodels.api as sm
from statsmodels.stats.power import FTestAnovaPower

# Set your parameters
effect_size = 0.5  # Cohen's d for a medium effect size
#effect_size = 0.8  # Cohen's d for a large effect size
alpha = 0.05      # Significance level
power = 0.80      # Desired statistical power
num_groups = 3    # Number of groups (sentiment versions)

# Create an instance of the FTestAnovaPower class
power_analysis = FTestAnovaPower()

# Perform the power analysis
sample_size = power_analysis.solve_power(effect_size=effect_size, alpha=alpha, nobs=None, power=power, k_groups=num_groups)

print(f"Required Sample Size per Group: {sample_size:.2f}")

# For medium effect size: n = 41.69 per group = 126 total
# For large effect size: n = 18.41 per group = 57 total