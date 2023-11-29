
####################### Load data ########################

# Jesper:
#D <- read.csv("/Users/jesperberglund/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")
# Chelina:
D <- read.csv("C:/Users/cheli/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")

neutral_grades <- D$grade[D$Text_version == "Neutral"]
positive_grades <- D$grade[D$Text_version == "Positive"]
negative_grades <- D$grade[D$Text_version == "Negative"]


# Print average grades
print(paste("Avg. positive text grade: " , mean(positive_grades))) # 60.56
print(paste("Avg. neutral text grade: " , mean(neutral_grades)))   # 55.26
print(paste("Avg. negative text grade: " , mean(negative_grades))) # 52.5



####################### Outlier detection ########################

# calculate mean and standard deviation for each variable
mean_positive <- mean(positive_grades)
sd_positive <- sd(positive_grades)
mean_negative <- mean(negative_grades)
sd_negative <- sd(negative_grades)
mean_neutral <- mean(neutral_grades)
sd_neutral <- sd(neutral_grades)

# identify outliers for each text version based on normal distribution
for (i in 1:length(positive_grades)) {
  if (abs(positive_grades[i] - mean_positive) > 3 * sd_positive) {
    print(paste("Positive Text outliers: ", positive_grades[i]))
  }
}
for (i in 1:length(negative_grades)) {
  if (abs(negative_grades[i] - mean_negative) > 3 * sd_negative) {
    print(paste("Negative Text outliers: ", negative_grades[i]))
  }
}
for (i in 1:length(neutral_grades)) {
  if (abs(neutral_grades[i] - mean_neutral) > 3 * sd_neutral) {
    print(paste("Neutral Text outliers: ", neutral_grades[i]))
  }
}
# There are no outliers with our limited data:
  # If we have 10 data points and one is very low then that is 10% of our
  # data being low. If we had 100 points then it would be 1%. This changes
  # how many points would be considered outliers.



####################### Plots ########################

# Rain cloud plot
library(ggplot2)
library(ggrain)
# Define colors based on Text_version
D$color <- ifelse(D$Text_version == "Positive", "#64cb6b",
                  ifelse(D$Text_version == "Neutral", "skyblue", "coral"))
# Create a rain cloud plot with ggrain
my_plot <- ggplot(D, aes(Text_version, grade, fill = color)) +
  geom_rain(alpha = 0.5) +
  scale_fill_identity() +  # Use identity scale for manual colors
  theme_classic() +
  guides(fill = 'none', color = 'none') +
  coord_flip() +
  labs(x = "Text Version", y = "Grade")
# Save the plot as an image (adjust the filename and extension as needed)
ggsave("rain_cloud_plot.png", my_plot, width = 8, height = 8)




####################### Normallity ########################

# Shapiro-Wilk test

shapiro.test(positive_grades) # p-value = 0.3079
# Not enough evidence to reject data being normally distributed
shapiro.test(neutral_grades)  # p-value = 0.3167
# Not enough evidence to reject data being normally distributed
shapiro.test(negative_grades) # p-value = 0.02389
# Evidence that data is NOT normally distributed

# At least one of the groups are NOT normally distributes so we
# cannot use ANOVA



####################### Statistics ########################

# Kruskal-Wallis test
result_kruskal <- kruskal.test(grade ~ Text_version, data = D)
result_kruskal$p.value # p-value = 0.7800437
# p > 0.05 = don't have sufficient reason to believe that the 
# choice of text version has a statistically significant effect 
# on the grades.


# Calculate effect size
Group1 = positive_grades
Group2 = neutral_grades
Group3 = negative_grades
# Create all possible pairs of groups
group_pairs <- list(c("Group1", "Group2"), c("Group1", "Group3"), c("Group2", "Group3"))
# Initialize an empty vector to store Cohen's d for each pair
cohen_d_values <- numeric(length(group_pairs))
# Calculate Cohen's d for each pair of groups
for (i in seq_along(group_pairs)) {
  group_pair <- group_pairs[[i]]
  mean_diff <- mean(get(group_pair[1])) - mean(get(group_pair[2]))
  pooled_sd <- sqrt(((length(get(group_pair[1])) - 1) * sd(get(group_pair[1]))^2 + 
                       (length(get(group_pair[2])) - 1) * sd(get(group_pair[2]))^2) / 
                      (length(get(group_pair[1])) + length(get(group_pair[2])) - 2))
  cohen_d_values[i] <- mean_diff / pooled_sd
}
# Average Cohen's d over all pairs
average_cohen_d <- mean(cohen_d_values, na.rm = TRUE)
# Print the result
average_cohen_d # 0.2365371 = small effect



################ Examining covariates with ANCOVA ################### 

# An important assumption is residuals being normally distributed.

# QQ-plot to investigate normality of residuals
library(car)
# QQ-plot of sentiment separation
png(file = "QQplot_text_sep.png", units = "cm",
    res = 1200, height = 1200 / 72, width = 1200 / 72)
par(mfrow=c(1,3))
qqPlot(positive_grades, ylab = "Given grades in %")
title("Positive Text", line = 0.5, cex.main = 0.8) # Add a subtitle
qqPlot(neutral_grades, ylab = "Given grades in %")
title("Neutral Text", line = 0.5, cex.main = 0.8) # Add a subtitle
mtext("QQ plot for grades", side = 3, line = 2)
qqPlot(negative_grades, ylab = "Given grades in %")
title("Negative Text", line = 0.5, cex.main = 0.8) # Add a subtitle
dev.off()
# They seem normally distributed. Let's continue.

# Fit the ANCOVA model
ancova_model <- lm(grade ~ Text_version + Age + Gender, data = D)
summary(ancova_model)
"""
Residuals:
    Min      1Q  Median      3Q     Max 
-54.517 -15.216   4.075  17.064  35.950 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)  
(Intercept)           46.3979    18.5345   2.503   0.0153 *
Text_versionNeutral    1.6412     7.3992   0.222   0.8253  
Text_versionPositive   7.2165     7.6467   0.944   0.3494  
Age                    0.2332     0.7053   0.331   0.7422  
GenderMale             2.5224     6.4848   0.389   0.6988  
GenderOther          -32.2274    24.4446  -1.318   0.1928  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 23.57 on 55 degrees of freedom
Multiple R-squared:  0.05955,	Adjusted R-squared:  -0.02594 
F-statistic: 0.6966 on 5 and 55 DF,  p-value: 0.6283
"""
# No variables has a significant effect on grade
# If ANCOVA shows no significant effect, it 
  # suggests that, after controlling for the covariates, 
  # there is no significant difference in the dependent 
  # variable across the groups being compared.


