
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
print(paste("Avg. negative text grade: " , mean(negative_grades))) # 52.50

# Print standard deviation
print(paste("Avg. positive text grade: " , sd(positive_grades))) # 18.30
print(paste("Avg. neutral text grade: " , sd(neutral_grades)))   # 20.10
print(paste("Avg. negative text grade: " , sd(negative_grades))) # 28.63



####################### Plot ########################

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
ggsave("rain_cloud_plot.png", my_plot, width = 5, height = 5)



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






############## ANCOVA assumptions ##################

# Linear relationship between independent/covariates and dependent variable

### Grade vs age ###
par(mfrow=c(1,1))
plot(D$Age, D$grade, col = "black", pch = 8, cex = 2, main = "grade vs age")
# Add linear regression line
abline(lm(D$grade ~ D$Age), col = "red")
# Add the loess line
loess_fit <- loess(D$grade ~ D$Age)
predicted_values <- predict(loess_fit, newdata = data.frame(Age = D$Age))
lines(D$Age, predicted_values, col = "blue")
# Calculate correlation p-value
cor.test(D$Age, D$grade)$p.value # 0.9176092
# Non significant correlation

# Without the age outlier
ages <- D$Age[D$Age < 40]
grades <- D$grade[-length(D$grade)]
plot(ages, grades, col = "black", pch = 8, cex = 2, main = "grade vs age")
# Add linear regression line
abline(lm(grades ~ ages), col = "red")
# Add the loess line
loess_fit <- loess(grades ~ ages)
predicted_values <- predict(loess_fit, newdata = data.frame(Age = ages))
lines(ages, predicted_values, col = "blue")
# Calculate correlation p-value
cor.test(ages, grades)$p.value # 0.8904612
# Non significant correlation

# Both look very non-linear and with no clear relationship between the variables
# Correlation p-values are also non-significant

### Grade vs gender ###
# Rain cloud plot
# Define colors based on Gender
D$color <- ifelse(D$Gender == "Male", "turquoise2",
                  ifelse(D$Gender == "Female", "plum1", "orange2"))
# Create a rain cloud plot with ggrain
my_plot <- ggplot(D, aes(Gender, grade, fill = color)) +
  geom_rain(alpha = 0.5) +
  scale_fill_identity() +  # Use identity scale for manual colors
  theme_classic() +
  guides(fill = 'none', color = 'none') +
  coord_flip() +
  labs(x = "Gender", y = "Grade")
# Save the plot as an image (adjust the filename and extension as needed)
ggsave("gender_vs_grade.png", my_plot, width = 5, height = 5)


##################################################################
male_grades <- D$grade[D$Gender == "Male"]
female_grades <- D$grade[D$Gender == "Female"]
other_grades <- D$grade[D$Gender == "Other"]
# Boxplot
# Create an empty plot with the desired range of the y-axis
plot(1, type = "n", xlim = c(0, 4), ylim = c(0, 100), xlab = "Text sentiment", ylab = "Grade in %", xaxt = "n")
# Create the boxplots 
boxplot(male_grades, at = 1, add = TRUE, col = "turquoise2")
boxplot(female_grades, at = 2, add = TRUE, col = "plum1")
boxplot(other_grades, at = 3, add = TRUE, col = "orange2")
# Add the individual data points with random jitter
jittered_x1 <- jitter(rep(1, length(male_grades)), factor = 1.5)
jittered_x2 <- jitter(rep(2, length(female_grades)), factor = 1.5)
jittered_x3 <- jitter(rep(3, length(other_grades)), factor = 1.5)
points(jittered_x1, male_grades, pch = 21, col = "black")
points(jittered_x2, female_grades, pch = 21, col = "black")
points(jittered_x3, other_grades, pch = 21, col = "black")
# Add the axis labels, ticks and title
axis(1, at = 1:3, labels = c("Male", "Female", "Other"), las = 1)
axis(2, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))
title("Boxplots of grades")
######################################################################

# The "male" and "female" grades do not differ significantly fro each other
# and the "other" grades have too few data points to be comparable. 


# Overall, the linearity assumption is not met in a high enough degree
# to validate utilizing ANCOVA.



# Hvis man alligevel gør det:
################ Examining covariates with ANCOVA ################### 

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





############## GLM ##################
# Let's try another kind of model like the General Linear Model:

### assumptions ###
# linearity, homoskedasticity (constant variance), normality, and independence
  # the general linear model assumes that the relationships between the 
  # outcome and any continuous predictors are linear. 

# Based on the quiz score vs age plot from prev. investigation, the linearity assumption
# is not met.
# If we perform it anyhow:

### GLM ###
glm_model <- glm(grade ~ Text_version + Age + Gender, data = D, family = gaussian)
summary(glm_model)

"""
Coefficients:
                     Estimate Std. Error t value Pr(>|t|)  
(Intercept)           46.3979    18.5345   2.503   0.0153 *
Text_versionNeutral    1.6412     7.3992   0.222   0.8253
Text_versionPositive   7.2165     7.6467   0.944   0.3494
Age                    0.2332     0.7053   0.331   0.7422
GenderMale             2.5224     6.4848   0.389   0.6988  
GenderOther          -32.2274    24.4446  -1.318   0.1928
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for gaussian family taken to be 555.5776)

    Null deviance: 32492  on 60  degrees of freedom
Residual deviance: 30557  on 55  degrees of freedom
AIC: 566.32

Number of Fisher Scoring iterations: 2 
"""
# None of the predictors (Text_version, Age, Gender) significantly influence quiz scores
# The model as a whole also might not be a good fit based on the null and residual deviance values