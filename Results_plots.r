
############# Load data ################
# Read data
# Jesper:
D <- read.csv("/Users/jesperberglund/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")
# Chelina:
#D <- read.csv("C:/Users/cheli/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")

neutral_grades <- D$grade[D$Text_version == "Neutral"]
positive_grades <- D$grade[D$Text_version == "Positive"]
negative_grades <- D$grade[D$Text_version == "Negative"]

# Print average grades
print(paste("Avg. positive text grade: " , mean(positive_grades))) # 60.56
print(paste("Avg. neutral text grade: " , mean(neutral_grades)))   # 55.26
print(paste("Avg. negative text grade: " , mean(negative_grades))) # 52.5


############# Plots ################
# Load required libraries
library(ggplot2)
library(ggrain)

# Assuming your data frame is named D with columns 'grade' and 'Text_version'
# Replace this with the actual names in your dataset

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















# Box plos of grades within each text sentiment
png(file="Boxplot_text_sep.png",
    units="cm",res=1200,height = 1200/72,
    width=1200/72)
# Create an empty plot with the desired range of the y-axis
plot(1, type = "n", xlim = c(0, 4), ylim = c(0, 100), xlab = "Text sentiment", ylab = "Grade in %", xaxt = "n")
# Create the boxplots
boxplot(positive_grades, at = 1, add = TRUE, col = "#64cb6b")
boxplot(neutral_grades, at = 2, add = TRUE, col = "skyblue")
boxplot(negative_grades, at = 3, add = TRUE, col = "coral")
# Add the individual data points with random jitter
jittered_x1 <- jitter(rep(1, length(positive_grades)), factor = 1.5)
jittered_x2 <- jitter(rep(2, length(neutral_grades)), factor = 1.5)
jittered_x3 <- jitter(rep(3, length(negative_grades)), factor = 1.5)
points(jittered_x1, positive_grades, pch = 21, col = "black")
points(jittered_x2, neutral_grades, pch = 21, col = "black")
points(jittered_x3, negative_grades, pch = 21, col = "black")
# Add the axis labels, ticks and title
axis(1, at = 1:3, labels = c("Positive", "Neutral", "Negative"), las = 1)
axis(2, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))
title("Boxplots of grades")
dev.off()


# Bar plot of all grades
possible_grades <- seq(0, 100, by = 10)
# Calculate the number of students for each grade in Possible_grades
Number_of_students <- sapply(possible_grades, function(grade) sum(D$grade == grade))
# Plot
png(file="barplot.png",
    units="cm",res=1200,height = 1200/72,
    width=1200/72)
par(mfrow = c(1,1))
barplot(Number_of_students, names.arg = possible_grades, col = "royalblue", 
        xlab = "Grades in %", ylab = "Number of students", 
        main = "Number of students given each grade")
dev.off()


# Bar plot of grades within each text sentiment
# TAKE 1 #
positive_students <- sapply(possible_grades, function(grade) sum(positive_grades == grade))
neutral_students <- sapply(possible_grades, function(grade) sum(neutral_grades == grade))
negative_students <- sapply(possible_grades, function(grade) sum(negative_grades == grade))
# Create the plots
png(file="barplot_text_sep.png",
    units="cm",res=1200,height = 1200/72,
    width=1200/72)
par(mfrow = c(1,3))
barplot(positive_students, names.arg = possible_grades, col = "#64cb6b", 
        xlab = "Grades in %", ylab = "Number of students", ylim = c(0,5))
title("Positive Text", line = 0.5, cex.main = 0.8) # Add a subtitle
barplot(neutral_students, names.arg = possible_grades, col = "skyblue", 
        xlab = "Grades in %", 
         main = "Barplot of grades", ylim = c(0,5))
title("Neutral Text", line = 0.5, cex.main = 0.8) # Add a subtitle
barplot(neutral_students, names.arg = possible_grades, col = "coral", 
        xlab = "Grades in %", ylim = c(0,5))
title("Negative Text", line = 0.5, cex.main = 0.8) # Add a subtitle
dev.off()


# Histogram of grades within each sentiment
png(file="histogram_text_sep.png",
    units="cm",res=1200,height = 1200/72,
    width=1200/72)
par(mfrow=c(1,3))
c(hist(positive_grades, breaks=5, col="#64cb6b", freq = FALSE, xlab = "Grades in %", 
       main = " ", xlim = c(0, 100)),
  axis(1, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)),
  title("Positive Text", line = 0.5, cex.main = 0.8), # Add a subtitle
  curve(dnorm(x, mean=mean(D$grade),sd=sd(D$grade)), add = TRUE, col = "black", lwd = 2),
  hist(neutral_grades, breaks=10, col="skyblue", freq = FALSE, xlab = "Grades in %", 
       main = "Histogram of grades", xlim = c(0, 100)),
  axis(1, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)),
  title("Neutral Text", line = 0.5, cex.main = 0.8), # Add a subtitle
  curve(dnorm(x, mean=mean(neutral_grades),sd=sd(neutral_grades)), add = TRUE, col = "black", lwd = 2),
  hist(negative_grades, breaks=10, col="coral", freq = FALSE, xlab = "Grades in %", 
       main = " ", xlim = c(0, 100)),
  axis(1, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)),
  title("Negative Text", line = 0.5, cex.main = 0.8), # Add a subtitle
  curve(dnorm(x, mean=mean(negative_grades),sd=sd(negative_grades)), add = TRUE, col = "black", lwd = 2))
dev.off() 





# QQ-plot to investigate normality
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





############# SW test ################
# Shapiro-Wilk test

shapiro.test(positive_grades) # p-value = 0.3079
# Not enough evidence to reject data being normally distributed
shapiro.test(neutral_grades)  # p-value = 0.3167
# Not enough evidence to reject data being normally distributed
shapiro.test(negative_grades) # p-value = 0.02389
# Evidence that data is NOT normally distributed

# At least one of the groups are NOT normally distributes so we
# cannot use ANOVA



############# Outlier detection ##############

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
# There are no outliers with our limited data 
  # If we have 10 data points and one i very low then that is 10% of our
  # data being low. If we had 100 points then it would be 1%. This changes
  # how many points would be considered outliers.


# Plot grades to examine the outliers visually
par(mfrow=c(1,1))
png(file = "scatterplot_positive.png", units = "cm",
    res = 1200, height = 1200 / 72, width = 1200 / 72)
plot(positive_grades, col = "white", pch = 8, cex = 2, main = "Scatterplot of positive grades")
points(positive_grades, pch = 21, bg = "#64cb6b", col = "black", cex = 2)
dev.off()
# No clear outliers
png(file = "scatterplot_neutral.png", units = "cm",
    res = 1200, height = 1200 / 72, width = 1200 / 72)
plot(neutral_grades, col = "white", pch = 8, cex = 2, main = "Scatterplot of neutral grades")
points(neutral_grades, pch = 21, bg = "skyblue", col = "black", cex = 2)
dev.off()
# No clear outliers
png(file = "scatterplot_negative.png", units = "cm",
    res = 1200, height = 1200 / 72, width = 1200 / 72)
plot(negative_grades, col = "white", pch = 8, cex = 2, main = "Scatterplot of negative grades")
points(negative_grades, pch = 21, bg = "coral", col = "black", cex = 2)
dev.off()
# No clear outliers






############# Statistics ################

#### If NOT normally distributed
result_kruskal <- kruskal.test(grade ~ Text_version, data = D)
result_kruskal$p.value # p-value = 0.7800437
# p > 0.05 = don't have sufficient reason to believe that the 
# choice of text version has a statistically significant effect 
# on the grades.

# Bonferonni correction
# ONLY PERFORM IF KRUSKAL P-VALUE < 0.05 ##### SO NOT #####
#pairwise.wilcox.test(D$grade, D$Text_version, p.adj = "bonferroni")





################ examining covariates with ANCOVA ################### 
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

