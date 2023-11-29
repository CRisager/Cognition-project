############# Load data ################
# Read data
D <- read.csv("C:/Users/cheli/Downloads/Testing_test_data.csv", header = TRUE, sep = ",", quote = "\"")

neutral_grades <- D$grade[D$Text_version == "Neutral"]
positive_grades <- D$grade[D$Text_version == "Positive"]
negative_grades <- D$grade[D$Text_version == "Negative"]

############# Plots ################

# Box plot of all grades
png(file="Boxplot.png",
    units="cm",res=1200,height = 1200/72,
    width=1200/72)
boxplot(D$grade, ylab = "Grade in %", col = "royalblue", main = "Boxplot of all grades", ylim = c(0, 100))
# Specify y-axis breaks and labels
axis(2, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))
dev.off()




# Bar plot of all grades
possible_grades <- seq(0, 100, by = 10)
# Calculate the number of students for each grade in Possible_grades
Number_of_students <- sapply(possible_grades, function(grade) sum(D$grade == grade))
# Plot
png(file="barplot.png",
    units="cm",res=1200,height = 1200/72,
    width=1200/72)
barplot(Number_of_students, names.arg = possible_grades, col = "royalblue", 
        xlab = "Grades in %", ylab = "Number of students", 
        main = "Number of students given each grade")
dev.off()






# Histogram of all grades
png(file="Histogram_plot.png",
    units="cm",res=1200,height = 1200/72,
    width=1200/72)
par(mfrow=c(1,1))
c(hist(D$grade, breaks=10, col="royalblue", freq = FALSE, xlab = "Grades in %", 
       main = "Histogram of grades", xlim = c(0, 100)),
  axis(1, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)),
  curve(dnorm(x, mean=mean(D$grade),sd=sd(D$grade)), add = TRUE, col = "black", lwd = 2))
dev.off()



# QQ-plot to investigate normality
library(car)
# QQ-plot of all grades
png(file = "QQplot.png", units = "cm",
    res = 1200, height = 1200 / 72, width = 1200 / 72)
par(mfrow=c(1,1))
qqPlot(D$grade, ylab = "Given grades in %")
mtext("QQ plot for grades", side = 3, line = 2)
dev.off()



############# KS test ################
# Kolmogorov-Smirnov test

# All grades
ks.test(D$grade, "pnorm", mean=mean(D$grade), sd=sd(D$grade)) # p-value = 0.4863

