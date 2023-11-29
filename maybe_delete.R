############# Load data ################
# Read data
# Jesper:
#D <- read.csv("/Users/jesperberglund/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")
# Chelina:
D <- read.csv("C:/Users/cheli/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")

neutral_grades <- D$grade[D$Text_version == "Neutral"]
positive_grades <- D$grade[D$Text_version == "Positive"]
negative_grades <- D$grade[D$Text_version == "Negative"]



####################### Outlier detection ########################

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



####################### Plots ########################

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






####################### Statistics ########################

# Bonferonni correction
# ONLY PERFORM IF KRUSKAL P-VALUE < 0.05 ##### SO NOT #####
#pairwise.wilcox.test(D$grade, D$Text_version, p.adj = "bonferroni")
