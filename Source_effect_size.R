####################### Load data ########################

# Jesper:
#D <- read.csv("/Users/jesperberglund/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")
# Chelina:
D <- read.csv("C:/Users/cheli/Downloads/Data.csv", header = TRUE, sep = ",", quote = "\"")

neutral_grades <- D$grade[D$Text_version == "Neutral"]
positive_grades <- D$grade[D$Text_version == "Positive"]
negative_grades <- D$grade[D$Text_version == "Negative"]

############ Effect size in source [2] ##################
# Initialize an empty vector to store Cohen's d for each pair
cohen_d_values <- numeric(2)

# Store sample size, mean and standard deviation
sample_size <- 38
mean_group1 <-2047
sd_group1 <- 443
mean_group2 <- 1770
sd_group2 <- 372

# Calculate pooled sd from Positive VS Negative
pooled_sd <- sqrt(((sample_size - 1) * sd_group1^2 + (sample_size - 1) * sd_group2^2) / (sample_size + sample_size - 2))

# Calculate Cohen's d
cohen_d <- (mean_group1 - mean_group2) / pooled_sd
cohen_d # 0.6771898 


############ Effect size in source [3] ##################
# Initialize an empty vector to store Cohen's d for each pair
cohen_d_values <- numeric(2)

# Store sample size, mean and standard deviation
sample_size <- 31
mean_group1 <- 6.58
sd_group1 <- 0.47
mean_group2 <- 6.87
sd_group2 <- 0.18

# Calculate pooled sd from Positive VS Negative
pooled_sd <- sqrt(((sample_size - 1) * sd_group1^2 + (sample_size - 1) * sd_group2^2) / (sample_size + sample_size - 2))

# Calculate Cohen's d
cohen_d <- (mean_group2 - mean_group1) / pooled_sd
cohen_d # 0.8148833 
