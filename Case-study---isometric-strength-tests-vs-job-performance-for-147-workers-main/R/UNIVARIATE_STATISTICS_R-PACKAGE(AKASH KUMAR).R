library(moments)                 
library(statip)
attach(data)
#number of data collected in each test
help(length)
GRIP_N <- length(data$GRIP)
ARM_N <- length(data$ARM)
RATINGS_N <- length(data$RATINGS)
SIMS_N <- length(data$SIMS)
cat("Number of data collected in GRIP",GRIP_N)
cat("Number of data collected in ARM",ARM_N)
cat("Number of data collected in RATINGS",RATINGS_N)
cat("Number of data collected in SIMS",SIMS_N)

#mean (average) of each survey
help(mean)
GRIP_MEAN <- mean(data$GRIP)
ARM_MEAN <- mean(data$ARM)
RATINGS_MEAN <- mean(data$RATINGS)
SIMS_MEAN <- mean(data$SIMS)
cat("Mean of data collected in GRIP",GRIP_MEAN)
cat("Mean of data collected in ARM",ARM_MEAN)
cat("Mean of data collected in RATINGS",RATINGS_MEAN)
cat("Mean of data collected in SIMS",SIMS_MEAN)

#median of each survey
# Half of the data have values less than the median.
help(median)
GRIP_MEDIAN <- median(data$GRIP)
ARM_MEDIAN <- median(data$ARM)
RATINGS_MEDIAN <- median(data$RATINGS)
SIMS_MEDIAN <- median(data$SIMS)
cat("Median of data collected in GRIP",GRIP_MEDIAN)
cat("Median of data collected in ARM",ARM_MEDIAN)
cat("Median of data collected in RATINGS",RATINGS_MEDIAN)
cat("Median of data collected in SIMS",SIMS_MEDIAN)

#trimean of each survey(The trimean function calculates the trimean,
#which is the average of the median (middle value) and
#the two quartiles (the 25th and 75th percentiles).
#It provides a measure of central tendency that is
#less sensitive to extreme values than the mean.)
help(trimean)
GRIP_TRIMEAN <- trimean(data$GRIP)
ARM_TRIMEAN <- trimean(data$ARM)
RATINGS_TRIMEAN <- trimean(data$RATINGS)
SIMS_TRIMEAN <- trimean(data$SIMS)
cat("Tri-Mean of data collected in GRIP",GRIP_TRIMEAN)
cat("Tri-Mean of data collected in ARM",ARM_TRIMEAN)
cat("Tri-Mean of data collected in RATINGS",RATINGS_TRIMEAN)
cat("Tri-Mean of data collected in SIMS",SIMS_TRIMEAN)

#Minimum of each survey
help(min)
GRIP_MIN <- min(data$GRIP)
ARM_MIN <- min(data$ARM)
RATINGS_MIN <- min(data$RATINGS)
SIMS_MIN <- min(data$SIMS)
cat("Minimum of data collected in GRIP",GRIP_MIN)
cat("Minimum of data collected in ARM",ARM_MIN)
cat("Minimum of data collected in RATINGS",RATINGS_MIN)
cat("Minimum of data collected in SIMS",SIMS_MIN)

#Maximum of each survey
help(max)
GRIP_MAX <- max(data$GRIP)
ARM_MAX <- max(data$ARM)
RATINGS_MAX <- max(data$RATINGS)
SIMS_MAX <- max(data$SIMS)
cat("Maximum of data collected in GRIP",GRIP_MAX)
cat("Maximum of data collected in ARM",ARM_MAX)
cat("Maximum of data collected in RATINGS",RATINGS_MAX)
cat("Maximum of data collected in SIMS",SIMS_MAX)

#QUANTILE OF 25TH PERCENTILE OF EACH SURVEY
# 25% of the data have values less than the first quartile 
help(quantile)
GRIP_QUANTILE25 <- quantile(data$GRIP,0.25)
ARM_QUANTILE25 <- quantile(data$ARM,0.25)
RATINGS_QUANTILE25 <- quantile(data$RATINGS,0.25)
SIMS_QUANTILE25 <- quantile(data$SIMS,0.25)
cat("quantile(25) of data collected in GRIP",GRIP_QUANTILE25)
cat("quantile(25) of data collected in ARM",ARM_QUANTILE25)
cat("quantile(25) of data collected in RATINGS",RATINGS_QUANTILE25)
cat("quantile(25) of data collected in SIMS",SIMS_QUANTILE25)

#QUANTILE OF 75TH PERCENTILE OF EACH SURVEY
#75% of the data have values less than the third quartile. 
help(quantile)
GRIP_QUANTILE75 <- quantile(data$GRIP,0.75)
ARM_QUANTILE75 <- quantile(data$ARM,0.75)
RATINGS_QUANTILE75 <- quantile(data$RATINGS,0.75)
SIMS_QUANTILE75 <- quantile(data$SIMS,0.75)
cat("quantile(75) of data collected in GRIP",GRIP_QUANTILE75)
cat("quantile(75) of data collected in ARM",ARM_QUANTILE75)
cat("quantile(75) of data collected in RATINGS",RATINGS_QUANTILE75)
cat("quantile(75) of data collected in SIMS",SIMS_QUANTILE75)

#Standard deviation of each survey

help(sd)
GRIP_SD <- sd(data$GRIP)
ARM_SD <- sd(data$ARM)
RATINGS_SD <- sd(data$RATINGS)
SIMS_SD <- sd(data$SIMS)
cat("Standard deviation of data collected in GRIP",GRIP_SD)
cat("Standard deviation of data collected in ARM",ARM_SD)
cat("Standard deviation of data collected in RATINGS",RATINGS_SD)
cat("Standard deviation of data collected in SIMS",SIMS_SD)

#Skewness of each survey
help(skewness)
GRIP_SKEW <- skewness(data$GRIP)
ARM_SKEW <- skewness(data$ARM)
RATINGS_SKEW <- skewness(data$RATINGS)
SIMS_SKEW <- skewness(data$SIMS)
cat("SKEWNESS of data collected in GRIP",GRIP_SKEW)
cat("SKEWNESS of data collected in ARM",ARM_SKEW)
cat("SKEWNESS of data collected in RATINGS",RATINGS_SKEW)
cat("SKEWNESS of data collected in SIMS",SIMS_SKEW)

#Kurtosis of each survey
help(kurtosis)
GRIP_KURTOSIS <- kurtosis(data$GRIP)
ARM_KURTOSIS <- kurtosis(data$ARM)
RATINGS_KURTOSIS <- kurtosis(data$RATINGS)
SIMS_KURTOSIS <- kurtosis(data$SIMS)
cat("KURTOSIS of data collected in GRIP",GRIP_KURTOSIS)
cat("KURTOSIS of data collected in ARM",ARM_KURTOSIS)
cat("KURTOSIS of data collected in RATINGS",RATINGS_KURTOSIS)
cat("KURTOSIS of data collected in SIMS",SIMS_KURTOSIS)

#function to get value between ranges
createDataFrameInRange <- function(dat, column, min_value, max_value) {
  # Extract the specified column from the data frame
  column_data <- data[[column]]
  
  # Filter the data frame to include only values within the range
  filtered_data <- data[column_data >= min_value & column_data <= max_value, ]
  
  return(filtered_data)
}

filtered_data1 <- createDataFrameInRange(data, "RATINGS", 20, 30)
filtered_data2 <- createDataFrameInRange(data, "RATINGS", 30, 40)
filtered_data3 <- createDataFrameInRange(data, "RATINGS", 40, 50)
filtered_data4 <- createDataFrameInRange(data, "RATINGS", 50, 60)

RATINGS_RANGE <- c(length(filtered_data1$RATINGS),length(filtered_data2$RATINGS),length(filtered_data3$RATINGS),length(filtered_data4$RATINGS))
ranges1 <- c("20 to 30","30 to 40","40 to 50","50 to 60")
colors <- c("black","red","yellow","blue","purple")
barplot(RATINGS_RANGE,names.arg =  ranges1, xlab = "RANGES",ylab ="NUMBER OF OBSERVATIONS", main = "RATINGS",col = colors)

filtered_data5 <- createDataFrameInRange(data, "SIMS", -4, -2)
filtered_data6 <- createDataFrameInRange(data, "SIMS", -2, 0)
filtered_data7 <- createDataFrameInRange(data, "SIMS", 0, 2)
filtered_data8 <- createDataFrameInRange(data, "SIMS", 2, 4)
filtered_data9 <- createDataFrameInRange(data, "SIMS", 4, 6)
ranges2 <- c("-4 to -2","-2 to 0","0 to 2","2 to 4","4 to 6")
SIMS_RANGE <- c(length(filtered_data5$SIMS),length(filtered_data6$SIMS),length(filtered_data7$SIMS),length(filtered_data8$SIMS),length(filtered_data9$SIMS))

barplot(SIMS_RANGE,names.arg =  ranges2, xlab = "RANGES",ylab ="NUMBER OF OBSERVATIONS", main = "SIMS",col = colors)

