dataset<- read.csv("D:/chrome download/stroke_dataset/full_data.csv", stringsAsFactors = FALSE)
dataset

dataset <- na.omit(dataset)


install.packages("ggplot2")
library(dplyr)
library(ggplot2)


hist(dataset$age, 
     main = "Normal Histogram of Age", 
     xlab = "Age", 
     col = "blue", 
     border = "black", 
     breaks = 10)


hist(dataset$avg_glucose_level, 
     main = "Normal Histogram of Avg Glucose Level", 
     xlab = "Avg Glucose Level", 
     col = "green", 
     border = "black", 
     breaks = 15)


hist(dataset$bmi, 
     main = "Normal Histogram of BMI", 
     xlab = "BMI", 
     col = "orange", 
     border = "black", 
     breaks = 10)



plot(density(na.omit(dataset$age)), 
     main = "Line Histogram (Density Curve) of Age", 
     xlab = "Age", 
     ylab = "Density", 
     col = "red", 
     lwd = 2)



plot(density(dataset$avg_glucose_level), 
     main = "Line Histogram (Density Plot) for Avg Glucose Level", 
     xlab = "Avg Glucose Level", 
     ylab = "Density", 
     col = "green", 
     lwd = 2)

abline(v = mean(dataset$avg_glucose_level), col = "red", lwd = 2, lty = 2)      
abline(v = median(dataset$avg_glucose_level), col = "blue", lwd = 2, lty = 2)   

legend("topright", 
       legend = c("Mean", "Median"), 
       col = c("red", "blue"), 
       lwd = 2, lty = 2)


hist(dataset$bmi, 
     probability = TRUE, 
     main = "Histogram and Density Plot for BMI", 
     xlab = "BMI", 
     ylab = "Density", 
     col = "lightblue", 
     border = "white") 


lines(density(dataset$bmi), 
      col = "purple", 
      lwd = 2)

legend("topright", 
       legend = c("Density Plot", "Histogram"), 
       col = c("purple", "lightblue"), 
       lwd = 2, 
       fill = c(NA, "lightblue"), 
       border = NA)


install.packages("e1071")
library(e1071)

hist(dataset$bmi, 
     main = paste("BMI Distribution\nSkewness =", round(skewness(dataset$bmi, na.rm = TRUE), 2)), 
     xlab = "BMI", 
     col = "lightgreen", 
     border = "black")



ggplot(dataset, aes(x = factor(stroke), fill = factor(stroke))) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("red", "blue"), labels = c("No Stroke", "Stroke")) +
  labs(title = "barplot for Stroke", x = "Stroke (0 = No, 1 = Yes)", y = "Count", fill = "Stroke Status")

ggplot(dataset, aes(x = factor(hypertension), fill = factor(hypertension))) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("red", "blue"), labels = c("No hypetension", "hypertension")) +
  labs(title = "barplot for Hypertension", x = "hypertension", y = "Count", fill = "Hypertension Status")



library(ggplot2)


ggplot(dataset, aes(x = gender, y = age, fill = gender)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violin Plot of Age by Gender", x = "Gender", y = "Age") +
  scale_fill_manual(values = c("blue", "pink")) +
  theme_minimal()




ggplot(dataset, aes(x = factor(stroke), y = age, fill = factor(stroke))) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violin Plot of Age by Stroke Status", x = "Stroke (0 = No, 1 = Yes)", y = "Age") +
  scale_fill_manual(values = c("red", "green"), labels = c("No Stroke", "Stroke")) +
  theme_minimal()

ggplot(dataset, aes(x = factor(heart_disease), y = bmi, fill = factor(heart_disease))) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Violin Plot of bmi by heart disease", x = "heart disease (0 = No, 1 = Yes)", y = "bmi") +
  scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
  theme_minimal()

ggplot(dataset, aes(x = factor(smoking_status), y = age, fill = factor(smoking_status))) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 2, fill = "white", color = "black") +
  stat_summary(fun.data = function(x) {
    data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))
  }, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Violin Plot of Age by Smoking Status ",
       x = "Smoking Status",
       y = "Age") +
  theme_minimal()






install.packages("vioplot")
library(vioplot)


vioplot(age ~ gender, data = dataset, col = c("blue", "pink"))
vioplot(bmi ~ hypertension, data = dataset, col = c("blue", "pink"))
vioplot(bmi ~ stroke, data = dataset, col = c("blue", "pink"))
vioplot(age ~ smoking_status, data = dataset, col = c("blue", "pink"))
vioplot(bmi ~ heart_disease, data = dataset, col = c("blue", "pink"))
vioplot(avg_glucose_level ~ heart_disease, data = dataset, col = c("blue", "pink"))






ggplot(dataset, aes(x = age, y = bmi)) +
  geom_point(color = "orange", alpha = 0.7) +
  labs(title = "Scatter Plot of Age vs. BMI", x = "Age", y = "BMI") +
  theme_minimal()


plot(dataset$age, dataset$bmi,
     main = "Scatter Plot of Age vs. BMI by stroke",
     xlab = "Age",
     ylab = "BMI",
     col = ifelse(dataset$stroke == 1, "red", "green"),
     pch = 19)
legend("topright", legend = c("No Stroke", "Stroke"), col = c("green", "red"), pch = 19)



plot(dataset$age, dataset$avg_glucose_level,
     main = "Scatter Plot of Age vs. Avg Glucose Level by Stroke",
     xlab = "Age",
     ylab = "Avg Glucose Level",
     col = ifelse(dataset$stroke == 1, "red", "green"),  
     pch = 19)
legend("topright", legend = c("No Stroke", "Stroke"), col = c("green", "red"), pch = 19)

plot(dataset$age, dataset$bmi,
     main = "Scatter Plot of Age vs. BMI by Heart Disease",
     xlab = "Age",
     ylab = "BMI",
     col = ifelse(dataset$heart_disease == 1, "red", "green"),  
     pch = 19)  
legend("topright", 
       legend = c("No Heart Disease", "Heart Disease"), 
       col = c("green", "red"), 
       pch = 19)


plot(dataset$age, dataset$bmi,
     main = "Scatter Plot of Age vs. BMI by Smoking Status",
     xlab = "Age",
     ylab = "BMI",
     col = as.factor(dataset$smoking_status),  
     pch = 19)  
legend("topright",
       legend = unique(dataset$smoking_status),  
       col = 1:length(unique(dataset$smoking_status)),  
       pch = 19)




numeric_dataset <- dataset[c("age", "avg_glucose_level", "bmi")]
pairs(numeric_dataset, 
      main = "Scatter Matrix for Numeric Attributes by stroke", 
      col = ifelse(dataset$stroke == 1, "red", "blue"),  
      pch = 19)  








