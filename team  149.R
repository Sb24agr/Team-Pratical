# Load the dataset
heart <- read.csv("heart.csv")
summary(heart)

# Display structure of the dataset (data types and format)
str(heart)

#required variables and remove missing values 
heart_clean <- na.omit(heart[, c("Cholesterol", "RestingBP")])


hist(heart_clean$Cholesterol,
     main = "Histogram of Cholesterol",
     xlab = "Cholesterol (mg/dL)",
     col = "lightblue",
     border = "black")

#caluculating mean and standard deviation
chol_mean <- mean(heart_clean$Cholesterol)
chol_sd <- sd(heart_clean$Cholesterol)

#bell curve to histogram 
curve(dnorm(x, mean = chol_mean, sd = chol_sd) *
        max(hist(heart_clean$Cholesterol, plot = FALSE)$counts),
      add = TRUE,
      col = "red",
      lwd = 2)

#scatter plot of visualisation
plot(heart_clean$Cholesterol,
     heart_clean$RestingBP,
     main = "Scatterplot: Cholesterol vs Resting BP",
     xlab = "Cholesterol (mg/dL)",
     ylab = "Resting Blood Pressure (mmHg)",
     pch = 19)

# Add a fitted regression line to the scatterplot
abline(lm(RestingBP ~ Cholesterol, data = heart_clean), col = "blue", lwd = 2)
cor(heart_clean$Cholesterol,
    heart_clean$RestingBP,
    use = "pairwise.complete.obs",
    method = "pearson")

#Calculate the correlation coefficient
cor.test(heart_clean$Cholesterol,
         heart_clean$RestingBP,
         method = "pearson")

boxplot(heart_clean$Cholesterol,
        heart_clean$RestingBP,
        names = c("Cholesterol", "RestingBP"),
        main = "Boxplots",
        ylab = "Values")
