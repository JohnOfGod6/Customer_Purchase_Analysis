
#Laoding the necessary package 
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)

#Importing data to be used
data <- read.csv("C://Users//user//Downloads//SR Material//ASSIGMENT//data_purchase_behaviour.csv")
head(data)

#_________________________________________________________________________
#Question 1
# We need tounderstand the distributions and relationships between variables.

#Let's first get summary statistics for numerical variables.
summary(data)
 
# Frequency tables for categorical variables
cat("Frequency of Gender:\n")
print(table(data$Gender))
cat("\nFrequency of City Categories:\n")
print(table(data$City_Category))
cat("\nFrequency of Marital Status:\n")
print(table(data$Marital_Status))
cat("\nFrequency of Stay In Current City Years:\n")
print(table(data$Stay_In_Current_City_Years))
 
# Visuals
# 1. Histogram to show the distribution of the purchase amounts
hist_plot <- ggplot(data, aes(x = Purchase)) +
  geom_histogram(binwidth = 1000, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = "Purchase Amounts", x = "Purchase Amount", y = "Frequency") +
  theme_minimal()
print(hist_plot)

ggsave("Purchase_Amounts.png", plot = hist_plot, width = 8, height = 6, dpi = 300)

# 2. Boxplot to compare purchase amounts between genders
boxplot_gender <- ggplot(data, aes(x = Gender, y = Purchase, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Purchase Amount by Gender", x = "Gender", y = "Purchase Amount") +
  theme_minimal()
print(boxplot_gender)

ggsave("Purchase_Amount_by_Gender.png", plot = boxplot_gender, width = 8, height = 6, dpi = 300)

# 3. Bar plot to show the relationship between purchase amount and age
 
# Group data by Age and calculate mean purchase amount
avg_purchase_by_age <- data %>%
  group_by(Age_num) %>%
  summarise(Average_Purchase = mean(Purchase, na.rm = TRUE))
avg_purchase_by_age

# Bar plot
bar_plot <- ggplot(avg_purchase_by_age, aes(x = Age_num, y = Average_Purchase, fill = Age_num)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(
    title = "Average Purchase Amount by Age",
    x = "Age (Years)",
    y = "Average Purchase Amount"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

print(bar_plot)

#Scatter plot
scatter_plot <- ggplot(data, aes(x = Age_num, y = Purchase)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Purchase Amount vs Age", x = "Age", y = "Purchase Amount") +
  theme_minimal()
print(scatter_plot)

ggsave("Purchase_Amount_vs_Age.png", plot = scatter_plot, width = 8, height = 6, dpi = 300)

# Save the plot
ggsave("Average_Purchase_by_Age.png", plot = bar_plot, width = 8, height = 6, dpi = 300)


# Create a 2x2 grid layout  
combined_plots <- grid.arrange(
  hist_plot, 
  boxplot_gender, 
  bar_plot, 
  scatter_plot,
  ncol = 2
) 
print(combined_plots)
ggsave("Combined_Plots.png", plot = combined_plots, width = 12, height = 8, dpi = 300)


#_____________________________________________________________________
#Question 2

#a. Fit a linear regression model by estimating the intercept and  the slope

Beta_est <- function(x, y) {
  n <- length(x)
  
  # Calculate the means of x and y
  x_bar <- sum(x)/n
  y_bar <- sum(y)/n
  
  # Compute the covariance between x and y
  S_xy <- sum((x - x_bar) * (y - y_bar))
  
  # Compute the variance of x and the variance of y
  S_xx <- sum((x - x_bar)^2)
  S_yy <-   sum((y - y_bar)^2)
  
  # Estimate the slope (Beta1) and  the intercept (Beta0)
  Beta1_est<- S_xy / S_xx
  Beta0_est<- y_bar - Beta1_est * x_bar
  
  cat("The intercept is",Beta0_est,"and the slope is ",Beta1_est)
}

Beta_est(data$Age_num, data$Purchase)

 
# Fitted model is given as : 
fitted_model <- function(Age_num, Beta0_est, Beta1_est) {
  Purchase <- Beta0_est + Beta1_est * Age_num
  return(Purchase)
}

#example
predicted_purchase <-fitted_model( 56, 9037.936, 6.680612)
cat("The predicted Purchase value for an individual aged", 56, 
    "is:", predicted_purchase, "\n")

#Let's Check using built_in function
model <- lm(Purchase ~ Age_num, data=data)

# Summary of the model
summary(model)

#for uncertainty, we need to calculate the variance of estimated parameters 


# Solving for y_hat and sigma2_hat
x<-data$Age_num
y<-data$Purchase

n <- length(x)
x_bar <- sum(x)/n
y_bar <- sum(y)/n
S_xy <- sum((x - x_bar) * (y - y_bar))
S_xx <- sum((x - x_bar)^2)
S_yy <-   sum((y - y_bar)^2)
Beta1_est<- S_xy / S_xx
Beta0_est<- y_bar - Beta1_est * x_bar

y_hat <- Beta0_est + Beta1_est*x
sigma2_hat <- sum((y-y_hat)^2)/(n-2)

# Now, we find var_beta0_est, var_beta1_est and cov_beta0_est_beta1_est
var_beta0_est <- (sum(x^2)*sigma2_hat)/(n*S_xx)
var_beta1_est <- sigma2_hat/S_xx
cov_beta0_est_beta1_est <- (-x_bar*sigma2_hat)/S_xx

cat("The variance of beta0 estimate is", var_beta0_est, "\n",
    "The variance of beta1 estimate is", var_beta1_est, "\n",
    "The covariance of beta0 and beta1 estimates is", cov_beta0_est_beta1_est, "\n")


#_______________________________________________________________________
#b) Interpretation of model.

#There is a statistically significant relationship between age and purchase, 
#the effect size is very small: for every additional year of age, #
#purchases increase by about 6.68 units. 
#However, the model explains almost none of the variability in purchase amounts (R-squared is nearly zero),
#meaning there are likely other important factors influencing purchase behavior that are not included in this model.

#_______________________________________________________________________
#c) Comment on the usefulness of model.

#The relationship between age and purchase amount is statistically significant (p-value < 0.001),
#which means there is a measurable association between the two variables.

#The R-squared value (0.0002444) indicates that the model explains only 0.024% of the variation in purchase amounts.
#This is extremely low and suggests that age is not a meaningful factor for predicting purchase behavior.

# In general, the model identifies a statistically significant relationship between age and purchase amounts, 
#it is not useful for practical applications. The relationship is too weak to provide meaningful predictions.
#A more robust model that includes additional predictors and explores interactions or nonlinear relationships 
#would likely be far more useful for understanding purchase behavior.

#_______________________________________________________________________
#d) Comment on the limitations of model

#The R-squared value is almost zero (0.0002444), meaning the model explains less than 0.03% of the variation in purchase amounts.

#The Residual Standard Error (5026) shows that the model's predictions can be off by thousands of units.
# This large error range makes the model unreliable for practical predictions.

#To sumup, our model focuses only on age while ignoring other important factors, 
#and its predictions are too inaccurate for practical use.

#_______________________________________________________________________
# e) Two ways to improve our model.

 #1. We can improve our model by adding other variables like Gender, City_Category, and Marital_Status to create a multiple regression model. 

#2. Another way is to investigate whether Age_num has a non-linear effect on Purchase (e.g., quadratic or piecewise regression).

#Example for fitting an improved linear regression model with more variables

improved_model <- lm(Purchase ~ Age_num + Gender + City_Category + Stay_In_Current_City_Years + Marital_Status, data=data)

# Summary of the improved model
summary(improved_model)


#_______________________________________________________________________

#Question 3
 #  Descriptive Analysis: Summary statistics by gender
 
grouped_data <- group_by(data, Gender)  

summary_by_gender <- summarise(
  grouped_data,
  Mean_Purchase = mean(Purchase, na.rm = TRUE),   
  Median_Purchase = median(Purchase, na.rm = TRUE),  
  SD_Purchase = sd(Purchase, na.rm = TRUE),  
  Count = n() 
) 
print(summary_by_gender)


 
# Two-sample t-test
t_test_result <- t.test(Purchase ~ Gender, data = data)
print(t_test_result)

# Linear Regression: Add Gender as a predictor
model_gender <- lm(Purchase ~ Gender, data = data)
summary(model_gender)






 