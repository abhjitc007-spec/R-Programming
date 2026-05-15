data <- data.frame(
  Gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
  Purchase = c("Yes", "No", "Yes", "Yes", "No", "Yes"),
  Amount = c(5000, 7000, 8000, 6000, 4000, 9000)
)

# Mean and Median of Amount
mean(data$Amount)
median(data$Amount)

# Frequency Table for Gender
table(data$Gender)

# Cross Tabulation of Gender and Purchase
table(data$Gender, data$Purchase)

# Total purchase by Gender
aggregate(Amount ~ Gender, data = data, sum)

# Count number of "Yes" Purchases
sum(data$Purchase == "Yes")