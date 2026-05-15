set.seed(100)

sales <- data.frame(
  Product = sample(c("Laptop", "Mobile", "Tablet"), 50, replace = TRUE),
  City = sample(c("Pune", "Mumbai", "Delhi"), 50, replace = TRUE),
  Quantity = sample(1:5, 50, replace = TRUE),
  Price = sample(seq(10000, 50000, 5000), 50, replace = TRUE)
)
sales$Revenue <- sales$Quantity * sales$Price

# Records where Revenue > 100000
sales[sales$Revenue > 100000,]

# Data by Revenue in descending order
sales[order(sales$Revenue, decreasing = TRUE),]

# Total revenue by City
aggregate(Revenue ~ City, sales, sum)

# Average Price by Product
aggregate(Price ~ Product, sales, mean)

# Product with highest total revenue => Total Revenue by Product
library(dplyr)

sales %>%
  group_by(Product) %>%
  summarise(Total_Revenue = sum(Revenue)) %>%
  arrange(desc(Total_Revenue))
