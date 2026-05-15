set.seed(123)
sales_data <- data.frame(
  OrderID = 1:200,
  CustomerName = sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),200,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),200,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),200,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),200,replace=TRUE),
  Quantity = sample(1:10,200,replace=TRUE),
  Price = sample(seq(500,50000,500),200,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),200,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),200,replace=TRUE)
)

sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount/100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount


# Question 1

# (a)
sales_data$OrderSize <- ifelse(sales_data$Quantity <= 3, "Small", 
                               ifelse(sales_data$Quantity <= 7, "Medium", "Large")) 
head(sales_data)

# (b)
sales_data[order(-sales_data$Quantity), ]

# (c)
sales_data[order(sales_data$City, -sales_data$Revenue), ]

# (d)
sales_data$Tax <- sales_data$FinalAmount * 0.18 
head(sales_data)

# (e)
sales_data$TotalBill <- sales_data$FinalAmount + sales_data$Tax 
head(sales_data)


# Question 2

# (a)
select(sales_data, CustomerName, Product)

# (b)
select_if(sales_data, is.numeric)

# (c)
select(sales_data, -Discount)

# (d)
filter(sales_data, City == "Pune")

# (e)
filter(sales_data, Price > 20000 & Quantity > 5)


# Question 3

# (a)
sales_data <- sales_data %>% 
  mutate(Tax = FinalAmount * 0.18)

# (b)
sales_data <- sales_data %>% 
  mutate(Profit = FinalAmount * 0.20)

# (c)
sales_data <- sales_data %>% 
  mutate(OrderCategory = ifelse(Revenue > 30000, "High", 
                                ifelse(Revenue > 10000, "Medium", "Low")))

# (d)
sales_data <- sales_data %>% 
  mutate(DiscountCategory = ifelse(Discount > 10, "High Discount", "Low Discount"))

# (e)
sales_data <- sales_data %>% 
  mutate(PaymentType = PaymentMethod)

# (f)
arrange(sales_data, desc(Revenue))


# Question 4

# (a)
sales_data %>% group_by(City) %>% 
  summarise(TotalRevenue = sum(Revenue))

# (b)
sales_data %>% group_by(Product) %>% 
  summarise(AveragePrice = mean(Price))

# (c)
sales_data %>% group_by(Product) %>% 
  summarise(TotalQuantity = sum(Quantity))

# (d)
sales_data %>% group_by(City) %>% 
  summarise(MaxRevenue = max(Revenue))

# (e)
sales_data %>% group_by(Product) %>% 
  summarise(MinRevenue = min(Revenue))


# Question 5

# (a)
sales_data %>% group_by(CustomerName) %>% 
  summarise(OrderCount = n())

# (b)
sales_data %>% group_by(CustomerName) %>% 
  summarise(TotalRevenue = sum(Revenue))

# (c)
mean(sales_data$Revenue)

# (d)
sales_data %>% group_by(CustomerName) %>% 
  summarise(TotalRevenue = sum(Revenue)) %>% arrange(desc(TotalRevenue))

# (e)
sales_data %>% group_by(CustomerName) %>% 
  summarise(TotalRevenue = sum(Revenue)) %>% arrange(desc(TotalRevenue))