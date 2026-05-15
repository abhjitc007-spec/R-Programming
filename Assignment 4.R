# Q1) Importing and Exploring Data from CSV file and Excel File
# a) Import dataset from CSV file and Excel file
sales_data_csv <- read.csv("sales_data.csv")
sales_data_excel <- read_excel("sales_data.xlsx")

# b) Display first 10 rows
head(sales_data_csv, 10)
head(sales_data_excel, 10)

# c) Display last 10 rows
tail(sales_data_csv, 10)
tail(sales_data_excel, 10)

# d) Check structure of dataset
str(sales_data_csv)
str(sales_data_excel)

# e) Display summary statistics
summary(sales_data_csv)
summary(sales_data_excel)

# Q2) Exporting Data
# a) Export dataset to CSV file
write.csv(sales_data,
          "sales_data.csv",
          row.names = FALSE)

# b) Export dataset to Excel file
write_xlsx(sales_data,
           "sales_data.xlsx")

# c) Export only Electronics category
electronics_data <- subset(
  sales_data,
  Category == "Electronics"
)
write.csv(electronics_data,
          "electronics_data.csv",
          row.names = FALSE)
