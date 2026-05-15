# -------------------------------------------------
# Create Dataset
# -------------------------------------------------
set.seed(123)
data <- data.frame(
  ID = 1:100,
  Age = sample(18:60, 100, replace = TRUE),
  Income = sample(20000:100000, 100, replace = TRUE),
  SpendingScore = sample(1:100, 100, replace = TRUE),
  Purchase = sample(c("Yes","No"), 100, replace = TRUE),
  Region = sample(c("North","South","East","West"),
                  100,
                  replace = TRUE)
)
# Display Dataset
head(data)

# -------------------------------------------------
# Q1) Retail Income vs Spending Strategy
# -------------------------------------------------
# 1. Pearson correlation
cor(data$Income, data$SpendingScore)
# 2. Significance test
cor.test(data$Income, data$SpendingScore)
# 3. Normal distribution using Income
rnorm(100,
      mean(data$Income),
      sd(data$Income))# 4. One-sample t-test for Income = 50000
t.test(data$Income, mu = 50000)
# 5. Binomial test
binom.test(
  sum(data$Purchase == "Yes"),
  nrow(data),
  p = 0.5
)

# -------------------------------------------------
# Q2) Customer Segmentation Strategy
# -------------------------------------------------
# 1. Correlation matrix
numeric_data <- data[, c("Age",
                         "Income",
                         "SpendingScore")]
cor(numeric_data)
# 2. Strongest relationship
cor(numeric_data)
# 3. Normal distribution for SpendingScore
rnorm(100,
      mean(data$SpendingScore),
      sd(data$SpendingScore))
# 4. t-test for SpendingScore = 60
t.test(data$SpendingScore,
       mu = 60)
# 5. Kruskal-Wallis test
kruskal.test(
  SpendingScore ~ Region,
  data = data
)

# -------------------------------------------------
# Q3) Financial Risk Assessment
# -------------------------------------------------
# 1. Correlation between Income and SpendingScore
cor(data$Income,
    data$SpendingScore)
# 2. Statistical significance
cor.test(data$Income,
         data$SpendingScore)
# 3. Poisson distribution
rpois(50, lambda = 5)
# 4. t-test on Income = 40000
t.test(data$Income,
       mu = 40000)
# 5. Binomial test assuming 60% safe customers
binom.test(
  60,
  100,
  p = 0.6
)

# -------------------------------------------------
# Q4) Product Demand Forecasting
# -------------------------------------------------
# 1. Spearman correlation
cor(data$Age,
    data$Income,
    method = "spearman")
# 2. Monotonic relationship explanation
cor(data$Age,
    data$Income,
    method = "spearman")
# 3. Binomial distribution
rbinom(50,
       size = 10,
       prob = 0.5)
# 4. t-test on Age = 35
t.test(data$Age,
       mu = 35)
# 5. Chi-square goodness-of-fit
chisq.test(table(data$Region))

# -------------------------------------------------
# Q5) Customer Satisfaction Modeling
# -------------------------------------------------
# 1. Pearson correlation
cor(data$Income,
    data$SpendingScore)
# 2. Spearman correlation
cor(data$Income,
    data$SpendingScore,
    method = "spearman")
# 3. Normal distribution for SpendingScore
rnorm(100,
      mean(data$SpendingScore),
      sd(data$SpendingScore))
# 4. t-test for SpendingScore = 55
t.test(data$SpendingScore,
       mu = 55)
# 5. Wilcoxon test
wilcox.test(data$Age,
            data$SpendingScore)

# -------------------------------------------------
# Q6) Sales Probability Analysis
# -------------------------------------------------
# 1. Convert Purchase into binary
data$PurchaseBinary <- ifelse(
  data$Purchase == "Yes",
  1,
  0
)
cor(data$PurchaseBinary,
    data$Income)
# 2. Binomial distribution
rbinom(50,
       size = 10,
       prob = 0.5)
# 3. Binomial test
binom.test(
  sum(data$Purchase == "Yes"),
  nrow(data),
  p = 0.5
)
# 4. t-test on Income = 55000
t.test(data$Income,
       mu = 55000)
# 5. Chi-square test for purchase distribution
chisq.test(table(data$Purchase))

# -------------------------------------------------
# Q7) Strategic Business Decision Model
# -------------------------------------------------
# 1. Correlation matrix
cor(numeric_data)
# 2. Strongest predictor of SpendingScore
cor(numeric_data)
# 3. Poisson distribution
rpois(100,
      lambda = 10)
# 4. t-test on Income benchmark = 60000
t.test(data$Income,
       mu = 60000)
# 5. Kruskal-Wallis test for Age across regions
kruskal.test(
  Age ~ Region,
  data = data
)