setwd("C:/Users/User 56/Downloads")

install.packages('ISLR')
install.packages('Broom')
install.packages('caret')

library(ISLR)
library(broom)
library(caret)

data1 <- read.csv('customer_churn.csv', header = T, stringsAsFactors = T)
data1$CustomerID = NULL
data1$MonthlyFee = NULL
model1 <- glm(Churn ~ ., family = "binomial", data=data1)
summary(model1)

model2 <- glm(Churn ~ Age, family = "binomial", data=data1)
summary(model2)

st <- step(model1, direction = "backward")

predict(model1, data.frame(Age = 40, ContractType = 'Yearly', PaymentMethod = 'CreditCard', TenureMonths = 12), type ="response")

