install.packages('e1071')
library(e1071)

setwd('C:/Users/User 56/Downloads')
data1 <- read.csv("credit_rating.csv", header = T, colClasses = "factor")

#model
model <- naiveBayes(class~., data=data1)
model

age <- c("young")
income <- c("high")
student <- c("no")
credit_rating <- c("fair")
x.data = data.frame(age, income, student, credit_rating)

predict(model, x.data, type = "raw")
predict(model, x.data, type = "class")
data(iris)
##Split in train + test set
idxs <- sample(1:nrow(iris), as.integer(0.7 * nrow(iris)))
trainIrls <- iris[idxs,]
testIrls <- iris[-idxs,]

model1 = naiveBayes(Species~., data=trainIrls)

class <- predict(model1, testIrls, type = "class")

library(caret)

confusionMatrix(class, testIrls$Species)

#=====================================
data2 <- read.csv("customer_churn.csv", header = T, stringsAsFactors = T)
data2$CustomerID = NULL
data2$MonthlyFee = NULL
#model
model2 <- naiveBayes(Churn~., data=data2)
#predict
Age <- c(40)
TenureMonths <- c(12)
PaymentMethod <- c("CreditCard")
ContractType <- c('Yearly')

x_data = data.frame(Age, TenureMonths,ContractType, PaymentMethod)
predict.nay <- predict(model2, x_data, type = "raw")

test = read.csv("testset.csv", stringsAsFactors = T, header = T)
predict.nay1 <- predict(model2, newdata = test )
write.csv(predict.nay1,"submission.csv")
#มีโอกาสยกเลิกสัญญา 6.66%

