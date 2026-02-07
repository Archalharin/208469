setwd('C:/Users/User 56/Downloads')
data = read.csv("customer_churn (1).csv", header = T, stringsAsFactors = T)
install.packages('class')
install.packages("caret")
library(class)
library(caret)


data$CustomerID = NULL
ch_nolabel$ContractType_Monthly = ifelse(data$ContractType == "Monthly", 1, 0)
ch_nolabel$ContractType_Yearly = ifelse(data$ContractType == "Yearly", 1, 0)
ch_nolabel$PaymentMethod_Cash = ifelse(data$PaymentMethod == "Cash", 1, 0)
ch_nolabel$PaymentMethod_CreditCard = ifelse(data$PaymentMethod == "CreditCard", 1, 0)
ch_nolabel$PaymentMethod_QR = ifelse(data$PaymentMethod == "QR", 1, 0)
ch_nolabel$PaymentMethod =NULL
ch_nolabel$ContractType = NULL
data$MonthlyFee = NULL

#normalization
ch_nolabel <- data[,1:4]
ch_label <- data[5]

maxs <- lapply(ch_nolabel, max)
mins <- lapply(ch_nolabel, min)
ch_nolabel_scaled <- scale(ch_nolabel, center=mins, scale=maxs)
## Split in train + test set
idxs <- sample(1:nrow(ch_nolabel_scaled),as.integer(0.7*nrow(ch_nolabel_scaled)))
idxs_test <- sample(1:nrow(ch_label),as.integer(0.7*nrow(ch_label)))

traindata <- ch_nolabel_scaled[idxs,]
testdata <- ch_nolabel_scaled[-idxs,]

traindata_ch <- ch_label[idxs_test,]
testdata_ch <- ch_label[-idxs_test,]

##removing factorvariable from training and test datasets
#trainData2 <- traindata[, -c(2,3,4,5,6)]
#testData2 <- testdata[,-c(2,3,4,5,6)]

## k-nn
churn_test_pred1 <- knn(train = traindata, test = testdata, cl=
                          as.factor(traindata_ch),k = 3,prob=TRUE)

table(as.factor(testdata_ch),churn_test_pred1)

confusionMatrix(as.factor(testdata_ch),churn_test_pred1)

##9.
new_customer <- data.frame(
  Age = 40,
  ContractType_ = 0,
  ContractType_Yearly = 1,
  PaymentMethod_Cash = 0,
  PaymentMethod_CreditCard = 1,
  PaymentMethod_QR = 0,
  TenureMonths = 12
)

knn_predict <-  knn(train = traindata, test = new_customer, cl=
                      traindata_ch,k = 3,prob=TRUE)
dim(traindata)
dim()