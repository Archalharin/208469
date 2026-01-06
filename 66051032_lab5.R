install.packages("ISLR") 
install.packages("MASS")
install.packages("caret")
library(ISLR)
library(MASS)
library(caret)
data2 <- ISLR::Default
#split data into train test
samplesize = 0.70*nrow(data2)#70:30
set.seed(80)
index=sample(seq_len(nrow(data2)), size=samplesize) 
train = data2[index,]
test = data2[-index,]
fit.LDA = lda(default~balance + student, data = train)
#score = 0.002377balance-0.4779066student

plot(fit.LDA)
predict.lda <- predict(fit.LDA, newdata = test)

table(test$default,predict.lda$class)
confusionMatrix(test$default, predict.lda$class, positive = 'Yes')

fit.LR = glm(default~balance + student, family = "binomial", data = train)
predict.lr <- predict(fit.LR, type = 'response', newdata = test)
ans.pred = rep("No", dim(test)[1])
ans.pred[predict.lr > 0.5] ="Yes"

confusionMatrix(test$default, as.factor(ans.pred), positive = 'Yes')
