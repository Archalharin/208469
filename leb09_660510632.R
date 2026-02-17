library(e1071)

setwd("C:/Users/User 56/Downloads/660510632_469")
data1<-read.csv("credit_rating-2.csv",header=T,colClasses="factor") 
svm_model <- svm(formula = class~., data = data1, type = 'C-classification', kernel ='radial')

#SVM decision function is w.x+b
# Obtain feature weights (w)
w = t(svm_model$coefs) %*%svm_model$SV
b = -svm_model$rho

#create test data
test.data = data.frame(0,0,1,0,1,1,1)
names(test.data) <- c("agemiddle_age","agesenior","ageyoung","incomelow","incomemedium","studentyes","credit_ratingfair")

#cal decision values manually
t(w %*% t(as.matrix(test.data))) + b

weather<-read.csv("weather_new.csv",header = T, stringsAsFactors = T)
install.packages('fastDummies')
library(fastDummies)
df_dummy <- dummy_cols(weather, select_columns = c("outlook"))
df_dummy$outlook = NULL
model <- svm(formula = play~., data = df_dummy , type = 'C-classification', kernel ='radial')
w = t(model$coefs) %*% model$SV
w
b = -model$rho
# SVM decision function -> 1.696236temperature+2.936599humidity+
#                           1.947249windy-3.381315outlook_overcast+0.59167outlook_rainy+
#                           2.56265outlook_sunny

predict(model, df_dummy, type="response")


outlook_sunny = c(1)
outlook_overcast=c(0)
outlook_rainy=c(0)
temperature = c(80)
humidity = c(70)
windy = c(0)
test.data = data.frame(outlook_sunny,outlook_overcast, outlook_rainy, temperature, humidity, windy)
predict(model, test.data, type="response")


