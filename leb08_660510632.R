install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd("C:/Users/User 56/Downloads/660510632_469")
data1<-read.csv("credit_rating-2.csv",header=T,colClasses="factor")


#สร้างตัวแบบ decision tree โดยใช้คำสั่ง fit
fit <- rpart(class~., data=data1, method="class", control =rpart.control(minsplit =1,minbucket=1, cp=0))

plot(fit)
fancyRpartPlot(fit)
table(data1$credit_rating)
predict(fit, data1)

weather <- read.csv("weather_nom.csv",header=T,colClasses="factor")
fit2 <- rpart(play~., data=weather, method="class", 
             control =rpart.control(minsplit =1,minbucket=1, cp=0))
rpart.plot(fit2, cex=0.6)

# windy 1, 0
weather2 <- read.csv("weather_new.csv",header=T)
fit3 <- rpart(play~., data=weather2, method="class", 
             control =rpart.control(minsplit =1,minbucket=1, cp=0))
rpart.plot(fit3, cex=0.6)


#predict
predict(fit3, weather2,type="class")
outlook = c("sunny")
temperature = c(80)
humidity = c(70)
windy = c(0)
test.data = data.frame(outlook, temperature, humidity, windy)
predict(fit3, test.data, type="class")
