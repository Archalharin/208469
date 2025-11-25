setwd("C:/Users/User 56/Downloads/แมว")
titanic <- read.csv("titanic.csv",header=T, stringsAsFactors = T, na.strings = '?')
#ลบ ticket
titanic$ticket = NULL
#การเติมค่าสูญหายของตัวแปรท่าเรือ (Embarked) โดยใช้ฐานนิยม
getmode = function(x){
  x <- na.omit(x)
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

mode_embarked = getmode(titanic$embarked)
I = which(is.na(titanic$embarked))
titanic$embarked[I] = mode_embarked

#การเติมค่าสูญหายของตัวแปรค่าโดยสารโดยใช้มัธยฐานของค่าโดยสาร
median_fare = median(titanic$fare, na.rm = T)
J = which(is.na(titanic$fare))
titanic$fare[J] = median_fare

#การเติมค่าสูญหายของตัวแปรอายุ (Age) โดยใช้ค่าเฉลี่ยอายุของผู้โดยสารทั้งหมด
