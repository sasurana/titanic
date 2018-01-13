tit<-read.csv("train.csv")

sum(is.na(tit$Survived))
sum(is.na(tit$Pclass))
sum(is.na(tit$Sex))
sum(is.na(tit$Age))
hist(tit$Age)

#treating the missing
tit$Age<- ifelse(is.na(tit$Age), m, tit$Age)
tit$age<- as.numeric(tit$Age)

#model selected after multiple iterations
model<- glm(Survived ~ age + Sex + Pclass + SibSp, data = tit, family = binomial(link = logit))
summary(model)

#predicting for test
test<- read.csv("test.csv")
library(sqldf)
mtest<- sqldf("select avg(Age) from test where Age is not null")
test$age<-replace(test$Age, is.na(Age), mtest)
test$age<- as.numeric(test$age)
test$survive<-round(predict(model, newdata=test, type = 'response'))
pred<- test[,c("PassengerId", "Survived")]
write.csv(pred, "predicted.csv")
