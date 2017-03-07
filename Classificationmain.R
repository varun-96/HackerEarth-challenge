train_orig <- read.csv("competition_first_train.csv")
test_orig <- read.csv("competition_first_test.csv")

library(lattice)
library(ggplot2)
library(e1071)
library(rpart)
library(caret)

set.seed(123)
train <- subset(train_orig, select = -(predict))
full <- rbind(train, test_orig)

full$f[is.na(full$f)] = 28.62

train <- full[1:65000,]
test <- full[65001:88758,]
train$predict <- train_orig$predict

test$b <- as.factor(test$b)
test$c <- as.factor(test$c)
test$d <- as.factor(test$d)
test$h <- as.factor(test$h)

train$b <- as.factor(train$b)
train$c <- as.factor(train$c)
train$d <- as.factor(train$d)
train$h <- as.factor(train$h)
train$predict <- as.factor(train$predict)

train2 <- train[,2:10]

naiv <- naiveBayes(predict~.,data = train)
pred_naiv <- predict(naiv, newdata = test, type = "raw")
na_da <- as.data.frame(pred_naiv)

library(randomForest)

randclas = randomForest(predict ~., data = train2)
pred <- predict(randclas, newdata = test, type = "prob")[,2]
#OUTPUT 
out <- data.frame(ID = test$ID, Prediction = pred)
out$Prediction[out$Prediction >= 0.1] = 1
out$Prediction[out$Prediction < 0.1] = 0

knn_clas = knn3(predict~., data = train2)
predclas = predict(knn_clas, newdata = test)
write.csv(out, "output1.csv", row.names = F)

