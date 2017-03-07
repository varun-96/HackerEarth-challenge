
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(caret)
library(Rcpp)
library(mice)

train_orig <- read.csv("competition_second_train.csv", header = FALSE)
test_orig <- read.csv("competition_second_test.csv", header = FALSE)

set.seed(123)
newf <- subset(train, select = c(V24,V25,V27,V28,V37,V50,V54,V59,V60,V68))

set.seed(125)
imputed_newf = complete(mice(full[var]))

full$V39[is.na(full$V39)] = "SBrkr"

lin_reg <- lm(V76 ~.,data = train)
pred <- predict(lin_reg, newdata = test)

new <- subset(train, select = -c(V20,V21,V22,V14,V30,V35,V43,V36,V37,V38,V39,V55,V71,V72,V73))
new <- subset(new, select = -c(V1))

#Using Decision Tree With Cross Validation
fitControl = trainControl(method = "CV", number = 10)
CartGrid = expand.grid(.cp = (1:50)*0.01)

train(V76~.,data = new,method = "rpart",trControl = fitControl, tuneGrid = CartGrid)
treeCV = rpart(V76~.,method = "anova", data = new,control = rpart.control(cp = 0.01))

#Decision Tree
prp(treeCV)