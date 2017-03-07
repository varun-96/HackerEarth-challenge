train_orig <- read.csv("competition_second_train.csv", header = FALSE)

test_orig <- read.csv("competition_second_test.csv", header = FALSE)

library(caret)

set.seed(123)
newf <- subset(train, select = c(V24,V25,V27,V28,V37,V50,V54,V59,V60,V68))

library(Rcpp)
library(mice)
set.seed(125)

imputed_newf = complete(mice(full[var]))

full$V39[is.na(full$V39)] = "SBrkr"

lin_reg <- lm(V76 ~.,data = train)
pred <- predict(lin_reg, newdata = test)

new <- subset(train, select = -c(V20,V21,V22,V14,V30,V35,V43,V36,V37,V38,V39,V55,V71,V72,V73))

new <- subset(new, select = -c(V1))