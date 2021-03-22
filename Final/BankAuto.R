library(readxl)
library(car)
library(caret)
library(ROSE)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)

setwd("C:/Users/manish.grewal/emdp/R/Final")
xlsx <- read_excel("IMB469-XLS-ENG.xlsx")
data <- xlsx[,c(5:16, 19)]

#tab <- lapply(data1, table)

# no missing values
summary(data)

str(data)
#data[, 1:5]  <- lapply(data[, 1:5], scale)
data[,1] <- data[,1] / 10
data[,2] <- (data[,2] - min(data[,2])) / (max(data[,2]) - min(data[,2]))
data[,4] <- (data[,4] - min(data[,4])) / (max(data[,4]) - min(data[,4]))

data[, 6:13] <- lapply(data[, 6:13], function(x) {recode(x, "0=0; 1=1", as.factor = TRUE)})
str(data)

set.seed(123)
myindex <- createDataPartition(data$DefaulterFlag, p = 0.8, list = FALSE)
traindata <- data[c(myindex),]
testdata <- data[-c(myindex),]

train <- traindata
test <- testdata

table(traindata$DefaulterFlag)
over <- ovun.sample(DefaulterFlag ~ ., data = train, method = "over", seed = 123,
                    N = table(train$DefaulterFlag)[2] * 2)$data
under <- ovun.sample(DefaulterFlag ~ ., data = train, method = "under", seed = 123,
                     N = table(train$DefaulterFlag)[1] * 2)$data
both <- ovun.sample(DefaulterFlag ~ ., data = train, method = "both", seed = 123,
                    p = 0.5 )$data

table(over$DefaulterFlag)
table(under$DefaulterFlag)
table(both$DefaulterFlag)

############################
# First model
############################

set.seed(123)
reg <- glm(DefaulterFlag ~ ., family = binomial, data = train)
summary(reg)

reg_class <- predict(reg, newdata = test, type = "response")
summary(reg_class)

pred <- as.factor(ifelse(reg_class > 0.5, 1, 0))
summary(pred)

confusionMatrix(pred, test$DefaulterFlag, positive = "1")

############################
# Parsimonious model
############################

set.seed(123)
reg1 <- glm(DefaulterFlag ~ 
              NOOFDEPE + SALDATFR + TENORYR + DWNPMFR + PROFBUS + QUALHSC + 
              QUAL_PG + SEXCODE + FULLPDC + FRICODE + WASHCODE, 
            family = binomial, data = train)
summary(reg1)

reg1_class <- predict(reg1, newdata = test, type = "response")
summary(reg1_class)

pred <- as.factor(ifelse(reg1_class > 0.5, 1, 0))
summary(pred)

confusionMatrix(pred, test$DefaulterFlag, positive = "1")

############################
# roc 
############################

par (pty = "s")
roc(train$DefaulterFlag, reg$fitted.values, 
    plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
    xlab = "FALSE POSITIVE PERCENTAGE", ylab = "TRUE POSITIVE PERCENTAGE", 
    col = "#2c7fb8", lwd = 4, print.auc = TRUE)

roc(train$DefaulterFlag, reg1$fitted.values, 
    plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
    col = "#4daf4a", lwd = 4, print.auc = TRUE, print.auc.y = 40, add = TRUE)

##############################
# Over sampling
##############################

train <- over
set.seed(123)
reg2 <- glm(DefaulterFlag ~ ., 
            family = binomial, data = train)
summary(reg2)

reg2_class <- predict(reg2, newdata = test, type = "response")
summary(reg2_class)

pred <- as.factor(ifelse(reg2_class > 0.5, 1, 0))
summary(pred)

confusionMatrix(pred, test$DefaulterFlag, positive = "1")

roc(train$DefaulterFlag, reg2$fitted.values, 
    plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
    col = "#ffaf4a", lwd = 4, print.auc = TRUE, print.auc.y = 60, add = TRUE)

##############################
# Under sampling
##############################

train <- under
set.seed(123)
reg3 <- glm(DefaulterFlag ~ ., 
            family = binomial, data = train)
summary(reg3)

reg3_class <- predict(reg3, newdata = test, type = "response")
summary(reg3_class)

pred <- as.factor(ifelse(reg3_class > 0.5, 1, 0))
summary(pred)

confusionMatrix(pred, test$DefaulterFlag, positive = "1")

roc(train$DefaulterFlag, reg3$fitted.values, 
    plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
    col = "#ffafcc", lwd = 4, print.auc = TRUE, print.auc.y = 80, add = TRUE)

##############################
# Decision Tree
##############################

set.seed(123)
myindex <- createDataPartition(data$DefaulterFlag, p = 0.6, list = FALSE)
train <- data[c(myindex),]
test <- data[-c(myindex),]

set.seed(123)
reg4 <- rpart(DefaulterFlag ~ ., data = train)
summary(reg4)
rpart.plot(reg4, extra = 2)

reg4_class <- predict(reg4, newdata = test, type = "class")
summary(reg4_class)

confusionMatrix(reg4_class, test$DefaulterFlag, positive = "1")
varImp(reg4)
varImpPlot(reg4, sort = TRUE, n.var = 10, main="TOP TEN VARIABLES")

##############################
# Decision tree oversampling
##############################
train <- over

set.seed(123)
reg5 <- rpart(DefaulterFlag ~ ., data = train)
summary(reg5)
rpart.plot(reg5, extra = 2)

reg5_class <- predict(reg5, newdata = test, type = "class")
summary(reg5_class)

confusionMatrix(reg5_class, test$DefaulterFlag, positive = "1")
varImp(reg5)

##############################
# Decision tree undersampling
##############################
train <- under

set.seed(123)
reg6 <- rpart(DefaulterFlag ~ ., data = train)
summary(reg6)
rpart.plot(reg6, extra = 2)

reg6_class <- predict(reg6, newdata = test, type = "class")
summary(reg6_class)

confusionMatrix(reg6_class, test$DefaulterFlag, positive = "1")
varImp(reg6)


##############################
# RandomForest
##############################

train <- traindata

set.seed(123)
reg7 <- randomForest(DefaulterFlag ~ ., data = train)
summary(reg7)
plot(reg7)
varImpPlot(reg7, sort = TRUE, n.var = 10, main="TOP TEN VARIABLES")
reg7_class <- predict(reg7, newdata = test, type = "class")
reg7_class1 <- predict(reg7, newdata = test, type = "prob")
summary(reg7_class)
par(pty="s")

aucdt <- roc(test$DefaulterFlag, reg7_class1[,1], 
             plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
             col = "#ffafcc", lwd = 4, print.auc = TRUE #, print.auc.y = 80, add = TRUE
)

confusionMatrix(reg7_class, test$DefaulterFlag, positive = "1")
varImp(reg7)

##############################
# RandomForest with tuning
##############################

train <- data.frame(train)
set.seed(123)
tuneRF(train[,-13], train[,13], 
       stepFactor = 2, plot = TRUE, ntreeTry = 300, improve = .01)

set.seed(123)
reg8 <- randomForest(DefaulterFlag ~ ., data = train, mtry = 2, ntree = 300)
plot(reg8)

reg8_class <- predict(reg8, newdata = test, type = "class")
summary(reg8_class)

confusionMatrix(reg8_class, test$DefaulterFlag, positive = "1")
varImp(reg8)

##############################
# RandomForest with tuning - Over
##############################

train <- over
set.seed(123)
tuneRF(train[,-13], train[,13], 
       stepFactor = 2, plot = TRUE, ntreeTry = 300, improve = .01)

set.seed(123)
reg8 <- randomForest(DefaulterFlag ~ ., data = train, mtry = 12, ntree = 300)
plot(reg8)

reg8_class <- predict(reg8, newdata = test, type = "class")
summary(reg8_class)

confusionMatrix(reg8_class, test$DefaulterFlag, positive = "1")
varImp(reg8)

##############################
# RandomForest with tuning - under
##############################

train <- under
set.seed(123)
tuneRF(train[,-13], train[,13], 
       stepFactor = 2, plot = TRUE, ntreeTry = 300, improve = .01)

set.seed(123)
reg8 <- randomForest(DefaulterFlag ~ ., data = train, mtry = 2, ntree = 300)
plot(reg8)

reg8_class <- predict(reg8, newdata = test, type = "class")
summary(reg8_class)

confusionMatrix(reg8_class, test$DefaulterFlag, positive = "1")
varImp(reg8)

##############################
# RandomForest with tuning - both
##############################

train <- both
set.seed(123)
tuneRF(train[,-13], train[,13], 
       stepFactor = 2, plot = TRUE, ntreeTry = 300, improve = .01)

set.seed(123)
reg8 <- randomForest(DefaulterFlag ~ ., data = train, mtry = 12, ntree = 300)
plot(reg8)

reg8_class <- predict(reg8, newdata = test, type = "class")
summary(reg8_class)

confusionMatrix(reg8_class, test$DefaulterFlag, positive = "1")
varImp(reg8)

