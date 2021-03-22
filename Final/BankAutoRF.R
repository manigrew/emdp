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
data <- xlsx[,c(4:16, 19)]

#tab <- lapply(data1, table)

# no missing values
summary(data)

str(data)
names(data)
#data[, 1:5]  <- lapply(data[, 1:5], scale)
data[,1] <- (data[,1] - min(data[,1])) / (max(data[,1]) - min(data[,1]))
data[,2] <- (data[,2] - min(data[,2])) / (max(data[,2]) - min(data[,2]))
data[,3] <- (data[,3] - min(data[,3])) / (max(data[,3]) - min(data[,3]))
data[,4] <- (data[,4] - min(data[,4])) / (max(data[,4]) - min(data[,4]))
data[,5] <- (data[,5] - min(data[,5])) / (max(data[,5]) - min(data[,5]))
data[,6] <- (data[,6] - min(data[,6])) / (max(data[,6]) - min(data[,6]))

data[, 7:14] <- lapply(data[, 7:14], function(x) {recode(x, "0=0; 1=1", as.factor = TRUE)})
str(data)
data[,14] <- factor(data[14])
set.seed(123)
myindex <- createDataPartition(data$DefaulterFlag, p = 0.8, list = FALSE)
traindata <- data[c(myindex),]
testdata <- data[-c(myindex),]

train <- traindata
test <- testdata

table(train$DefaulterFlag)
over <- ovun.sample(DefaulterFlag ~ ., data = train, method = "over", seed = 123,
                    N = table(train$DefaulterFlag)[2] * 2)$data
under <- ovun.sample(DefaulterFlag ~ ., data = train, method = "under", seed = 123,
                     N = table(train$DefaulterFlag)[1] * 2)$data
both <- ovun.sample(DefaulterFlag ~ ., data = train, method = "both", seed = 123,
                    p = 0.5 )$data


##############################
# RandomForest with tuning - Over
##############################

train <- as.data.frame(over)
set.seed(123)
tuneRF(train[,-14], train[,14], 
       stepFactor = 2, plot = TRUE, ntreeTry = 500, improve = .01)

set.seed(123)
reg8 <- randomForest(DefaulterFlag ~ ., data = train, mtry = 6, ntree = 500)
plot(reg8)

reg8_class <- predict(reg8, newdata = test, type = "class")
summary(reg8_class)
confusionMatrix(reg8_class, test$DefaulterFlag, positive = "1")

confusionMatrix(reg8_class, test$DefaulterFlag, positive = "1")
varImp(reg8)

##############################
# RandomForest with tuning - under
##############################

train <- under
set.seed(123)
tuneRF(train[,-14], train[,14], 
       stepFactor = 2, plot = TRUE, ntreeTry = 500, improve = .01)

set.seed(123)
reg8 <- randomForest(DefaulterFlag ~ ., data = train, mtry = 2, ntree = 500)
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
tuneRF(train[,-14], train[,14], 
       stepFactor = 2, plot = TRUE, ntreeTry = 300, improve = .01)

set.seed(123)
reg8 <- randomForest(DefaulterFlag ~ ., data = train, mtry = 6, ntree = 300)
plot(reg8)

reg8_class <- predict(reg8, newdata = test, type = "class")
str(reg8_class)
pred <- as.factor(ifelse(reg8_class[,1] > 0.485, 1, 0))
summary(pred)

confusionMatrix(reg8_class, test$DefaulterFlag, positive = "1")
varImp(reg8)


