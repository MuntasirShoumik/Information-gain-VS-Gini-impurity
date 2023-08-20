library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(ggplot2)


data <- read.csv("D:/AIUB/DATA WAREHOUSING AND DATA MINING/final project/data/training.csv")

head(data)

str(data)

data <- subset(data, select = -CustomerID)

head(data)


sum(duplicated(data))


sum(is.na(data))


data <- na.omit(data)


sum(is.na(data))

subset(as.matrix(Filter(is.numeric, na.exclude(distinct(data)))), 
       select = c(Total.Spend,Payment.Delay, Tenure)) %>% cor() %>% heatmap()



sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, ]
test   <- data[!sample, ]



rpart.plot(rpart(formula = Churn ~., data = train, 
                 method = "class", parms = list(split = "gini")), extra = 100)



DT_Model_gini <- rpart(formula = Churn ~., data = train, method = "class", parms = list(split = "gini"))



test_pred_gini = predict(DT_Model_gini, newdata= test, type = "class")


sum(test_pred_gini == test$Churn) / nrow(test)


confusion_matrix <- table(test_pred_gini, test$Churn)



print(confusion_matrix)


accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

report <- data.frame(Class = rownames(confusion_matrix),
                     Precision = precision,
                     Recall = recall,
                     F1_Score = f1_score)

print(report)



rpart.plot(rpart(formula = Churn ~., data = train, 
                 method = "class", parms = list(split = "information")), extra = 100)



DT_Model_info <- rpart(formula = Churn ~., data = train, method = "class", parms = list(split = "information"))


test_pred_info = predict(DT_Model_info, newdata= test, type = "class")



sum(test_pred_info == test$Churn) / nrow(test)


confusion_matrix <- table(test_pred_info, test$Churn)


print(confusion_matrix)


accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

report <- data.frame(Class = rownames(confusion_matrix),
                     Precision = precision,
                     Recall = recall,
                     F1_Score = f1_score)

print(report)


















