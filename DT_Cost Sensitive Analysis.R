#*********Cost Sensitive Example: Statlog Heart***********
#1) Import dataset to R and change the required variables to factor type
heart <-  read.csv(file.choose())
str(heart)

#convert target variable to factor type
heart$statlogheart <- factor(heart$statlogheart)

#convert other variables to factor type
heart[c(2,3,6,7,9,13)] <- lapply(heart[c(2,3,6,7,9,13)], factor)

#check the structure again
str(heart)


#2)check the levels
str(heart$statlogheart)
#levels are: "1" - Absence and "2" - Presence of heart disease

table(heart$statlogheart)
#120 actually have heart disease

#Summarize variable max heart rate
summary(heart$maxheartrate)
#average max heart rate of patients is 149.7

#3) Randomly partition data into one training sample (60%) and two testing samples (20% each)
library(caret)
set.seed(100)
inTrain <- createDataPartition(heart$statlogheart, p = 0.6, list = F)
traindata <- heart[inTrain,]
testdata <-  heart[-inTrain,]

inTest <- createDataPartition(testdata$statlogheart, p = 0.5, list = F)
testdata1 <-  testdata[inTest,]
testdata2 <-  testdata[-inTest,]

#4) Build decision tree models and evaluate the decision tree model for two testing samples using Confusion Matrix and mmetric
library(C50)
DT_model <-  C5.0(statlogheart~., data = traindata)
summary(DT_model)
#For 1st testing dataset
DT_predictions1 <- predict(DT_model,testdata1)
#confusion matrix and mmetrics
library(rminer)
DT_matrix_T1 <- confusionMatrix(DT_predictions1, testdata1$statlogheart, positive = "1" ,dnn = c("prediction", "true"))
DT_matrix_T1
#            true
#prediction  1    2
#1           19   5
#2           11   19

mmetric(testdata1$statlogheart, DT_predictions1, c("ACC","PRECISION","TPR","F1"))
#ACC        PRECISION1  PRECISION2      TPR1       TPR2       F11        F12 
#70.37037   79.16667    63.33333        63.33333   79.16667   70.37037   70.37037 


str(DT_predictions1)
#Class-1 is 1: positive (absence) and class-2 is 2: negative (presence)

#For 2nd testing dataset
DT_predictions2 <- predict(DT_model,testdata2)
#confusion matrix and mmetrics
DT_matrix_T2 <- confusionMatrix(DT_predictions2, testdata2$statlogheart, positive = "1" ,dnn = c("prediction", "true"))
DT_matrix_T2
#            true
#prediction  1    2
#1           18   3
#2           12   21

mmetric(testdata2$statlogheart, DT_predictions2, c("ACC","PRECISION","TPR","F1"))
#ACC        PRECISION1  PRECISION2  TPR1       TPR2       F11        F12 
#72.22222   85.71429    63.63636    60.00000   87.50000   70.58824   73.68421 

str(DT_predictions2)
#Class-1 is 1: positive (absence) and class-2 is 2: negative (presence)

#5)calculate the cost estimated per patient
#For 1st testing data
FP_DT_T1 <- DT_matrix_T1$table[1,2]
FN_DT_T1 <- DT_matrix_T1$table[2,1]
TP_DT_T1 <- DT_matrix_T1$table[1,1]
TN_DT_T1 <- DT_matrix_T1$table[2,2]

#total testing1 sample size
n_T1 <- FP_DT_T1 + FN_DT_T1 + TP_DT_T1 + TN_DT_T1
n_T1 <- nrow(testdata1)
#total cost
cost_DT_T1 <- FP_DT_T1*5000 + FN_DT_T1*500
#cost per patient
cost_DT_T1_avg <- cost_DT_T1/n_T1
cost_DT_T1_avg
#cost per patient = 564.81

#For 2nd testing data
FP_DT_T2 <- DT_matrix_T2$table[1,2]
FN_DT_T2 <- DT_matrix_T2$table[2,1]
TP_DT_T2 <- DT_matrix_T2$table[1,1]
TN_DT_T2 <- DT_matrix_T2$table[2,2]

#total testing1 sample size
n_T2 <- FP_DT_T2 + FN_DT_T2 + TP_DT_T2 + TN_DT_T2
n_T2 <- nrow(testdata2)
#total cost
cost_DT_T2 <- FP_DT_T2*5000 + FN_DT_T2*500
#cost per patient
cost_DT_T2_avg <- cost_DT_T2/n_T2
cost_DT_T2_avg
#cost per patient = 388.89

#6)Create a cost sensitive decision tree on training data.
matrix_dinames <- list(c("Predicted-1","Predicted-2"), c("1","2"))
cost_matrix <- matrix(c(0,1,10,0), nrow = 2, dimnames = matrix_dinames)
cost_matrix

#7) and 8): evaluating two test samples based on cost matrix and the costs estimated per patient 
DT_cost_model <-  C5.0(statlogheart~., data = traindata, cost = cost_matrix)

#For 1st testing dataset
DT_cost_predictions1 <- predict(DT_cost_model,testdata1)

#confusion matrix and mmetrics
DT_cost_matrix_T1 <- confusionMatrix(DT_cost_predictions1, testdata1$statlogheart, positive = "1" ,dnn = c("prediction", "true"))
DT_cost_matrix_T1
#            true
#prediction  1    2
#1           8    0
#2           22   24

FP_cost_DT_T1 <- DT_cost_matrix_T1$table[1,2]
FN_cost_DT_T1 <- DT_cost_matrix_T1$table[2,1]

#total cost
cost_DT_adjusted_T1 <- FP_cost_DT_T1*5000 + FN_cost_DT_T1*500
#cost per patient
cost_DT_adjusted_T1_avg <- cost_DT_adjusted_T1/n_T1
cost_DT_adjusted_T1_avg
#cost per patient = 203.70

#overall evaluation metrics
mmetric(testdata1$statlogheart, DT_cost_predictions1, c("ACC","PRECISION","TPR","F1"))
#ACC        PRECISION1  PRECISION2   TPR1       TPR2        F11        F12 
#59.25926   100.00000   52.17391     26.66667   100.00000   42.10526   68.57143 
#Overall accuarcy has been reduced as compared to accuracy of test1 data without cost matrix
#Precision1 and TPR2 have been increased after implemented cost matrix
#Rest all the other metrics (except Precision1 and TPR2) value have been reduced as compared to test1 data without cost matrix

str(DT_cost_predictions1)
#Class-1 is 1: positive (absence) and class-2 is 2: negative (presence)


#For 2nd testing dataset
DT_cost_predictions2 <- predict(DT_cost_model,testdata2)

#confusion matrix and mmetrics
DT_cost_matrix_T2 <- confusionMatrix(DT_cost_predictions2, testdata2$statlogheart, positive = "1" ,dnn = c("prediction", "true"))
DT_cost_matrix_T2
#            true
#prediction  1    2
#1           6    1
#2           24   23

FP_cost_DT_T2 <- DT_cost_matrix_T2$table[1,2]
FN_cost_DT_T2 <- DT_cost_matrix_T2$table[2,1]

#total cost
cost_DT_adjusted_T2 <- FP_cost_DT_T2*5000 + FN_cost_DT_T2*500
#cost per patient
cost_DT_adjusted_T2_avg <- cost_DT_adjusted_T2/n_T2
cost_DT_adjusted_T2_avg
#cost per patient = 314.81

#overall evaluation metrics
mmetric(testdata2$statlogheart, DT_cost_predictions2, c("ACC","PRECISION","TPR","F1"))
#ACC        PRECISION1  PRECISION2      TPR1       TPR2        F11        F12 
#53.70370   85.71429    48.93617        20.00000   95.83333    32.43243   64.78873 
#Overall accuarcy has been reduced as compared to accuracy of test2 data without cost matrix
#Precision1 is same in both cases i.e. with and without cost matrix model
#TPR2 has been increased after implemented cost matrix
#Rest all the other metrics (except Precision1 and TPR2) value have been reduced as compared to test2 data without cost matrix

str(DT_cost_predictions2)
#Class-1 is 1: positive (absence) and class-2 is 2: negative (presence)



