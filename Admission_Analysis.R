#College Admission Case study
getwd()
setwd("G:\\Simplilearn\\R\\Naveen")

library(dplyr)
library(readxl)

college = read.csv("College_admission.csv")


str(college)
sum(anyNA(college))

# Transforming the data in factor
college$rank = as.factor(college$rank)
college$Race = as.factor(college$Race)
college$admit = as.factor(college$admit)
college$Gender_Male = as.factor(college$Gender_Male)
college$ses = as.factor(college$ses)

summary(college)

gre_box = boxplot(college$gre)
gre_box$stats
gre_box$out

gpa_box = boxplot(college$gpa)
gpa_box$stats
gpa_box$out



d = density(college$gre)
plot(d)

gp = density(college$gpa)
plot(gp)





install.packages("e1071")
library(e1071)
skewness(college$gre) # checks for skewness of data
skewness(college$gpa)


# Running logistic model
# Because we have only two response here 0 & 1 so logistic will restrict 
# the response with 0 & 1

college_glm = glm(admit ~ ., family = "binomial", data = college)

summary(college_glm)


#preparing training and test data set

install.packages("caret")
library(caret)

set.seed(100)

# Randomly selecting the test & training data set
traindataindex = createDataPartition(college$admit, p =0.7, list = F) 

train_data = college[traindataindex,]
test_data = college[-traindataindex,]


#Builing logistic model
logmodel = glm(admit ~ gre + gpa + rank, family = "binomial", data = train_data)
summary(logmodel)

#predicting stage
 pred = predict(logmodel, newdata = test_data, type = "response")


# Recoding factors
y_pred_num = ifelse(pred > 0.5, 1, 0)

y_actual = test_data$admit

# Calculating accuracy
# accuracy = (correctly predicted class / total testing class) × 100%
mean(y_pred == y_actual) # Accuracy
cof_mat = table(y_pred,y_actual) # Confusion matrix
cof_mat

#downsample Model
down_train = downSample(x= train_data[,-1],
                        y = train_data$admit)

#BUliding second model

logmodel2 = glm(Class ~ gre + gpa + rank, family = "binomial",data = down_train)
summary(logmodel2)

pred1 = predict(logmodel2, newdata = test_data,type = "response")


#Recoding to factor
y1_pred_num = ifelse(pred1 > 0.5,1,0)
y1_pred = factor(y1_pred_num, levels = c(0,1))


#accuracy
#accuracy = (correctly predicted class / total testing class) × 100%
mean(y1_pred == y_actual) # this gives the accuracy 
confusion_m = table(y1_pred,y_actual)
confusion_m

#Accuracy comes out to be  0.6134454 for Downsample model

# Validation
# Validation is done to identify if our model is not underfitting and
# not overfitting


#K fold
# set seed
set.seed(100)
#define train control for k fold  cross validation
train_control = trainControl(method = "cv", number = 10)
# Fit Navie Bayes model
k_model = train(admit ~ gre + gpa + rank,data = college,
                trControl = train_control, method = "nb")

print(k_model)

# repeated k fold
set.seed(100)
tc = trainControl(method = "repeatedcv", number = 10,
                  repeats = 3)
k_model1 = train(admit ~ gre + gpa + rank,data = college,
                trControl = tc, method = "nb")
print(k_model1)


#Support Vector Machine

library(e1071)
classifier = svm(admit ~ gpa +gre +rank,data = train_data,
                 type = "C-classification",kernel= "linear",
                 probability = T, cost = 0.5)
p = predict(object = classifier,newdata = test_data)

mean(p == y_actual) # Accuracy
conf_mat = table(p,y_actual)
svm_accuracy = (conf_mat[1,1] + conf_mat[2,2])/nrow(test_data)
svm_accuracy
# Accuracy comes out to be 0.6890756 SVM linear

#non -linear kernel

classfy = svm(admit~ gre + gpa + rank, data = train_data,
              type = "C-classification", kernel="radial",
              probability = T , cost = 0.5)
p1 = predict(object = classfy,newdata = test_data)


mean(p1 == y_actual) # Accurcy 
matr = table(y_actual , p1)
matr
#aaccuracy comes out to be 0.7058824 for non-linear SVM 


# Decision Tree

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)

set.seed(100)
dt_model = rpart(formula = admit ~ gre +gpa + rank, train_data)


library(rpart.plot)
prp(dt_model)

# it will show the size of tree vs error
# it will also show that after a point size of tree does not reduce any error
# cp - complexity parameter - after which relative error is not decreasing
plotcp(dt_model)

m2 =prune(tree = dt_model,cp = 0.04)
prp(m2)

p2 =predict(object = dt_model,newdata = test_data,type = "class")

mean(p2 == y_actual)
# it has accuracy of 0.6218487 - Decision tree

# Random forest
 install.packages("randomForest")
library(randomForest)
set.seed(100) 

rfmodel = randomForest(admit ~ gre +gpa + rank,data = train_data,
                       ntree = 500, mtry = 3)
#ntree & mtry are the hyper parameters which needs to be tuned for 
# for better accuracy
plot(rfmodel)
legend('topright',colnames(rfmodel$err.rate), col = 1:3, fill = 1:3)

#as the graph says red line is for error in predicting 0s
# green line is for error in predicting 1s
# black ones is out of bag error - overall error rate

p3 = predict(object = rfmodel,newdata = test_data,type = "class")
y_actual = test_data$admit
mean(p3 == y_actual)

varImpPlot(rfmodel)
# accuracy comes out to be 0.6302521 - Random forest


# XGBOOST Model

install.packages("xgboost")
install.packages("Matrix")

library(xgboost)
library(Matrix)

#since xgboost works only on numeric vectors
# we are converting all category vector into numeric vector except the response variable
# that is sparse matrix

sparse_matrix = sparse.model.matrix(admit ~ gre + gpa + rank,data = train_data)


#Tune and run the model
dtrain = xgb.DMatrix(data = sparse_matrix,
                     label = as.numeric(as.character(train_data$admit)) ,
                     missing = NA)

# oredict the values in test
# score the test
sparse_test_mat = sparse.model.matrix(admit ~ gre + gpa + rank, data = test_data)

dtest = xgb.DMatrix(data = sparse_test_mat, 
                    label = as.numeric(as.character(test_data$admit)),
                    missing = NA)
watchlist = list(train = dtrain, eval = dtest)
# one way


xgmodel = xgb.train(data = dtrain, eta = 0.01,max_depth = 2,
                    objective = "binary:logistic",nrounds = 200,
                    watchlist = watchlist,verbose = 3,
                    early_stopping_rounds = 100)
#
xgb = xgboost(data = sparse_matrix,
              label = train_data$admit,
              eta = 0.01,
              nrounds = 30)

pxgb1 = predict(object = xgmodel,newdata = dtest)

xg_pred = ifelse(pxgb1 > 0.5,1,0)
mean(xg_pred == y_actual)

cf_mat = table(predicted = pxgb1 > 0.5 , actual = test_data$admit)

Acc = (cf_mat[1,1] + cf_mat[2,2])/nrow(test_data)
Acc

pxgb2 = predict(object = xgb,newdata = dtest)

xgb_pred = ifelse( pxgb2 > 0.5,1,0)
xgb_actual = test_data$admit
mean(xgb_pred == xgb_actual)
mean(xgb_actual == xgb_pred)
