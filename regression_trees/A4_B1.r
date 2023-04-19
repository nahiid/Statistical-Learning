#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B1 ooooooooooooooooooooo
#oooooooooo ISL Section 8.4 Exercise 8 ooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

set.seed(2022)
library(tree)
library(randomForest)
library(BART)
library(ISLR2)


#_________________PART A_______________________
#______________________________________________
# Goal: Splitting the dataset

train_set <- sample(1:nrow(Carseats), 100)
test_set <- Carseats[-train_set, ]
Sales.test <- Carseats$Sales[-train_set]

#_________________PART B_______________________
#______________________________________________
# Goal: Fit a regression tree to the training set. Plot the tree, and interpret the results. MSE?

carseat_fitted <- tree(Sales ~ . , Carseats, subset = train_set)
summary(carseat_fitted)
plot(carseat_fitted)

#Test MSE
prediction <- predict(carseat_fitted, newdata = Carseats[-train_set,])
testMSE = mean((prediction - Sales.test)^2)

#_________________PART C_______________________
#______________________________________________
# Goal: Use cross-validation to determine the optimal level of tree complexity. 
# Does pruning the tree improve the test MSE?

# Cross validation
cross_validation <- cv.tree(carseat_fitted)
plot(cross_validation$size, cross_validation$dev, type = "b")

# pruning
pruning <- prune.tree(carseat_fitted, best = 11)
plot(pruning)
text(pruning, pretty = 0)

# Compute Validation MSE
prediction <- predict(pruning, newdata = Carseats[-train_set,])
test_MSE <- mean((prediction - Sales.test)^2)

"""
The test MSE is computed for the pruned tree by using the predict() function to obtain the predicted values of Sales for the test set (Carseats[-train_set,]) 
and comparing them to the actual Sales values (Sales.test). 
The mean() and ^2 functions are used to compute the mean squared error. 
The resulting test MSE is lower than the test MSE obtained for the original tree, 
indicating that pruning has improved the performance of the model.
"""


#_________________PART D_______________________
#______________________________________________
# Goal: Use the bagging approach in order to analyze this data. What test MSE do you obtain? 
# Use the importance() function to determine which variables are most important.
preds <- ncol(Carseats)-1
bagging <- randomForest(Sales ~ ., data = Carseats, subset = train_set, mtry = preds, importance = TRUE)

importance(bagging)
# Most important variables are Price, ShelveLoc, CompPrice

# Calculating Validation MSE
prediction <- predict(bagging, newdata = Carseats[-train_set,])
mean((prediction - Sales.test)^2)

#_________________PART E_______________________
#______________________________________________
# Goal:  Use random forests to analyze this data. test MSE? 
#Use the importance() function to determine which variables are most important. 
#Describe the effect of m, the number of variables considered at each split, on the error rate obtained.

# Random forest
random_forest <- randomForest(Sales ~ ., data = Carseats, subset = train_set, mtry =9, importance = TRUE)

# test MSE
prediction <- predict(random_forest, newdata = Carseats[-train_set,])
testMSE <- mean((prediction - Sales.test)^2)
# with m = 9, the test MSE is slightly lower (this might vary with the rand.seed)

importance(random_forest)
# the most important variables are still Price, ShelveLoc and CompPrice

test_MSE <- rep(0,preds)
for(m in 1:preds){
  random_forest <- randomForest(Sales ~ ., data = Carseats, subset = train_set, mtry = m, importance = TRUE)
  prediction <- predict(random_forest, newdata = Carseats[-train_set,])
  test_MSE[m] <- mean((prediction - Sales.test)^2)
}
# Plotting to describe the effect of m:
plot(test_MSE, xlab = "m", ylab = "MSE")

#Results:
# base on plotting m = 4 is the best value.


#_________________PART F_______________________
#______________________________________________
# Goal: Now analyze the data using BART, and report your results.


# Extract design matrix and responses
X <- Carseats[, -1]
Y <- Carseats[, "Sales"]

# Splitting
# Inputs train_set and test
xtrain <- X[train_set, ]
xtest <- X[-train_set, ]
# Responses train_set and test
ytrain <- Y[train_set]
ytest <- Y[-train_set]

# Fitting model BART
bart <- gbart(xtrain, ytrain, X.test = xtest)

# Calculating Validation MSE
prediction <- bart$prediction.test.mean
testMSE <- mean((ytest - prediction)^2)
testMSE

"""
With the given set.seed(2022) statement, the reported test mean squared error (MSE) is 2.268147.
This means that on average, the BART model's predictions are off by the square root of 2.268147 units of sales for the test set. 
Lower values of test MSE indicate better performance, 
so a test MSE of 2.268147 suggests that the BART model provides reasonably accurate predictions 
of the sales of car seats for the test set.
"""
