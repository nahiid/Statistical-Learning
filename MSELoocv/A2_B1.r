#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B1 ooooooooooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

# Goal: Validate MSELoocv closed formula

set.seed(2023)

#Simulate n = 100 observations from the model Y = 2 + 3X + e where X, e are both i.i.d. N(0, 1)
n = 100
x = rnorm(n)
e = rnorm(n)
y = 2 + 3*x + e

# Creating data frame from the generated data
generated_data = data.frame(cbind(y,x))

#--------------[1]----------------
# Calculating the leave-one-out cross-validation mean squared error (LOOCV MSE) manually.
# OS_er is out of sample error
# The OS_err variable is initialized as a vector of zeros with length n.
OS_err = rep(0,n)

for (i in 1:n)
{
    # A for loop is used to iterate through each observation in the data set. 
    # For each iteration, the ith observation is removed from the data set to create a training set, 
    # and a linear regression model is fit to the training set using the lm function.
    #Creating train data set with removing ith observation 
    train_set = generated_data[-i,] 
    # Fit the model
    fit_data =lm(y ~ x, data=train_set) 
    # The OS_y variable is set to the ith observation's y value, 
    # which represents the value of y that was left out in the training set.
    # y value of out-of-sample data 
    OS_y = generated_data$y[i] 
    # Prediction for out-of-sample data using the linear regression model fit on the training set.
    OS_predict = predict(fit_data ,generated_data)[i] 
    # Prediction error y value - prediction
    OS_err[i] = OS_y - OS_predict 
}
r_MSELOOCV = mean(OS_err^2)

#--------------[2]----------------
# [2] Calculating LOOCV MSE with r built-in function
# This code calculates the LOOCV MSE using the built-in cv.glm function from the `boot'
library(boot)
glm.fit = glm(y ~ x ,data=generated_data)
err =cv.glm(generated_data ,glm.fit)

# The err$delta object contains a vector of length two, with the first element representing the estimated LOOCV MSE. 
built_in_MSELoocv = err$delta[1]

#--------------[3]----------------
# Calculating LOOCV MSE with a closed formula
# Prdict y with lm model
lm.fit =lm(y ~ x, data = generated_data)
ypred = cbind(rep(1,n),x) %*% coef(lm.fit)

# Leverage of point i with a given formula
h = 1/n + ((x - mean(x))^2) / sum((x - mean(x))^2)

# Calculationg MSELoocv with closed form formula
closed_formula_MSELOOCV = mean( ((y - ypred)/(1-h))^2)

#-------------- Validation  ----------------
# Validate that all calculated LOOCV MSE are the same.
cat("MSE LOOCV calculated with r code manually: ", r_MSELOOCV)
cat("MSE LOOCV calculated with Built In function in r: ", built_in_MSELoocv)
cat("MSE LOOCV calculated with closed formula in r: ", closed_formula_MSELOOCV)
