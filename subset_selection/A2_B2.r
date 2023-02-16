#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B2 ooooooooooooooooooooo
#oooooooooo ISL Section 6.6 Exercise 8 ooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

# Goal: In this problem, we will generate simulated data, and use this data to perform best subset selection

set.seed(17)

#_________________PART A_______________________
#______________________________________________
# Goal: generate a predictor X of length n = 100, as well as a noise vector ϵ of length n = 100 with rnorm()

#Simulate n = 100 observations, x and e are both with length 100 by rnorm() function
n = 100
x = rnorm(100)
e = rnorm(100)

#_________________PART B_______________________
#______________________________________________
# Goal:  Generate a response vector Y of length n = 100 according to the model Y = β0 + β1X + β2X2 + β3X3 + ϵ,
# where β0, β1, β2, and β3 are constants.

beta =c(2,1,-0.2,0.5)
y = beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3 + e

#_________________PART C_______________________
#______________________________________________
# Goal: Use the regsubsets() function to perform best subset selection
# What is the best model obtained according to Cp, BIC, and adjusted R^2? 
# Show some plots to provide evidence for your answer, and report the coefficients of the best model obtained. 

# Insert X and Y in a matrix
data_matrix = y
# We have 10 predictors so i in 1:10
# The process involves generating a matrix of data with all possible combinations of predictor variables 
# (up to a maximum of 10), and fitting a linear regression model on each combination. 
# Therefore we should create this matrix
for (i in 1:10){
  data_matrix = cbind(data_matrix, x^i)
}

# Create data frame
generated_data = data.frame(data_matrix)
names(generated_data) = c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10')
# generated_data now contains 'y','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10'

# Performing best subset selection
# Using regsubsets() built-in function from leaps lib in r
library(leaps)
# The response variable y is regressed against all the predictor variables in the data frame 
# '.' is used to indicate "all other columns" 
# The nvmax parameter is set to 10 to specify that up to 10 predictor variables can be used in the model. 
reg_fitted = regsubsets(y~., generated_data, nvmax=10)

#--------------[i]----------------
# What is the best model obtained according to Cp, BIC, and adjusted R2?

# The best models are selected based on three different criteria: 
# the adjusted R^2, the Cp statistic (also known as the Mallows' Cp), and the Bayesian information criterion (BIC).

# The summary includes the adjusted R^2, the Cp statistic, and the BIC for all possible combinations of predictor variables
summary = summary(reg_fitted)
summary

# The index of the model with the highest adjusted R^2 is determined using the which.max() function on the adjr2 component of the summary object.
adjr2 = summary$adjr2
which.max(adjr2)
cp = summary$cp
which.min(cp)
bic = summary$bic
which.min(bic)

# So base on the above results the best models are the third model and the 6th model (Model #3 and Model #6)
# Then we plot the models to visualize

#--------------[ii]----------------
# Provide evidence for the best models wit plotting

# set up a 2x2 grid of plots
layout(matrix(1:4, nrow=2, ncol=2))
# plot RSS
plot(summary$rss, xlab='Number of Variables', ylab='RSS', type='l')
# plot Adjusted R Square
plot(summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')
# plot Cp Mallows
plot(summary$cp, xlab='Number of Variables', ylab='Cp', type='l')

# loop over the three graphs and plot the corresponding points
for (i in c(which.max(summary$adjr2), which.min(summary$cp), which.min(summary$bic))) {
  points(i, summary$adjr2[i], col='#d13ed1', cex=2, pch=20)
}

# plot BIC
plot(summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
points(which.min(summary$bic), summary$bic[which.min(summary$bic)], col='#d13ed1', cex=2, pch=20)

# Report the coefficients of the best model obtained.
cat ("coefficients of model 3", coef(reg_fitted,3))
cat ("coefficients of model 6", coef(reg_fitted,6))

#_________________PART D_______________________
#______________________________________________
# Goal: Repeat (c) using forward stepwise selection and also using backwards stepwise selection. 
# How does your answer compare to the results in (c)?

# Performing forward stepwise selection
forward_reg_fitted =regsubsets(Y~., method="forward", data=generated_data, nvmax=10)

# The summary includes the adjusted R^2, the Cp statistic, and the BIC for all possible combinations of predictor variables
summary = summary(forward_reg_fitted)
summary

# The index of the model with the highest adjusted R^2 is determined using the which.max() function on the adjr2 component of the summary object.
adjr2 = summary$adjr2
which.max(adjr2)
cp = summary$cp
which.min(cp)
bic = summary$bic
which.min(bic)
# So base on the above results the best models are the third model and the 6th model (Model #3 and Model #6)

# Performing backwards stepwise selection
backward_reg_fitted =regsubsets(Y~., method="backward", data=generated_data, nvmax=10)

# The summary includes the adjusted R^2, the Cp statistic, and the BIC for all possible combinations of predictor variables
summary(backward_reg_fitted)
summary

# The index of the model with the highest adjusted R^2 is determined using the which.max() function on the adjr2 component of the summary object.
adjr2 = summary$adjr2
which.max(adjr2)
cp = summary$cp
which.min(cp)
bic = summary$bic
which.min(bic)
# So base on the above results the best models are the third model and the 6th model (Model #3 and Model #6)


#--------------[i]----------------
#Comparing coefficients of 3 used methods for best models #3 and #6 individually

print ("coefficients of model #3 with the best subset selection, forward stepwise selection, backward stepwise selection")
c(coef(reg_fitted,3), coef(forward_reg_fitted,3), coef(backward_reg_fitted,3))

print ("coefficients of model #6 with the best subset selection, forward stepwise selection, backward stepwise selection")
c(coef(reg_fitted,6), coef(forward_reg_fitted,6), coef(backward_reg_fitted,6))


#_________________PART E_______________________
#______________________________________________
# Goal: fit a lasso model to the simulated data. Use cross-validation to select the optimal value of λ. 
# Create plots of the cross-validation error as a function of λ.
# Report the resulting coefficient estimates, and discuss the results obtained.

set.seed((2022))

library(glmnet)
x <- as.matrix(generated_data[, -1])
y <- generated_data$y

# Define the sequence of lambda values to use
lambda_seq <- 10^seq(10, -2, length = 100)

# Fit the Lasso regression model
lasso_model <- cv.glmnet(x, y, alpha = 1, lambda = lambda_seq)

# Perform cross-validation and obtain the mean squared error for each value of lambda
cvfit <- cv.glmnet(x, y, alpha = 1)

# The best value of lambda that minimizes the mean squared error
best_lam <- cvfit$lambda.min

# Fit the Lasso model using the optimal lambda value
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lam)

# Plot the mean squared error versus log(lambda)
plot(cvfit, type = "p", xlab = "Log(lambda)", ylab = "Mean Squared Error")

# Extract the coefficients for the optimal Lasso model
lasso_coefs <- coef(lasso_model, s = best_lam)
lasso_coefs

# Discuss the results obtained.
"""
The output shows that the intercept, X1, X2, and X3 have non-zero coefficients while the coefficients for X4 to X10 are zero.
This means that the Lasso model has identified these four predictors, intercept, X1, X2, and X3, 
as the most important predictors for explaining the variation in the response variable. 
The magnitude and sign of the non-zero coefficients give us information about the direction 
and strength of the relationship between each predictor and the response variable.
For example, a positive coefficient for X1 means that an increase in the value of X1 is associated with an increase in the response variable,
while a negative coefficient for X3 means that an increase in the value of X3 is associated with a decrease in the response variable.
"""



