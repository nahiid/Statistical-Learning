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
fitted_model = regsubsets(y~.,generated_data,nvmax=10)

#--------------[i]----------------
# What is the best model obtained according to Cp, BIC, and adjusted R2?

# The best models are selected based on three different criteria: 
# the adjusted R^2, the Cp statistic (also known as the Mallows' Cp), and the Bayesian information criterion (BIC).

# The summary includes the adjusted R^2, the Cp statistic, and the BIC for all possible combinations of predictor variables
summary = summary(fitted_model)

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
cat ("coefficients of model 3", coef(fitted_model,3))
cat ("coefficients of model 6", coef(fitted_model,6))
