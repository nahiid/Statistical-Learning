#_________________PART A________________________
#_______________________________________________
# Goal: Generate a simulated data set
# i. In this data set, what is n and what is p? 
# ii. Write out the model used to generate the data in equation form.

set.seed (1)
# rnorm(n, mean = 0, sd = 1)
x <- rnorm (100)
y <- x - 2 * x^2 + rnorm (100)

#--------------[i]----------------
# n is the number of the observation and here we created a dataset with 100 observations.
# So n is equal to 100
# p is the number of predictors. here predictors are x and x^2
# So p is equal to 2

#--------------[ii]----------------
# The model = x - 2 + 2*x^2 + error here rnorm(100) play the role of error that is a small number.
# We can write it in this way: f(x) = x - 2 + 2*x^2 
# So the model= Y = f(x) + error
# We know this error is irreducible which has the mean zero and sd=1 and they have a Normal dist.
# becuse we made it that way with rnorm function :)

#_________________PART B________________________
#_______________________________________________
# Goal: Create a scatterplot of X against Y. 
# i. Comment on what you find.

plot(x, y, main='scatterplot of X against Y',
   xlab='x', ylab='y', pch=19)

#--------------[i]----------------
# It obviously a quadratic function.
# Since plot indicates a distinct parabolic pattern.

#_________________PART C________________________
#_______________________________________________
# Goal: Set a random seed, and then compute the LOOCV errors that
# result from fitting the following four models using least squares

set.seed(2022)
x <- rnorm (100)
y <- x - 2 * x^2 + rnorm (100)

# Create a data frame with all predictors in all models.
data_frame = data.frame(y, x, x^2, x^3, x^4)

# We have to store all of the errors in the matrix then we could get the average of them.
# To initialize we plug zero as the elements of the matrix. but it could be any numbers.
# o_error stands for out of sample error
o_error = matrix(0, nrow=100, ncol = 4)

for (i in 1:100)
{
    # we should exclude the ith row means the ith input out of the data and use it as trained data.
    train_data = data_frame[-i,]

    # our linear regression models are fit using the lm function with the traindata data frame, 
    # with the response variable y and different predictor variables 
    # x, x + x^2, x + x^2 + x^3, and x + x^2 + x^3 + x^4 respectively.

    # Fit the data with the model : Y = β0 + β1X + ϵ
    fitted_lm1 =lm(y ~ x, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + ϵ
    fitted_lm2 =lm(y ~ x + x.2, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + β3X3 + ϵ
    fitted_lm3 =lm(y ~ x + x.2 + x.3, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + β3X3 + β4X4 + ϵ
    fitted_lm4 =lm(y ~ x + x.2 + x.3 + x.4, data=train_data)

    # data$y[i] accesses the i-th element of the y column in the data data frame
    # which here is considered as output of out of sample data since i excluded from the trained data 
    # and we still in the for loop scope
    real_y_oos = data_frame$y[i]

    # Prediction using the four given models. 
    predict1_y_oos = predict(fitted_lm1 ,data_frame)[i]
    predict2_y_oos = predict(fitted_lm2 ,data_frame)[i]
    predict3_y_oos = predict(fitted_lm3 ,data_frame)[i]
    predict4_y_oos = predict(fitted_lm4 ,data_frame)[i]

    # Now we calculate the errors for out of sample data and 
    # insert the errors into the out of sample error that we have made.
    # For the first model
    o_error[i,1] = real_y_oos-predict1_y_oos
    # For the second model
    o_error[i,2] = real_y_oos-predict2_y_oos
    # For the third model
    o_error[i,3] = real_y_oos-predict3_y_oos
    # For the forth model
    o_error[i,4] = real_y_oos-predict4_y_oos
  
}

# Initialize zero vector for MSE with 4 elements
MSE=rep(0,4)
# Now we can easily compute the average of the errors for each model.
#. i is defined! I thought it is out of the scope:/ so whenever i run the below code with i as index got a wrong answer!!
for (j in 1:4){
  MSE[j] = mean( o_error[,j]^2 )
}
LOOCV_MSE_C <- MSE
LOOCV_MSE_C

#_________________PART D________________________
#_______________________________________________
# Goal:  Underestanding the function of setting seed (redo previous part with another seed)
# i. Are your results the same as what you got in (c)? Why?

set.seed(2023)
x <- rnorm (100)
y <- x - 2 * x^2 + rnorm (100)

# Create a data frame with all predictors in all models.
data_frame = data.frame(y, x, x^2, x^3, x^4)

# We have to store all of the errors in the matrix then we could get the average of them.
# To initialize we plug zero as the elements of the matrix. but it could be any numbers.
# o_error stands for out of sample error
o_error = matrix(0, nrow=100, ncol = 4)

for (i in 1:100){
    # we should exclude the ith row means the ith input out of the data and use it as trained data.
    train_data = data_frame[-i,]

    # our linear regression models are fit using the lm function with the traindata data frame, 
    # with the response variable y and different predictor variables 
    # x, x + x^2, x + x^2 + x^3, and x + x^2 + x^3 + x^4 respectively.

    # Fit the data with the model : Y = β0 + β1X + ϵ
    fitted_lm1 =lm(y ~ x, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + ϵ
    fitted_lm2 =lm(y ~ x + x.2, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + β3X3 + ϵ
    fitted_lm3 =lm(y ~ x + x.2 + x.3, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + β3X3 + β4X4 + ϵ
    fitted_lm4 =lm(y ~ x + x.2 + x.3 + x.4, data=train_data)

    # data$y[i] accesses the i-th element of the y column in the data data frame
    # which here is considered as output of out of sample data since i excluded from the trained data 
    # and we still in the for loop scope
    real_y_oos = data_frame$y[i]

    # Prediction using the four given models. 
    predict1_y_oos = predict(fitted_lm1 ,data_frame)[i]
    predict2_y_oos = predict(fitted_lm2 ,data_frame)[i]
    predict3_y_oos = predict(fitted_lm3 ,data_frame)[i]
    predict4_y_oos = predict(fitted_lm4 ,data_frame)[i]

    # Now we calculate the errors for out of sample data and 
    # insert the errors into the out of sample error that we have made.
    # For the first model
    o_error[i,1] = real_y_oos-predict1_y_oos
    # For the second model
    o_error[i,2] = real_y_oos-predict2_y_oos
    # For the third model
    o_error[i,3] = real_y_oos-predict3_y_oos
    # For the forth model
    o_error[i,4] = real_y_oos-predict4_y_oos

}

# Initialize zero vector for MSE with 4 elements
MSE=rep(0,4)
# Now we can easily compute the average of the errors for each model.
#. i is defined! I thought it is out of the scope:/ so whenever i run the below code with i as index got a wrong answer!!
for (j in 1:4){
  MSE[j] = mean( o_error[,j]^2 )
}
MSE

#--------------[i]----------------
# We use another random seed so results are different since the data sets are different!

#_________________PART E________________________
#_______________________________________________
# Goal: Which of the models in (c) had the smallest LOOCV error? Is this what you expected? Explain your answer.

# Here is the LOOCV error for the model in part C
LOOCV_MSE_C

"""
  LOOCV error for the model in part C as follows>> 
  LOOCV_MSE_C for the model 1 (Y = β0 + β1X + ϵ): 
  12.258464  
  LOOCV_MSE_C for the model 2 (Y = β0 + β1X + β2X2 + ϵ): 
  1.097851  
  LOOCV_MSE_C for the model 3(Y = β0 + β1X + β2X2 + β3X3 + ϵ): 
  1.072996  
  LOOCV_MSE_C for the model 4 (Y = β0 + β1X + β2X2 + β3X3 + β4X4 + ϵ): 
  1.080529

  The first model has a larger error compared to the other models due to its linear structure, 
  and the scatter plot of data (observed in part B) indicates that the data has a quadratic trend.
  which makes this linear model less flexible and unable to fit the data well. 
  So the last three MSEs have the smaller value which is good! 
  The trend for data is quadratic Although the model 3 and 4 are not quadratic but they have small MSE which is good:)
"""

#_________________PART F________________________
#_______________________________________________
# Goal: Comment on statistical significance of the coefficient estimates from fitting each of the models in (c) using least squares. 
# i. Do these results agree with the conclusions drawn based on the cross-validation results?

# Use summery() to comparing the results
summary(fitted_lm1)
summary(fitted_lm2)
summary(fitted_lm3)
summary(fitted_lm4)

"""
>> summary(fitted_lm1)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -1.6917     0.2777  -6.092 2.25e-08 ***
x             0.4134     0.2815   1.468    0.145    

Residual standard error: 2.753 on 97 degrees of freedom
Multiple R-squared:  0.02175,	Adjusted R-squared:  0.01166 
F-statistic: 2.156 on 1 and 97 DF,  p-value: 0.1452

>> summary(fitted_lm2)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.11031    0.11578   0.953    0.343    
x            0.95718    0.09820   9.747  5.2e-16 ***
x.2         -1.89764    0.06996 -27.126  < 2e-16 ***

Residual standard error: 0.9403 on 96 degrees of freedom
Multiple R-squared:  0.8871,	Adjusted R-squared:  0.8847 
F-statistic: 377.1 on 2 and 96 DF,  p-value: < 2.2e-16

>> summary(fitted_lm3)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.10423    0.11986   0.870    0.387    
x            0.98637    0.16976   5.810 8.28e-08 ***
x.2         -1.88991    0.07926 -23.843  < 2e-16 ***
x.3         -0.01076    0.05089  -0.211    0.833    
---

Residual standard error: 0.945 on 95 degrees of freedom
Multiple R-squared:  0.8872,	Adjusted R-squared:  0.8836 
F-statistic: 248.9 on 3 and 95 DF,  p-value: < 2.2e-16

>> summary(fitted_lm4)

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.082862   0.143013   0.579    0.564    
x            0.960897   0.193772   4.959 3.14e-06 ***
x.2         -1.832481   0.221965  -8.256 9.26e-13 ***
x.3          0.002669   0.070438   0.038    0.970    
x.4         -0.013051   0.047086  -0.277    0.782    
---

Residual standard error: 0.9496 on 94 degrees of freedom
Multiple R-squared:  0.8872,	Adjusted R-squared:  0.8824 
F-statistic: 184.9 on 4 and 94 DF,  p-value: < 2.2e-16

"""

"""

A large F-statistic indicates a strong relationship between the independent variables and the dependent variable, 
while a small F-statistic indicates a weak relationship.
So the higher F-statistic the better model. 
In the summerys we can see that the F-statistic for the second model is the highest.
That makes sense since we know that our data has the quadratic trend and the second model is quadratic too!

The coefficient estimates for the last three models are very alike, 
in compare to the first model that we have talked about it in a previous part
which confirms the earlier cross-validation results.
Therefore we again underestand that the first model is not appropriate for our data.

In last three models we can see that the first two predictors are statistical significance. (They are shown as *** in the summary output)
Which again make sense. The worst model (the first model) do not have these *** for predictors since it is not fit the model well!

So The results agree with Cross Validation results and make sence due to our data.
"""