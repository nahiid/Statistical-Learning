


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
# Create a data frame with all predictors in all models.
data_frame = data.frame(y,x,x^2,x^3,x^4)

# We have to store all of the errors in the matrix then we could get the average of them.
# To initialize we plug zero as the elements of the matrix. but it could be any numbers.
# o_error stands for out of sample error
o_error = matrix(0,nrow = n,ncol = 4)

for (i in 1:n){
    # we should exclude the ith row means the ith input out of the data and use it as trained data.
    train_data = data[-i,]

    # our linear regression models are fit using the lm function with the traindata data frame, 
    # with the response variable y and different predictor variables 
    # x, x + x^2, x + x^2 + x^3, and x + x^2 + x^3 + x^4 respectively.

    # Fit the data with the model : Y = β0 + β1X + ϵ
    fitted_lm1 =lm(y ~ x, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + ϵ
    fitted_lm2 =lm(y ~ x + x^2, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + β3X3 + ϵ
    fitted_lm3 =lm(y ~ x + x^2 + x^3, data=train_data)
    # Fit the data with the model : Y = β0 + β1X + β2X2 + β3X3 + β4X4 + ϵ
    fitted_lm4 =lm(y ~ x + x^2 + x^3 + x^4, data=train_data)

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