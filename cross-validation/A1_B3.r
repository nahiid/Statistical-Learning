


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