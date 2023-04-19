#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B1 ooooooooooooooooooooo
#oooooooooooooooooo Assignment 3 ooooooooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

set.seed(2022)  # set the seed for reproducibility of random numbers

library(mvtnorm)  # load the "mvtnorm" package for multivariate normal distribution
library(MASS)  # load the "MASS" package for some statistical functions

obs = 150  # set the number of observations

p_orange = 0.6  # set the probability of a data point belonging to the "orange" class
p_blue = 1 - p_orange  # calculate the probability of a data point belonging to the "blue" class

avg_orange = c(1,1)  # set the mean vector for the "orange" class
avg_blue = c(1,0)  # set the mean vector for the "blue" class

cov_orange = matrix( c(1,0.2,0.2,1) , nrow = 2, ncol = 2)  # set the covariance matrix for the "orange" class
cov_blue = matrix( c(0.7,0.3,0.3,0.7) , nrow = 2, ncol = 2)  # set the covariance matrix for the "blue" class

obs_orange = rbinom(n=1,size=obs, prob = p_orange)  # randomly generate the number of observations in the "orange" class
obs_blue = obs - obs_orange  # calculate the number of observations in the "blue" class

ObsOrange = mvrnorm(n = obs_orange, mu = avg_orange, Sigma = cov_orange)  # generate the data for the "orange" class
ObsBlue = mvrnorm(n = obs_blue, mu = avg_blue, Sigma = cov_blue)  # generate the data for the "blue" class

x_points = rbind(ObsOrange,ObsBlue)  # combine the data points from the "orange" and "blue" classes
y_points = c(rep(1,obs_orange),rep(2,obs_blue))  # create a vector of class labels for each data point

plot(ObsOrange[,1],ObsOrange[,2],main='Classification dataset',
     xlab = expression('X'[1]), ylab = expression('X'[2]),type='n', xlim = c(-2,3), ylim = c(-3,4))  # plot the data points from the "orange" class
points(ObsOrange[,1],ObsOrange[,2],col='#ff7b00',pch=1)  # add the "orange" data points to the plot
points(ObsBlue[,1],ObsBlue[,2],col='#1515a4')  # add the "blue" data points to the plot
legend(x=1, y=-1.5, legend=c("Class 1",  "Class 2"), col=c("#ff7b00","#1515a4"), pch=c(1,1) )  # add a legend to the plot

x_grid <- seq(from = -2, to = 4, by = 0.1)  # create a grid of x-values for contour plots
y_grid <- seq(from = -4, to = 5, by = 0.1)  # create a grid of y-values for contour plots
grid <- expand.grid(x_grid, y_grid)  # create a data frame of all combinations of x and y values

# Convert 'grid' to a numeric matrix
matrix_grid <- as.matrix(sapply(grid, as.numeric)) 

# Set the prior probabilities for the two classes
POrange <- p_orange 
PBlue <- p_blue 

# Calculate the multivariate normal density functions for each class
pdf_Orange <- dmvnorm(x = grid, mean = avg_orange, sigma = cov_orange, log = FALSE)
pdf_Blue <- dmvnorm(x = grid, mean = avg_blue, sigma = cov_blue, log = FALSE)

# Calculate the posterior probabilities using Bayes' theorem
POrangecondX <- pdf_Orange * POrange / (pdf_Orange * POrange + pdf_Blue * PBlue) 

# Calculate the coefficients for the Bayes decision boundary
atilde1 <- -0.5 * solve(cov_orange)
atilde2 <- -0.5 * solve(cov_blue)
atildediff <- atilde1 - atilde2
btilde1 <- matrix(avg_orange, 1, 2) %*% solve(cov_orange)
btilde2 <- matrix(avg_blue, 1, 2) %*% solve(cov_blue)
btildediff <- btilde1 - btilde2
ctilde1 <- log(p_orange) - 0.5 * log(det(cov_orange)) - 0.5 * matrix(avg_orange, 1, 2) %*% solve(cov_orange) %*% matrix(avg_orange, 2, 1)
ctilde2 <- log(p_blue) - 0.5 * log(det(cov_blue)) - 0.5 * matrix(avg_blue, 1, 2) %*% solve(cov_blue) %*% matrix(avg_blue, 2, 1)
ctildediff <- ctilde1 - ctilde2

# Create a uniform grid of step size 0.1 from -5 to 5 for x1 values
x1seqFrontier <- seq(from = -5, to = 5, by = 0.1) 

# Calculate the coefficients for the quadratic equation for x2 values on the Bayes decision boundary
A <- atildediff[2, 2]
B <- (2 * atildediff[2, 1] * x1seqFrontier + btildediff[2])
C <- as.vector(atildediff[1, 1]) * x1seqFrontier^2 + as.vector(btildediff[1]) * x1seqFrontier + as.vector(ctildediff)

# Calculate the x2 values on the Bayes decision boundary
x2Boundtop <- (-B + sqrt(B^2 - 4 * A * C)) / (2 * A)
x2Boundbot <- (-B - sqrt(B^2 - 4 * A * C)) / (2 * A)

# Set the plotting parameters
par(mfcol=c(1,1)) 

# Create scatter plot of observations from the orange and blue classes
# Set the main title and axis labels
# Set the x and y axis limits
plot(ObsOrange[,1],ObsOrange[,2],main='Bayes classifier', xlab = 'X1', ylab = 'X2',type='n', xlim = c(-2,4), ylim = c(-4,5))

# Add points to the plot for orange class regions where P(Orange | X) >= 0.5
# Set the color, symbol, and size of the points
points(grid[POrangecondX>= 0.5,1],grid[POrangecondX>= 0.5,2],col='#ff860e',pch=".",cex=1)

# Add points to the plot for blue class regions where P(Orange | X) < 0.5
# Set the color, symbol, and size of the points
points(grid[POrangecondX< 0.5,1],grid[POrangecondX< 0.5,2],col='#0b0be5',pch=".",cex=1)

# Add points to the plot for observations from the orange class
# Set the color and symbol of the points
points(ObsOrange[,1],ObsOrange[,2],col='#ff860e',pch=1)

# Add points to the plot for observations from the blue class
# Set the color and symbol of the points
points(ObsBlue[,1],ObsBlue[,2],col='#0b0be5',pch=1)

# Add a line to the plot representing the Bayes decision boundary
# Set the color and line width of the line
lines(c(x1seqFrontier,rev(x1seqFrontier)), c(x2Boundtop,rev(x2Boundbot)), col='red',lwd=2)
