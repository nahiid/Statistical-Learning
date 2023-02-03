#_________________PART A________________________
#_______________________________________________
# Goal: Creating a function that predicts outcomes using the Nearest Neighbor and Parzen Window methods.
# inputs of the function[
#  yval: dependent variable for each data point
#  xval: independent variable for each data point
#  xpred: vector of independent variable values at which the forecast is made 
#  WindowSize: Parzen window's size]
# output of the function[
#  NNpreds: vector of predictions for each entry of the vector xpred]

PredictNearestNeighborsParzen1D = function(yval, xval, xpred, WindowSize)
{

    # First we should find a number of predictors to create output vector with the same size.
    num_xpred = length(xpred)
    NNpreds = rep(0,num_xpred)

    # the mean of the yval values corresponding to the nearest neighbors is computed and stored in the k-th position 
    # of the NNpreds vector. This process is repeated for each prediction in the xpred vector. 
    # The function returns the final NNpreds vector as its output.
    for(k in 1:num_xpred)
    {
        NN_index = which(abs(xpred[k]-xval) <= WindowSize)
        NNpreds[k] = mean(yval[NN_index])
    }
    return(NNpreds)
}

#_________________PART B________________________
#_______________________________________________
# Goal: Setting the simulated data and model.

set.seed(2022)

# Number of  observations insample
nIN = 200
# Number of  observations out-of-sample
nOOS = 200 

# min x value
x_min =-2 
# max x value
x_max = 2 
# insample independent variables
xvalIS = (x_max-x_min)*runif(nIN) + x_min
# out of sample independat variables
xvalOOS = (x_max-x_min)*runif(nOOS) + x_min

# true pattern for insample observations
fIS =  0.05 * xvalIS^5 - 3 * xvalIS^2 
# true pattern for out-of-sample observations
fOOS =  0.05 * xvalOOS^5 - 3 * xvalOOS^2 
# noisy in sample observations
yobsIS = fIS + rnorm(nIN) 
# noisy out-of-sample observations
yobsOOS = fOOS + rnorm(nOOS) 

#_________________PART C________________________
#_______________________________________________
# Goal: 1.Parzen window approach of to estimate the responses associated with a uniform grid of predictors 
# from −2 to 2 with step size 0.01. 
# 2.In one scattered plot of in-sample data and the predictions associated with the grid of predictors for D = 0.1 and D = 0.5.

plot(xvalIS,yobsIS,main='Parzen Window for Nearest Neighbors Prediction',xlab = 'x', ylab = 'y')

# Generating x of predictions
xpred <- seq(from=x_min, to=x_max, by = 0.01)

NNpreds <- PredictNearestNeighborsParzen1D(yval=yobsIS, xval=xvalIS, xpred=xpred, WindowSize=0.1)
lines(xpred, NNpreds, col='#f4314f')
NNpreds <- PredictNearestNeighborsParzen1D(yval=yobsIS, xval=xvalIS, xpred=xpred, WindowSize=0.5)
lines(xpred, NNpreds, col='#3ad6fd')

# legend("bottom", legend='blue line for window size 0.5', col='#3ad6fd')
legend(x=-2.1,y=2.09, legend=c('windowsize 0.1',  'windowsize 0.5'), col=c('#f4314f','#3ad6fd'), lty=1, bg='#fee2ff')

#_________________PART D________________________
#_______________________________________________
# Goal: 1.compute the corresponding in-sample MSE and out-of-sample MSE for 
# window sizes from 0.05 to 1, with step size 0.05.
# 2.visualize the corresponding curves in the same plot.

# Creating the vector to store window sizes
# seq() takes three arguments: the starting value (0.05), the end value (1), and the increment value (0.05)
WindowSize_vector = seq(0.05, 1, 0.05)

# Numbers of window sizes
num_WindowSize = length(WindowSize_vector)

# numeric() creates an empty numeric vector of the specified length
# In sample MSE empty vector
MSE_IS = numeric(num_WindowSize)
# out of sample MSE empty vector
MSE_OS = numeric(num_WindowSize)


for(i in 1:num_WindowSize) 
{ 
    WindowSize = WindowSize_vector[i]

    # Prediction using the defined function Nearest Neighbours with Parzen window
    NNpreds_IS = PredictNearestNeighborsParzen1D(yval=yobsIS,xval=xvalIS,xpred=xvalIS,WindowSize=WindowSize)
    NNpreds_OS = PredictNearestNeighborsParzen1D(yval=yobsIS,xval=xvalIS,xpred=xvalOOS,WindowSize=WindowSize)

    # Calculating In Sample MSE
    MSE_IS[i] = mean( (NNpreds_IS - yobsIS)^2 )
    # Calculating Out of Sample MSE
    MSE_OS[i] = mean( (NNpreds_OS - yobsOOS)^2 )
}


# Ignore these below lines from 113 to 135 since it fails and IDK why I got error for that!
# plot(WindowSize_vector, 
#     # cbind() binds the two vectors into a single matrix that can be plotted. 
#     cbind(MSE_IS, MSE_OS), 
#     main='Training vs Validation MSE',
#     ylim = c(0,5), 
#     xlab='Window Size',
#     ylab='MSE', 
#     # The value "n" for type creates a plot without any plotted data, 
#     # which allows us to add data to the plot using other functions, such as 'lines'.
#     type='n', 
#     col=c("#ff2bd5", "#ffb52d"), 
#     # pch symbols for the plotted points.
#     pch=1,
#     # lty used to specify line types for the plotted points.
#     lty=1,
#     # line width
#     lwd=2,
#     legend = c("MSE_IS", "MSE_OS"),
#     # box type
#     bty="l",
#     # plot type
#     pty="o")

# Comparing In sample MSE with Out Of Sample MSE 
# Visualizing them in the same plot
plot(WindowSize_vector, MSE_IS, main = 'In sample MSE and Out of Sample MSE', ylim = c(0,5), xlab = 'Window Size', ylab = 'MSE', type = 'n')
lines(WindowSize_vector, MSE_IS, col = '#00ae14', type = "p", pch = 1)
lines(WindowSize_vector, MSE_OS, col = '#fc34ff', type = "p", pch = 1)