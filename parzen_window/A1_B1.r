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
