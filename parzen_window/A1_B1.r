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

PredictNearestNeighborsParzen1D = function(yval,xval,xpred,WindowSize)
{

    # First we should find a number of predictors to create output vector with the same size.
    num_xpred = length(xpred)
    NNpreds = rep(0,num_xpred)

    for(k in 1:num_xpred)
    {
        NN_index = which(abs(xpred[k]-xval) <= WindowSize)
        NNpreds[k] = mean(yval[NN_index])
    }
    return(NNpreds)
}

#_________________PART B________________________
#_______________________________________________
set.seed(2022)