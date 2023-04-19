#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B1 ooooooooooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm



#Final Goal: PCA principal components analysis implementation 

###PCA MY OWN CODE

x= cbind(USArrests$Murder, USArrests$Assault, USArrests$UrbanPop, USArrests$Rape) #data matrix
xscaled = scale(x, center = TRUE, scale = TRUE) # scaled data

covmat = cov(xscaled) #covariance matrix

#_________________PART A________________________
#_______________________________________________
# Goal: Find Loading

library(ISLR2)

#load data
data_set = USArrests 
#scale data
scaled_data = scale(data_set, center = TRUE, scale = TRUE)
covariance = cov(scaled_data)

loadings = eigen(covariance)$vectors
loadings

