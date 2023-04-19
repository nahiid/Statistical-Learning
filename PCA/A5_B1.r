#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B1 ooooooooooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

#Final Goal: PCA (principal components analysis implementation) 

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

#_________________PART B________________________
#_______________________________________________
# Goal: Find scores

scores = solve(loadings) %*% t(scaled_data)
scores = t(scores)
scores

