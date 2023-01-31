library(ISLR2)
library(ggplot2)
library(GGally)

#_________________PART A________________________
#_______________________________________________
# Goal: Find Scatterplot of all variables

# Method 1: Use Base R
plot(Auto, pch=20, cex=1.5, col='steelblue')

# Method 2: Use ggplot2 and GGally packages
ggpairs(Auto, cardinality_threshold = 304)

# Method 3: Base R
pairs(Auto);

#_________________PART B_______________________
#______________________________________________
# Goal: Find Correlation between numerical columns

# Method 1: Find cor() for numeric variables
cor(Auto[sapply(Auto, is.numeric)])

# Method 2 : Find and exclude the qualitative column and then use cor()
index <- which(colnames(Auto) == 'name')
modified_auto <- Auto[ , -index ]
cor(modified_auto)

#_________________PART C_______________________
#______________________________________________
# Goal: With lm() perform a multiple linear regression 
# with mpg as the response and all other variables except name as
# the predictors.