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
# with mpg as the response and all other variables except name as the predictors.
# i. Is there a relationship between the predictors and the response?
# ii. Which predictors appear to have a statistically significant relationship to the response?
# iii. What does the coefficient for the year variable suggest?

# . means consider all of the variables
# modified auto is the Auto dataset withoun 'name' column defined in line 27
fitted_lm_auto <- lm(mpg ~ ., data = modified_auto)
summary(fitted_lm_auto)

#--------------[i]----------------
# Due to the output of the summary the F-statistic is 252.4 which is high. and this value is measuerd only on 7 predictors.
# and the p-value linked to the F-test is extremely small, less than 2.2e-16. 
# Therefore we can conclude that there is a strong relationship between the predictors and the response
# So we reject null hypothesis.

# Explain of F-statistic
# The F-statistic provides a test of the null hypothesis that all coefficients in the regression model are equal to zero 
# (i.e., that there is no relationship between the predictors and the response variable). 
# A high F-statistic indicates that the model is a good fit to the data

# Explain of p-value
# The p-value assesses the significance of each predictor in explaining the response variation in a regression model.
# A low p-value (typically less than 0.05) indicates that there is strong evidence against the null hypothesis 
# that the corresponding coefficient is equal to zero, and suggests that the predictor is significantly related to the response. 
# i.e a low p-value indicates that the predictor has a non-zero effect on the response. A high p-value (greater than 0.05) suggests 
# that there is not enough evidence to conclude that the predictor is significantly related to the response,
# and that the corresponding coefficient may be equal to zero.

#--------------[ii]----------------
# For finding which predictors has statistically significant relationship to the response 
# we should look for the predictors that have smallest p-value. due to a summery output 
# we underestand 'origin', 'weight', 'year' have the extermly small p-value.
# So these predictors have statistically significant relationship to the response.

#--------------[iii]----------------
# In the summary the coefficient for the year predictor is 0.750773.
