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

# We underestood that the 'Origin' is the numerical variable but its concept is not a numerical!
# So we decided to exclude it and then fit the model.
# But it does not effect the quality of the model that much:)
index <- which(names(modified_auto) == 'origin')
modified_auto2 <- modified_auto[ , -index ]
fitted_lm_auto2 <- lm(mpg ~ ., data = modified_auto2)
summary(fitted_lm_auto2)


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

#_________________PART D_______________________
#______________________________________________
# Goal: produce diagnostic plots of the linear regression fit. 
# i. Comment on any problems you see with the fit.
# ii. Do the residual plots suggest any unusually large outliers? 
# iii. Does the leverage plot identify any observations with unusually high leverage?

# Divide the plot window to get a better graph and have all of the plots in one window.
# plot window with 2 rows and columns
layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
plot(fitted_lm_auto2)

#--------------[i]----------------

# By examining the residual plot we can underestand two problems:

# problem 1
# In general, if a linear regression model fits the data well, the residuals should be 
# randomly distributed around zero, with no noticeable pattern or structure. 
# >>In this question with examining the Residual plot, it appears that there is a significant pattern. 
# So linear model may not be the best representation of the underlying relationship in the data.
# problem 2
# Heteroscedasticity violates one of the assumptions of linear regression models, which is that the errors 
# have constant variance. This can lead to biased or misleading results if not properly addressed.
# with examining the Residual plot it appears there errors do no have constant variance.

#--------------[ii]----------------
# The residual plot indicates that the data does not contain any extreme values that 
# deviate significantly from the rest.
# Means that it does not suggest any unusually large outliers. Which makes us happy:)

#--------------[iii]----------------
# Observations with high leverage values are identified as those that lie far from the origin in the plot, 
# and are considered to have the potential to strongly influence the results of a linear regression analysis. 
# The red line in the plot represents the cutoff for high leverage values, and observations above this line 
# are considered to have high leverage.
# By examining the residual plot we can underestand that we have observations that strongly influence 
# the results of a linear regression, but we do not have many large numbers.

#_________________PART E_______________________
#______________________________________________
# Goal: Use the * and : symbols to fit linear regression models with
# interaction effects. Do any interactions appear to be statistically significant?

# We can combine each two with each other.
fitted_lm_auto3 <- lm(mpg ~ cylinders + cylinders : displacement + cylinders : horsepower + 
                     cylinders : weight + cylinders : acceleration + cylinders : year +
                     displacement + displacement : horsepower + displacement : weight +
                     displacement : acceleration + displacement : year +  horsepower + 
                     horsepower : weight + horsepower : acceleration +  horsepower : year +
                     weight + weight : acceleration +  weight : year + acceleration +
                     acceleration : year +  year, data = modified_auto2)

summary(fitted_lm_auto3)
# We can see that some coefficients that have p-value less than 0.05 
# but it is not that effective (these are display with * in the summary)