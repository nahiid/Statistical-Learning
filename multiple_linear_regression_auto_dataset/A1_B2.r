library(ISLR)
library(ggplot2)
library(GGally)

# Method 1: Use Base R
plot(Auto, pch=20, cex=1.5, col='steelblue')

# Method 2: Use ggplot2 and GGally packages
ggpairs(Auto, cardinality_threshold = 304)