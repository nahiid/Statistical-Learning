
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
#oooooooooooooooooo Problem B1 ooooooooooooooooooooo
#mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

# Goal: Validate MSELoocv closed formula

set.seed(1)

#Simulate n = 100 observations from the model Y = 2 + 3X + e where X, e are both i.i.d. N(0, 1)
n = 100
x = rnorm(n)
e = rnorm(n)
y = 2 + 3*x + e

# Creating data frame from the generated data
df = data.frame(cbind( y,x))