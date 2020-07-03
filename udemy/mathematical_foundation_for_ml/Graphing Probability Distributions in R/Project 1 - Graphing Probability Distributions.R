# Graphing Probability Distributions in R

# Example: Plotting the normal distribution of children's IQ scores - mean of 100 and standard deviation of 15
# What percentage of children have an IQ score greater than 115? 

# Define Parameters of Distribution
mean = 100
standev = 15

# Define lower and upper bounds of region of interest
lower = 115
upper = Inf

# Generate sequence of numbers and make normal distribution
x <- seq(-4, 4, length=100)*standev + mean
prob <- dnorm(x, mean, standev)

# Make plot and add probability distribution and axis labels
plot(x, prob, type="n", xlab = "IQ Values", ylab = "P(x)", main = "Normal Distribution of IQ Scores", axes = FALSE)
lines(x, prob)
axis(1, at=seq(40, 160, 20), pos=0)


# Generate polygon for region of interest
i <- x >= lower & x <= upper
polygon(c(lower,x[i],upper), c(0,prob[i],0), col="red") 

# Calculate are under curve for region of interest and print results
area <- 1 - pnorm(lower, mean, standev)
result <- paste("P(",lower,"< IQ <",upper,") =", signif(area, digits=3))
mtext(result,3)
