#' ---
#' title: "Demo: Introduction to R - Distribution Functions"
#' author: "Eleni-Rosalina Andrinopoulou, Department of Biostatistics, Erasmus Medical Center"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     df_print: paged
#' ---
#' 


#' # Common probability distributions 

#' - Normal
#' - $t$
#' - $\chi^2$
#' - $F$
#' - Binomial	

#'
#' # Key functions to describe distributions

#' - the probability density function always begins with 'd' (p.d.f.)
#' - the cumulative distribution function always begins with 'p' (c.d.f.)
#' - the inverse cumulative distribution (or quantile function) always beings with 'q' 
#' - a function that produces random variables always begins with 'r'
#' 
#' ## Normal distribution ($z$-distribution)

#' Plotting a graph with the value of the variable in the horizontal axis and 
#' the count of the values in the vertical axis leads to a bell shape curve.

#' We will use the standard normal distribution, given by $\mu = 0$ and 
#' $\sigma = 1$.


# Create a sequence of numbers between -6 and 6 incrementing by 0.1.
x <- seq(from = -6, to = 6, by = 0.1)


# p.d.f.
y <- dnorm(x = x, mean = 0, sd = 1)
# This function returns the density of the standard normal distribution 
# (mean = 0 and sd = 1).

# Let's try other mean and sd.
w <- dnorm(x = x, mean = -1, sd = 1)
z <- dnorm(x = x, mean = 1, sd = 0.6)
# This function returns the density of the standard normal distribution 
# for a given mean and standard deviation (p.d.f.).

# Plot
plot(x = x, y = y, type = "l", xlim = c(-6,6), ylim = c(0, 0.7))
lines(x = x, y = z, col = "red")
lines(x = x, y = w, col = "blue")


# c.d.f.
y <- pnorm(q = x)
# This function returns the cumulative probability of the standard normal 
# distribution. 
# That is the area under the standard normal curve to the left of x.
# E.g. for x equal to 1
x <- seq(from = -6, to = 6, by = 0.1)
y <- dnorm(x = x, mean = 0, sd = 1) # Z score
plot(x = x, y = y, type = "l", xlim = c(-6,6))
polygon(x = c(x[x <= 1], 1), y = c(y[x <= 1], 0), col = "blue")
# Then:
pnorm(q = 1)
# This means that the probability of getting a value smaller or equal 
# than 1 is 0.84 or 84%.


# If now we want Pr(X > 1) = 1 - Pr(X <= 1)
1 - pnorm(q = 1)
# This means that the probability of getting a value larger than 1 is 0.16 or 16%.

# We can also use the argument lower.tail = FALSE to obtain the same result.
pnorm(q = 1, lower.tail = FALSE)


# Inverse cumulative distribution
qnorm(p = 0.84)
# This function is the inverse of the `pnorm()` function. For example, the X 
# score corresponding to the 84th percentile is 1.


#  Random variables
# Create a sample of 50 numbers which are normally distributed with 
# mean 10 and sd 0.2.
y <- rnorm(n = 50, mean = 10, sd = 0.2)
# This function is used to generate random numbers whose distribution is normal. 
hist(x = y, main = "Normal Distribution")

# In many applications, we want to be able to reproduce the results of 
# our data analysis. It is then important to use the set.seed(q) function 
# to generate the same set of random numbers. Here `q` is an integer. 
set.seed(123)
rnorm(n = 5)
# If we run the above code multiple times, we get the same numbers.


# Let's generate 10000 random values from a normal distribution with 
# mean 0 and sd = 1. 
set.seed(123)
x <- rnorm(n = 10000)
mean(x)
sd(x)
# We see that the mean and sd are very close to 0 and 1 as expected.
# To verify that the data is normally distributed, we generate a histogram 
# using the hist().
hist(x = x)

hist(x = x, freq = FALSE)
# The freq = FALSE argument is to obtain the density (fraction of points in
# each interval) on the y-axis instead of frequency (number of points in each 
# interval). With this option, the total area of the histogram is normalized to 1.
curve(expr = dnorm(x), col = "blue", add = TRUE)
# The function `curve()` draws a curve corresponding to a function.



#' ## t-distribution

#' - Like the normal distribution ($z$-distribution), the t-distribution is 
#' also symmetric.\
#' - The $t$-distribution depends on the degrees of freedom. The curves with more 
#' degrees of freedom are taller and have thinner tails. 
#' These are related to the sample size.\
#' - The $t$-distribution is most useful for small sample sizes, when the 
#' population standard deviation is not known, or both.\
#' - As the sample size increases, the $t$-distribution becomes more similar 
#' to a normal distribution. \
#'
#' **Degrees of freedom**\

#' Degrees of freedom of an estimate is the number of values that have the 
#' freedom to vary in the data sample. It is not quite the same as the number 
#' of items in the sample. For example we are interested in the mean BMI of 
#' our students. Let's assume that we have 50 students. If we also have the 
#' mean value, this means that 49 BMI values can vary and 1 has to be fixed.
#' If we have $X_1$, $\dots$, $X_n$, where $n$ is the number of subjects and 
#' we know the mean value $\bar{x} = \frac{(X_1 + \dots + X_n)}{n}$, 
#' then $X_1$, $\dots$, $X_{n-1}$ can vary but $X_n$ has to be fixed. E.g., 
#' suppose we collect two random samples of observations shown below. 
#' Let's assume that we know the mean but we don't know the value of an 
#' observation ($X$ and $Y$). All other values can vary except $X$ and $Y$ in
#' order to get those means. $X$ should be $20$ and $Y$ should be $26$.
# =========== ========= =========
#              Sample1   Sample2
# =========== ========= =========
#  Subject1      18        19
#  Subject2      15        28
#  Subject3      21        22
#  Subject4      30        18
#  Subject5      22        19
#  Subject6       X         Y   
# =========== ========= =========
#  Average       21        22


# Generate a random sample from the standard normal distribution.
x <- rnorm(n = 10000)


# p.d.f.
# For this distribution we need to define the degrees of freedom.
curve(expr = dt(x = x, df = 1), xlim = c(-3, 3), ylim = c(0, 0.45), 
      ylab = "Chi Square Density")
curve(expr = dt(x = x, df = 2), col = "red",lty = 2, add = TRUE)
curve(expr = dt(x = x, df = 3), col = "blue",lty = 3, add = TRUE)
curve(expr = dt(x = x, df = 5), col = "green",lty = 4, add = TRUE)
curve(expr = dt(x = x, df = 10), col = "brown",lty = 5, add = TRUE)
legend(2, 0.38, c("k=1", "k=2", "k=3", "k=5", "k=10"),
       col = c("black", "red", "blue", "green", "brown"), lty = 1:5)


# Difference between normal and $t$ distribution
hist(x, freq = FALSE, breaks = 200)
curve(expr = dnorm(x), col = "red", lwd = 2, add = TRUE)
curve(expr = dt(x = x, df = 1), col = "blue", lwd = 2, add = TRUE)
legend(2, 0.38, c("Normal", "t"),
       col = c("red", "blue"), lty = c(1,1))


# c.d.f.
# The `pt()` function returns the cumulative probability of the $t$ distribution. 
# E.g. Pr(X <= 1):
pt(q = 1, df = 1)
# This means that the probability of getting a value smaller or equal than 
# 1 is 0.75 or 75%.

# If now we want Pr(X > 1) = 1 - Pr(X <= 1)
1 - pt(q = 1, df = 1)
# This means that the probability of getting a value larger than 1 is 0.25 or 25%.

# We can also use the argument lower.tail = FALSE to obtain the same result.
pt(q = 1, df = 1, lower.tail = FALSE)


# Inverse cumulative distribution
qt(p = 0.75, df = 1)
# This function is the inverse of the `pt()` function. 
# For example, the value corresponding to the 75th percentile is 1.

# Random variables
set.seed(123)
x <- rt(n = 10000, df = 3)



#' ## $\chi^2$ distribution

#' A $\chi^2$ distribution with $k$ degrees of freedom is constructed by the sum 
#' of the square of $k$ independent standard normal random variables. 
#' If $X_1, X_2, \dots, X_k$ are independent random variables from the 
#' standard normal distribution, then the random variable
#' $Q = X^2_1+X^2_2+ \dots +X^2_k$ follows the $\chi^2$ distribution with $k$ 
#' degrees of freedom. 


# Generate a random sample from the standard normal distribution.
x <- rnorm(n = 10000)


# p.d.f.
# For this distribution we need to define the degrees of freedom.
curve(expr = dchisq(x = x, df = 1), xlim = c(0, 15), ylim = c(0, 0.6), 
      ylab = "Chi Square Density")
curve(expr = dchisq(x = x, df = 2), col = "red",lty = 2, add = TRUE)
curve(expr = dchisq(x = x, df = 3), col = "blue",lty = 3, add = TRUE)
curve(expr = dchisq(x = x, df = 5), col = "green",lty = 4, add = TRUE)
curve(expr = dchisq(x = x, df = 10), col = "brown",lty = 5, add = TRUE)
legend(12, 0.55, c("k=1", "k=2", "k=3", "k=5", "k=10"),
       col=c("black", "red", "blue", "green", "brown"), lty = 1:5)


# If we now plot x^2, were x is a random variable from the standard normal 
# distribution we get:
hist(x = x^2, freq = FALSE, breaks = 200, xlim = c(0,5))
# Does it look familiar?
curve(expr = dchisq(x = x, df = 1), add = TRUE)
# x^2  follows the \chi^2 distribution with one degree of freedom. 


# Let's assume more variables.
X1 <- rnorm(n = 10000)^2 
X2 <- rnorm(n = 10000)^2  
X3 <- rnorm(n = 10000)^2 
Q <- X1 + X2 + X3

hist(Q, freq = FALSE, breaks = 100)
curve(expr = dchisq(x = x, df = 3), col = "red", add = TRUE)


# Random variables
x <- rchisq(n = 10000, df = 3)


# c.d.f.
# Same as before:
x <- seq(from = 0, to = 10, by = 0.1)
y <- dchisq(x = x, df = 3) 
plot(x, y, type = "l")
polygon(c(x[x <= 8], 8), c(y[x <= 8], 0), col = "blue")
# The Pr(x^2 <= 8) is
pchisq(q = 8, df = 3)

plot(x, y, type = "l")
polygon(c(8, x[x >= 8], max(x)), c(0, y[x >= 8], 0), col = "blue")
# The Pr(x^2 > 8) is
pchisq(q = 8, df = 3, lower.tail = FALSE)


# Inverse cumulative distribution
qchisq(p = 0.95, df = 3)
# This function is the inverse of the pchisq() function. 
# For example, the value corresponding to the 95th percentile is 8.



#' ## $F$ distribution
#' If two independent random variables follow the $\chi^2$-distribution 
#' with m1 and m2 degrees of freedom respectively, then the ratio each divided 
#' by its degrees of freedom follows an $F$ distribution.
#' This distribution is used whenever you need to compare two 
#' $\chi^2$-distributions (compare two different sum of squares).
#' 
# Generate a sequence of values.
x <- seq(from = 0, to = 20, by = 0.1) 


# p.d.f.
plot(df(x = x, df1 = 3, df2 = 5), type = "l", ylab = "F Density")
plot(df(x = x, df1 = 10, df2 = 20), type = "l", ylab = "F Density")

# Generate chi square data with df = 3.
chi.sq.1 <- rchisq(10000, 3)
# Scale the chi square variable.
scaled.chi.sq.1 <- chi.sq.1 / 3
# Generate chi square data with df = 20.
chi.sq.2 <- rchisq(10000, 20)  
# Scale the chi square variable.
scaled.chi.sq.2 <- chi.sq.2 / 20  
# Take the ratio of the two chi squares (F).
F_3_20 <-  scaled.chi.sq.1  / scaled.chi.sq.2 
# Plot
hist(F_3_20, freq = FALSE) 
curve(expr = df(x = x, df1 = 3, df2 = 20), col = "red", add = TRUE)



#' ## Binomial

#' The binomial distribution model deals with finding the probability of 
#' success of an event which has only two possible outcomes in a series of 
#' experiments. For example, tossing a coin always gives a head or a tail. 
#' The probability of finding exactly 3 heads in tossing a coin repeatedly 
#' for 10 times is estimated using the binomial distribution.\
#' Note: A single success/failure experiment is also called a Bernoulli trial or
#' a Bernoulli experiment.

# Create a sample of 50 numbers ranging from 0 to 50 which are incremented by 1.
x <- seq(from = 0, to = 50, by = 1)


# p.d.f.
# Here we need to define the number of trials (size) and the probability of 
# success on each trial (prob). 
y <- dbinom(x, size = 50, prob = 0.5, log = FALSE)


# c.d.f.
y <- pbinom(q = x, size = 50, prob = 0.5)
# This function returns the cumulative probability function of the 
# binomial distribution.

# Assume a coin is weighted so that it comes up heads 60% of the time. 
# What is the probability that you will obtain more than 25 heads after 50 flips?
# Pr(X > 25) = 1 - Pr(X <= 25)
1 - pbinom(q = 25, size = 50, prob = 0.6)
# other way to obtain the same results
1 - sum(dbinom(x = (1:25), size = 50, prob = 0.6))
# Alternatively,
pbinom(q = 25,  size = 50, prob = 0.6, lower.tail = FALSE)

# Assume a die is rolled 10 times. What it the probability that you will roll 
# fewer than 5 sixes?
# Pr(X < 5) = Pr(X <= 4)
pbinom(q = 4, size = 10, prob = 0.16)

# Assume a die is rolled 100 times. 
# What it the probability that you will roll more than 5 sixes?
# Pr(X > 5) = 1 - Pr(X <= 5)
1 - pbinom(q = 5, size = 100, prob = 0.16, lower.tail = TRUE)
pbinom(q = 5, size = 100, prob = 0.16, lower.tail = FALSE)

# Random variables
x <- rbinom(n = 20, size = 1, prob = 0.3)
print(x)

