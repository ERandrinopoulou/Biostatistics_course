#' ---
#' title: "Demo: Statistical Tests for Categorical Data (Part I)"
#' author: "Eleni-Rosalina Andrinopoulou, Department of Biostatistics, Erasmus Medical Center"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     df_print: paged
#' ---
#' 



#' ## One sample test
#' We first create data.
#' In particular we create a categorical vector with two categories (yes/no):
set.seed(123)
x <- rbinom(n = 60, size = 1, prob = 0.3)

#' Null hypothesis: the probability of `x` being yes is equal to 0.4.
#' We have a **large sample size** so we can use the $z$-test for proportions. 
#' In R we can use the `prop.test()` function for a proportion test. 
#' The data are provided as the number of successes `x` and the number of 
#' trials `n`:
prop.test(x = sum(x), n = length(x), p = 0.4)
#' Alternatively we can create a matrix with the successes and failures:
mat <- matrix(data = c(sum(x), sum(1-x)), nrow = 1, ncol = 2)
#' The following code provides us with the same results:
prop.test(mat, p = 0.4)

#' Note that, by default, the function `prop.test()` uses the Yates 
#' continuity correction. To not use this correction we can set the 
#' argument `correct = FALSE`:
prop.test(x = sum(x), n = length(x), p = 0.4, correct = FALSE)

#' If we want to test whether the proportion is less than 0.4 
#' (one-tailed test), we can change the argument `alternative`:
prop.test(sum(x), n = length(x), p = 0.4, correct = FALSE,
          alternative = "less")
#' Or, if we want to test whether the proportion 
#' is greater than 0.4 (one-tailed test), use this:
prop.test(sum(x), n = length(x), p = 0.4, correct = FALSE,
          alternative = "greater")


#' Using the formula $z = \frac{p-\pi_0}{\sqrt{\frac{\pi_0(1-\pi_0)}{n}}}$ 
#' we can calculate the test statistic:
test_res <- (sum(x)/length(x) - 0.4) / (sqrt((0.4*0.6)/(length(x))))
#' The p-value for a two-tailed test will then be:
pVal <- 2 * pnorm(test_res, lower.tail = TRUE)
pVal

#' Important note: The `prop.test()` function does not do a $z$-test. 
#' It does a Chi-square test, using one categorical 
#' variable with two states (success and failure).\
#' **Connection between $\chi^2$ and $z$ distribution**\
#' If $X$ is an independent random variable from the standard $z$ distribution, 
#' then the random variable $X^2$ follows the $\chi^2$ distribution:
test_res^2 
prop.test(x = sum(x), n = length(x), p = 0.4, correct = FALSE)$statistic



#' Let's now assume that we only have 20 subjects (**small sample size**).
#' We first create the data:
set.seed(123)
x <- rbinom(n = 20, size = 1, prob = 0.3)

#' Null hypothesis: the probability of `x` being yes is equal to 0.4.
#' We have a **small sample size** so we can use the exact binomial test:
binom.test(x = sum(x), n = length(x), p = 0.4)


#' ### The $\chi^2$ goodness-of-fit test
#' We first create data.
#' In particular we create a categorical vector with two categories:
set.seed(123)
x <- rbinom(n = 60, size = 1, prob = 0.3)
#' Null hypothesis: the two categories are chosen with equal probability
chisq.test(x = c(sum(x), sum(1-x)))
chisq.test(x = as.table(c(sum(x), sum(1-x)))) 

#' Null hypothesis: the two categories have probabilities equal to 0.4 and 0.6
#' respectively:
chisq.test(x = c(sum(x), sum(1-x)), p = c(0.4, 0.6))



#' ## Two sample test
#' We first create the data:
set.seed(123)
x <- rbinom(n = 60, size = 1, prob = 0.3)
y <- rbinom(n = 60, size = 1, prob = 0.6)

#' Null hypothesis: the probability of successes in `x` is equal to 
#' the probability of successes of `y`.
#' We have a **large sample size** so we can use the $z$-test for 
#' proportions:
prop.test(x = c(sum(x), sum(y)), n = c(length(x), length(y)))



#' ## Test of independence for two variables
#' We first create the data:
set.seed(123)
x <- rbinom(n = 60, size = 1, prob = 0.5)
y <- rbinom(n = 60, size = 1, prob = 0.3)
mat <- table(x, y)
dimnames(mat) <- list(gender = c("F", "M"),
                      treatment = c("yes","no"))
mat

#' Null hypothesis: there is no association between 
#' `gender` and `treatment`.
#' We use the $\chi^2$ test: 
chisq.test(mat)
#' Note that here have two categories per variable, but more categories can
#' be assumed.

#'
#' Let's now assume that we only have 20 subjects (**small sample size**).
#' We first create the data:
set.seed(123)
x <- rbinom(n = 20, size = 1, prob = 0.3)
y <- rbinom(n = 20, size = 1, prob = 0.6)
mat <- table(x, y)
dimnames(mat) <- list(gender = c("F", "M"),
                      treatment = c("yes","no"))
mat

#' Null hypothesis: there is no association between 
#' `gender` and `treatment`. In this case we use the Fisher's exact test:
fisher.test(mat)









