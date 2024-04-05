#' ---
#' title: "Demo: Statistical Tests for Continuous Data"
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
#' In particular we create a continuous vector:
set.seed(123)
x <- rnorm(n = 300, mean = 10, sd = 5)

#' Null hypothesis: the mean of `x` is equal to 0.
#' We have a **large sample size** so we can use the t-test.
t.test(x = x, mu = 0)
#' If you check the help page you will see that `mu = 0` is the default option.
#' This means that we can remove this part:
t.test(x = x)
#' Output interpretation: The mean sample is 10.11 and the confidence interval is 
#' (9.63, 10.71). The p-value is very small so we reject the null hypothesis
#' that the sample mean is equal to 0. The test statistic is 37.26.
#' The test statistic can be obtained using the formula:
#' $\frac{\bar{x} - \mu_0}{sd(x)\sqrt{n}}$
test_stat <- mean(x)/(sd(x) / sqrt(300))

#'
#' Now let's assume that we want to investigate whether the sample mean is
#' equal to 10:
t.test(x = x, mu = 10)
#' In that case the p-value is too large to reject the null hypothesis.
#' The test statistic can be also obtain as:
test_stat <- (mean(x) - 10)/(sd(x) / sqrt(300))
#' The p-value can be also obtained as:
2 * pt(q = test_stat, df = 300 - 1, lower.tail = FALSE)
2 * (1 - pt(q = test_stat, df = 300 - 1, lower.tail = TRUE))

#' By default, a two-sided test is performed. To do a one-sided test, the 
#' argument `alternative` can be set to less or greater:
t.test(x, mu = 10, alternative = 'less')
t.test(x, mu = 10, alternative = 'greater')

#' Furthermore, we can change the confidence interval level using the
#' argument `conf.level`:
t.test(x, mu = 10, alternative = 'less', conf.level = 0.975)

#' What if we do not want to print the whole output?
#' In that case we can save the test results as an object and then select
#' the parts that we want to print:
test_res <- t.test(x, mu = 10, alternative = 'less', conf.level = 0.975)
test_res$statistic
test_res$p.value
test_res$null.value



#'
#' Let's now assume that we only have 30 subjects (**small sample size**).
#' We first create the data:
set.seed(123)
x <- rnorm(n = 30, mean = 10, sd = 5)

#' Null hypothesis: the median of `x` is equal to 0.
#' We have a **small sample size** so we can use the 
#' Wilcoxon signed rank test:
wilcox.test(x = x, mu = 0)
#' Note that confidence intervals are only returned if `conf.int = TRUE`:
wilcox.test(x = x, mu = 0, conf.int = TRUE)


#' The additional argument `exact` controls if exact p-values and confidence 
#' intervals are calculated or if the normal approximation is used. In the 
#' latter case, the argument `correct` determines if a continuity correction 
#' is applied.
wilcox.test(x = x, mu = 0, exact = TRUE)
wilcox.test(x = x, mu = 0, exact = FALSE)

#' Specific parts of the output can be also extracted:
test_res <- wilcox.test(x = x, mu = 0, exact = TRUE)
test_res$statistic

#' The test statistic $W_-$ and $W_+$ can be also obtained as:
res <- rank(abs(x-0)) # take ranks of the absolute differences
sum(res[(x-0)<0]) # sum all negative differences
sum(res[(x-0)>0]) # sum all positive differences



#' ## Two samples test
#' We first create data.
#' In particular we create two continuous vectors:
set.seed(123)
x <- rnorm(n = 300, mean = 10, sd = 5)
y <- rnorm(n = 300, mean = 11, sd = 2)

#' Null hypothesis: the mean of `x` is equal to the mean of `y`.
#' Let's assume that the samples are **independent**.
#' We have a **large sample size** so we can use the t-test.
t.test(x = x, y = y)

#' It is also possible to specify the test using a formula. This is useful
#' when we have the data in a data.frame:
dat <- data.frame(value = c(x, y), group = rep(x = c(1, 2), each = length(x)))
t.test(value ~ group, data = dat)

#' By default, the test assumes that the two samples have different variances. 
#' Check the help file for all this information!
t.test(x = x, y = y, var.equal = TRUE)

#' F test can be used to check if two samples have the same variance:
var.test(x = x, y = y)

#' Let's now assume that the samples are **dependent**.
#' In that case we need to set the argument `paired = TRUE`:
t.test(x = x, y = y, paired = TRUE)

#' This is equivalent to performing a one-sample t-test
#' of the differences x - y:
t.test(x = x - y)

#' We can change the mean to test if the difference is different from a value 
#' instead of testing for a difference equal to zero. 
t.test(x = x, y = y, mu = 10, paired = TRUE)



#'
#' Let's now assume that we only have 30 subjects (**small sample size**).
#' We first create the data:
set.seed(123)
x <- rnorm(n = 30, mean = 10, sd = 5)
y <- rnorm(n = 30, mean = 11, sd = 2)

#' Null hypothesis: the distribution of `x` is equal to the distribution of `y`.
#' Let's assume that the samples are **independent**.
#' We have a **small sample size** so we can use the 
#' Wilcoxon rank sum test:
wilcox.test(x = x, y = y, correct = TRUE, conf.int = TRUE)
#' Check the help page for the `correct` argument.

#' Let's now assume that the samples are **dependent**.
#' In that case we can use the Wilcoxon singed rank test:
wilcox.test(x = x, y = y, paired = TRUE)



#' 
#' ## M sample test
#' We first create data.
#' In particular we create three continuous vectors:
set.seed(123)
x <- rnorm(n = 300, mean = 10, sd = 5)
y <- rnorm(n = 300, mean = 11, sd = 2)
z <- rnorm(n = 300, mean = 15, sd = 7)

#' Null hypothesis: the means of `x`, `y` and `z` are identical.
#' We have a **large sample size** so we can use the anova test.
dat <- data.frame(value = c(x, y, z), group = rep(x = c(1, 2, 3), each = length(x)))

boxplot(value ~ group, data = dat)
test_res <- aov(formula = value ~ group, data = dat)
summary(test_res)



#'
#' Let's now assume that we only have 30 subjects (**small sample size**).
#' We first create the data:
set.seed(123)
x <- rnorm(n = 30, mean = 10, sd = 5)
y <- rnorm(n = 30, mean = 11, sd = 2)
z <- rnorm(n = 30, mean = 15, sd = 7)
dat <- data.frame(value = c(x, y, z), group = rep(x = c(1, 2, 3), each = length(x)))

#' Null hypothesis: the distributions of `x`, `y` and `z` are identical.
#' We have a **small sample size** so we can use the Kruskal-Wallis test, 
#' which is an extension to the Wilcoxon rank sum test to 
#' more than two groups:
# (all following options will provide the same result)
kruskal.test(x = dat$value, g = dat$group)
kruskal.test(x = list(x, y, z))
kruskal.test(formula = value ~ group, data = dat)


#' ## Correlation test
#' We first create the data:
set.seed(123)
x <- rnorm(n = 300, mean = 10, sd = 5)
y <- rnorm(n = 300, mean = 11, sd = 2)

#' Null hypothesis: the variables `x` and `y` are independent (no correlation).
#' By default, the Pearson correlation is assumed.
cor.test(x = x, y = y)

#' Alternatively, we can obtain the test statistic and p-value using the formula
#' $\frac{\rho \sqrt{n-2}}{\sqrt{1-\rho^2}}$:
test_stat <- (cor(x, y) * sqrt(300 - 2))/sqrt(1 - cor(x,y)^2)
pVal <- 2 * pt(q = test_stat, df = 300 - 2, lower.tail = TRUE)
pVal

#'
#' Let's now assume that we only have 30 subjects (**small sample size**).
#' We first create the data:
set.seed(123)
x <- rnorm(n = 30, mean = 10, sd = 5)
y <- rnorm(n = 30, mean = 11, sd = 2)

#' Null hypothesis: the variables `x` and `y` are independent (no correlation).
#' We have a **small sample size** so we can use the Spearman correlation 
#' by changing the `method` argument:
# (with the `exact` argument we can select whether we want to perform the exact
# test or the approximate test)
cor.test(x = x, y = y, method = "spearman", exact = FALSE)


#' Other correlation coefficients include the Kendall:
cor.test(x = x, y = y, method = "kendall")




