#' ---
#' title: "Demo: Statistical Tests for Categorical Data (Part II)"
#' author: "Eleni-Rosalina Andrinopoulou, Department of Biostatistics, Erasmus Medical Center"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     df_print: paged
#' ---
#' 



#' ## Two dependent sample test
#' We first create data.
#' In particular we create two categorical vectors with two categories:
set.seed(123)
x <- rbinom(n = 60, size = 1, prob = 0.3)
y <- rbinom(n = 60, size = 1, prob = 0.6)
mat <- table(x, y)
dimnames(mat) <- list(before = c("yes","no"),
                      after = c("yes","no"))
mat
#' Let's assume that we have paired observations.
#' Null hypothesis: the probability of before = no and after = yes is 
#' equal to the probability of before = yes and after = no.
#' We can use the McNemar test for paired data:
mcnemar.test(x = mat)
#' Alternatively:
mcnemar.test(x = x, y = y)


#' By default the function assumes continuity correction. We can use the argument
#' `correct = FALSE` to change that:
mcnemar.test(x = mat, correct = FALSE)


#' ## Multiple testing
#' Performing many comparisons within one experiment increases the likelihood 
#' of obtaining at least one false-positive result with each additional test. 
#' We need to correct for multiple testing.
#' This can be done using the function `p.adjust()`:
#' Let's assume that we have performed the following tests:
set.seed(123)
x <- rnorm(n = 300, mean = 10, sd = 5)
y <- rnorm(n = 300, mean = 11, sd = 2)
z <- rnorm(n = 300, mean = 15, sd = 3)
w <- rbinom(n = 300, size = 1, prob = 0.6)
u <- rbinom(n = 300, size = 1, prob = 0.4)

test1 <- t.test(x = x, y = y)
test2 <- t.test(x = x, y = z)
test3 <- t.test(x = y, y = z)
test4 <- mcnemar.test(x = w, y = u)


#' The corresponding p-values are then:
test1$p.value
test2$p.value
test3$p.value
test4$p.value


# Adjustment using the Bonferroni method:
p_adj_Bonf <- p.adjust(p = c(test1$p.value, test2$p.value, test3$p.value,
                         test4$p.value), method = 'bonferroni')
p_adj_Bonf

# Adjustment using the Holm method
p_adj_Holm <- p.adjust(p = c(test1$p.value, test2$p.value, test3$p.value,
                         test4$p.value), method = "holm")
p_adj_Holm
