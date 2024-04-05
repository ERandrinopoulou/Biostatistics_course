#' ---
#' title: "Demo: Introduction to R - Basics"
#' author: "Eleni-Rosalina Andrinopoulou, Department of Biostatistics, Erasmus Medical Center"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     df_print: paged
#' ---
#' 

#' ## Load packages 
#' If you are using the package for the first time, you will first have to 
#' install it. \
# install.packages("survival") 
#' If you have already downloaded these packages in the current version of R, 
#' you will only have to load the packages.
library(survival)

#' ## Get and view the data
#' Load a data set from a package.\
#' You can use the double colon symbol (:), to return the pbc object from the 
#' package survival. We store this data set to an object with the name pbc.
pbc <- survival::pbc

#' Print the first 6 rows of the data set using the function `head()`.
head(pbc)

#' View the data set:
View(pbc)

#' If I run "Hello" I get the following:
"Hello"

#' In order to obtain this word again, we will have to retype it. 
#' Alternatively, I can assign the string "Hello" to a new object 
#' named `hi`:
hi <- "Hello"
#' Then we can print this word whenever we type hi.
hi

#' Make sure that you have defined the object before you use it. \
#' E.g. `number` and `x` will not be found since we did not define them. 
#' We can only call them after we have defined them. \
# number
number <- 10
number
# x
x <- 1
x

#' ## Things to remember!
#' * `=` is different from `==`, e.g. `x == 3` is asking a question to R. 
#' The single `=` is equal to <-.\ 
#' * **R** is sensitive, e.g. `pbc$Age` will not run because there is a typo. \
#' We need to check the names first using the function `names()`:
names(pbc)

#' The correct name is age and not Age.
pbc$age

#' ## Data checks
#' Use the function `is.na()` to check if an object consists of missing values:
is.na(x)

#' Use the `is.null()` function to check for Null data:
is.null(pbc$age)

#' Use the `is.infinite()` function to check for infinity data:
is.infinite(pbc$age)
