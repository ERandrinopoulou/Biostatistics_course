#' ---
#' title: "Demo: Introduction to R - Data Exploration"
#' author: "Eleni-Rosalina Andrinopoulou, Department of Biostatistics, Erasmus Medical Center"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float:
#'       collapsed: false
#' ---
#' 

#' ## Load packages 
#' If you are using the package for the first time, you will first have to install it. \ 
# install.packages("survival") 
# install.packages("memisc")
#' If you have already downloaded this package in the current version of R, 
#' you will only have to load the package.
library(survival)
library(memisc)

#' ## Get the data
#' Load a data set from a package.\
#' You can use the double colon symbol (:), to return the pbc object from the package survival. 
#' We store this data set to an object with the name pbc.
pbc <- survival::pbc

#' ## Common questions that can be answered in R
#' 
#' ### Continuous data
#' What is the mean and standard deviation for the variable `age` of the pbc data set?
mean(x = pbc$age)
mean(x = pbc$age, na.rm = TRUE)
sd(x = pbc$age)

#' What is the mean and variance for the variable `chol` of the pbc data set?
mean(x = pbc$chol)
mean(x = pbc$chol, na.rm = TRUE)
var(x = pbc$chol, na.rm = TRUE)

#' What is the median and interquartile range for the variable `age` of the pbc data set?
median(x = pbc$age)
IQR(x = pbc$age)

#' What is the min and max of the variable `age` of the pbc data set?
min(x = pbc$age)
max(x = pbc$age)
range(x = pbc$age)

#' What are the 10th, 25th, 50th, 75th and 90th percentiles for 
#' `serum bilirubin` of the pbc data set?
quantile(x = pbc$bili, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

#' The functions `colMeans()` and `rowMeans()` allow us to calculate the mean 
#' for each column or column in a matrix or data.frame, e.g.:
colMeans(x = data.frame(bili = pbc$bili, chol = pbc$chol), na.rm = TRUE)
rowMeans(x = data.frame(bili = pbc$bili, chol = pbc$chol), na.rm = TRUE)

#' The functions `colSums()` and `rowSums()` allow us to calculate the sum for 
#' each column or column in a matrix or data.frame, e.g.:
colSums(x = data.frame(bili = pbc$bili, chol = pbc$chol), na.rm = TRUE)
rowSums(x = data.frame(bili = pbc$bili, chol = pbc$chol), na.rm = TRUE)

#' What is the correlation between `serum bilirubin` and `serum cholesterol`
#' of the pbc data set?
cor(x = pbc$bili, pbc$chol, use = "complete.obs", method = "pearson")
cor(x = pbc$bili, pbc$chol, use = "complete.obs", method = "spearman")

#' What is the correlation matrix for the variables `serum bilirubin`, 
#' `serum cholesterol` and `alkaline ` of the pbc data set?
cor(x = data.frame(pbc$bili, pbc$chol, pbc$albumin), 
    use = "complete.obs")

#' What is the variance-covariance matrix for the above variables?
var(x = data.frame(pbc$bili, pbc$chol, pbc$albumin), 
    use = "complete.obs")
cov(x = data.frame(pbc$bili, pbc$chol, pbc$albumin), 
    use = "complete.obs")

#' A (co)variance matrix can be converted to a (pearson) correlation matrix 
#' with the help of the function `cov2cor()`:
cov2cor(V = var(x = data.frame(pbc$bili, pbc$chol, pbc$albumin),
                use = "complete.obs"))



#' ### Categorical data
#' What is the percentage of `placebo` and `treatment` patients in the pbc data
#' set? (In order to use the `percent()` function you will need to load the 
#' `memisc` package)
percent(x = pbc$trt)

#' What is the percentage of `females` and `males` in the pbc data set?
percent(x = pbc$sex)

#' What are the frequencies of each combination for the variables `trt` and
#' `sex` in the pbc data set?
table(trt = pbc$trt, sex = pbc$sex)

#' To add summaries (e.g. the sum) for each column and/or row use the 
#' `addmargins()` function:
tab <- table(trt = pbc$trt, sex = pbc$sex)
addmargins(A = tab)

#' We can also change the function (e.g use the mean):
addmargins(A = tab, FUN = mean)

#' What are the percentages of each combination for the variables `trt` and
#' `sex` in the pbc data set?
prop.table(x = tab)

#' For tables with more that 2 dimensions use:
ftable(x = table(trt = pbc$trt, sex = pbc$sex, ascites = pbc$ascites))
ftable(x = data.frame(trt = pbc$trt, sex = pbc$sex, ascites = pbc$ascites))

#' With the help of the arguments row.vars and col.vars we can determine which 
#' variables are given in the rows and which in the columns:
ftable(x = table(trt = pbc$trt, sex = pbc$sex, ascites = pbc$ascites), 
       row.vars = c(3, 2))



#' ## Handling missing values and outliers
#' Check if there are any missing values in the `serum cholesterol` variable
#' of the pbc data set:
is.na(pbc$chol)

#' Check if there are any complete cases in the `serum cholesterol` variable
#' of the pbc data set:
complete.cases(pbc$chol)


#' Obtain the dimensions of a matrix or data frame. We can use the function 
#' `dim()`:
dim(pbc)


#' Outliers: e.g. let's assume that patients with `serum bilirun` values > 25
#' are outliers.  \
#' 
#' * Check whether there are any outliers: obtain all rows from the data set 
#' which correspond to `serum bilirun` outliers:
pbc_out_bili <- pbc[pbc$bili > 25, ]

#' Calculate the mean and median of the `serum bilirun` variable without the 
#' outliers:
pbc_no_out_bili <- pbc[pbc$bili <= 25, ]

mean(pbc_no_out_bili$bili)
median(pbc_no_out_bili$bili)


#' Calculate the mean and median of the `serum bilirubin` variable without the 
#' missing values in the `serum cholesterol` variable:

pbc_no_mis_chol <- pbc[complete.cases(pbc$chol) == TRUE, ]
mean(pbc_no_mis_chol$bili)
median(pbc_no_mis_chol$bili)


