#' ---
#' title: "Demo: Introduction to R - Extra Programming Examples"
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
#' If you are using the package for the first time, you will have to first install it.
# install.packages("survival") 
library(survival)

#' ## Get data
pbc <- survival::pbc

#' ## Smart programming
#' * Keep good notes! 
#' 
#' Code 1:
x<-10
y<-10/2
z<-x+y

#' Code 2:
# Store a value
x <- 10
# Take half of the stored value
y <- x/2
# Get the sum of both values
z <- x + y

#' Which code (1 or 2) would you prefer? 
#' 
#' * Always check whether you can make your code faster.
#'
#' Create a matrix: \
A <- matrix(data = rnorm(n = 1e06), nrow = 1000, ncol = 1000)

#' Calculate the sum per row:
system.time( apply(A, 1, sum) )
system.time( rowSums(A) )

#' Calculate the sum per row, repeat this procedure 100 times:
res1 <- replicate(n = 100, expr = apply(A, 1, sum))
res2 <- replicate(n = 100, expr = rowSums(A)) # use specialized functions

#' Compute the cumulative sum of a numeric vector.
#' (cumulative sum of 1, 2, 3, 4 is 1, 3, 6, 10)
#' First we create a numerical vector:
x <- rnorm(n = 1e06, mean = 10, sd = 10)

# The cumulative sum in the beginning is 0:
cSum <- 0 
# Create an empty vector:
z <- numeric() 
for (k in 1:length(x)){
  cSum <- cSum + x[k]
  z[k] <- cSum
}

#' Explore how it works:
k <- 1
cSum <- 0
cSum <- cSum + x[k]

k <- 2
cSum <- cSum + x[k]

#' Use specialized functions:
cSum <- cumsum(x)

#' Explore the timing of the code.
cSum <- 0 
z <- numeric() 
system.time({
  cSum <- 0
  for (i in 1:length(x)){
    cSum <- cSum + x[i]
    z[i] <- cSum
  }
})

# better
system.time( cumsum(x) ) 


#' More than one ways exist to do something in R! \
#' 
#' ## Functions / loop / if statement
#' Calculate the mean weight for `males` and `females` in 100 datasets. \
#' Since we do not have 100 data sets, we will create them! \
#' Let's do that first manually...
datlist <- list()

i <- 1
set.seed(123+i)
patient <- c(1:20)
weight <- rnorm(20, 70, 10)
sex <- sample(1:2, 20, replace = TRUE)
sex <- factor(sex, levels = 1:2, labels = c("male", "female"))

datlist[[i]] <- data.frame(patient, weight, sex)

i <- 2
set.seed(123+i)
patient <- c(1:20)
weight <- rnorm(20, 70, 10)
sex <- sample(1:2, 20, replace = T)
sex <- factor(sex, levels = 1:2, labels = c("male", "female"))

datlist[[i]] <- data.frame(patient, weight, sex)
# ..... 
#'
#' It is too much work!
#' Now use a loop:
for (i in 1:100) {
  set.seed(123+i)
  patient <- c(1:20)
  weight <- rnorm(20, 70, 10)
  sex <- sample(1:2, 20, replace = T)
  sex <- factor(sex, levels = 1:2, labels = c("male", "female"))
  
  datlist[[i]] <- data.frame(patient, weight, sex)
}


#' Now that we have 100 data sets we need to calculate the mean `weight` per 
#' `gender` in each of them. \
#' Let's do that manually...
means <- matrix(data = NA, nrow = length(datlist), ncol = 2)

i <- 1
dat <- datlist[[i]]
means[i, ] <- tapply(dat$weight, dat$sex, mean)

i <- 2
dat <- datlist[[i]]
means[i, ] <- tapply(dat$weight, dat$sex, mean)
# .....
#'
#' Now use a loop:
for (i in 1:length(datlist)) {
  dat <- datlist[[i]]
  means[i, ] <- tapply(dat$weight, dat$sex, mean)
}

means

#' Select the data sets, where more than 39% of the patients are `females`:
# We will store the data sets in a list called newList:
newList <- list()

# Using a for loop and an if statement we can go through the data sets and
# check the percentages of females:
for (i in 1:length(datlist)) {
  dat <- datlist[[i]]
  if (sum(dat$sex == "female")/20 > 0.39) {
    newList[[i]] <- dat
  }
}

#' Now we need to remove the elements of the list that are NULL:
newList <- newList[!sapply(newList, is.null)]
length(newList)


#' Create a function that takes as input the data sets in a list format, the name 
#' of the `sex` variable and the name of the `female` category. \
#' This function returns only the data sets, where more than 39% of the patients
#'  are `females`. \
#' The output should be a list.
subset_data <- function(dataset = x, sex_var = "sex", female_cat = "female"){
  newList <- list()
  for (i in 1:length(dataset)) {
    dat <- dataset[[i]]
    if (sum(dat[sex_var] == female_cat)/20 > 0.39) {
      newList[[i]] <- dat
    }
  }
  newList <- newList[!sapply(newList, is.null)]
}

res <- subset_data(dataset = datlist, sex_var = "sex", female_cat = "female")

length(res)


#' Make a function that takes as input a data set, the name of a continuous 
#' variable and the name of a categorical variable. This function calculates 
#' the mean and standard deviation of the continuous variable for each group 
#' in the categorical variable.
des <- function(data = x, cont = "age", cat = "group"){
  c(tapply(data[[cont]], data[[cat]], mean),
    tapply(data[[cont]], data[[cat]], sd))
}

#' Apply the function to the `pbc` data set.
#' Use `age` and `sex` as continuous and categorical variables.
des(data = pbc, cont = "age", cat = "sex")

#' Why do we use `[[]]` and not `[]`?
data = pbc
cont = "age"
cat = "sex"
data[[cont]] # this is in a vector format
data[cont] # this is in a matrix format
# The tapply works with vector format data.

#' Now change the `des` function so that the user would specify the function 
#' applied to the data set.
des <- function(data = x, cont = "age", cat = "group", fun = mean){
  tapply(data[[cont]], data[[cat]], fun)
}

des(data = pbc, cont = "age", cat = "sex", fun = function(x) { sum(x) + 1 } )
