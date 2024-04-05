library(survival)

plot(pbc$time, pbc$chol, pch = 16, ylab = "Serum cholesterol", xlab = "Time")


barplot(chol ~ trt, data = pbc)


colvec <- c(rgb(0,0,1,1), rgb(0,0,1,0.6), rgb(0,0,1,0.2))

barplot(table(pbc$status, pbc$sex), col = colvec, ylab = "Number of patients", xlab = "Sex")
legend("topleft", c("alive", "transplanted", "died"), fill = colvec, bty = "n")


boxplot(chol ~ sex, data = pbc, ylab = "Serum cholesterol", xlab = "Sex")

hist(pbc$bili, xlab = "Serum bilirubin", main = "")



# tab <- table(pbc$status, pbc$sex)
# rownames(tab) <- c("alive", "trasplanted", "died")
# dotchart(tab, xlim = c(0,220), bg = "skyblue",
#                    main = "", xlab = "Number of patients")


dotchart(pbc$bili[pbc$id %in% c(1:30)], xlab = "Serum bilirubin", ylab = "", main = "Serum bilirubin for 30 patients")
title(ylab="Patient", mgp=c(1,1,0))


pie(table(pbc$sex))


pairs(data.frame(Age = pbc$age, Time = pbc$time, Cholesterol = pbc$chol))
