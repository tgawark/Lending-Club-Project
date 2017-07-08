setwd("~/R Projects/Lending Club/Data")
getwd()

LTLoan2015 <- read.csv("2015 Loan Data.csv", skip = 1)

#Load decision tree package
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("cForest")

save(LTLoan2015, file="loans_2015")