#Load random forest package
library(rpart)
library(randomForest)
library(cForest)
library(dplyr)

set.seed(44)

#Look for unique values on the loan status
unique(LTLoan2015[["loan_status"]])

#Subset data to loans that have a status
CleanLTLoan<-filter(LTLoan2015, loan_status!="NA")

#Make new dummy variable "paid_flag" if the loan was paid
CleanLTLoan$paid_flag <- ifelse(CleanLTLoan$loan_status=="Fully Paid",1,0)

#Make interest rate numeric
CleanLTLoan$int_rate <- as.numeric(sub("%","",CleanLTLoan$int_rate))/100

#Calculate monthly interest rate
mutate(CleanLTLoan,mthly_int_rate = (1+(int_rate))^(1/12)-1)
#CleanLTLoan$mthly_int_rate <- (1+(CleanLTLoan$int_rate))^(1/12)-1

#Replace variables with numeric versions
CleanLTLoan$term <- ifelse(CleanLTLoan$term=="36 months",36,50)

#Subset to random subset

training_set <- CleanLTLoan[sample(1:nrow(CleanLTLoan), 1000, replace=FALSE),]
keep_vars <- names(CleanLTLoan) %in% c("paid_flag", "term", "mthly_int_rate", "annual_inc", "delinq_2_yrs",
                                       "loan_amnt", "grade")
training_set <- training_set[keep_vars]
test <- CleanLtLoan[-training_set]

#Build condition inference tree Random Forest
fit <- cforest(training_set ~ .,
               data = training_set, controls=cforest_unbiased(ntree=2000, mtry=3))

#Make prediction
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
