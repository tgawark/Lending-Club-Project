#Load decision tree package
library(rpart)
library(rpart.plot)

set.seed(44)

#Look for unique values on the loan status
unique(LTLoan2015[["loan_status"]])

#Subset data to loans that have a status
CleanLTLoan<-LTLoan2015[which(LTLoan$loan_status!="NA"),]

#Make new dummy variable "paid_flag" if the loan was paid
CleanLTLoan$paid_flag <- ifelse(CleanLTLoan$loan_status=="Fully Paid",1,0)

#Make interest rate numeric
CleanLTLoan$int_rate <- as.numeric(sub("%","",CleanLTLoan$int_rate))/100

#Calculate monthly interest rate
CleanLTLoan$mthly_int_rate <- (1+(CleanLTLoan$int_rate))^(1/12)-1

#Replace variables with numeric versions
CleanLTLoan$term <- ifelse(CleanLTLoan$term=="36 months",36,50)

#Subset to random subset
training_set <- CleanLTLoan[sample(1:nrow(CleanLTLoan), 1000, replace=FALSE),]
keep_vars <- names(CleanLTLoan) %in% c("paid_flag", "term", "mthly_int_rate", "annual_inc", "delinq_2_yrs",
                                       "loan_amnt", "grade")
training_set <- training_set[keep_vars]

#Adjust prior probabilities to address unbalanced data issue
non_default_proportion <- .5
default_proportion <- .5


#Build decision tree
tree_prior <- rpart(paid_flag ~ ., method = "class",
                    data = training_set, parms = list(prior = c(0.5, 0.5)))

plotcp(tree_prior)
plot(tree_prior)
text(tree_prior)

printcp(tree_prior)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[, "xerror"])

# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_prior)