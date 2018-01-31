
##------------Bank Marketing Analysis---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------
#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

#----------------------------------------------------------------------------

bank_data_DT <- bank_data
bank_data_RF <- bank_data

#---------------------------------------------------------------------------
#####################Model Building starts####################
##Starting with logistic regression
## Model Building   

##---------Logistic Regression----------#

# Required Packages

library(caret)
library(caTools)
library(dummies)

#---------------------------------------------------------    

# Removing binning variables 

bank_data <- bank_data[, -21]


#creating dummy variables

bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

#---------------------------------------------------------    

### Model 1: Logistic Regression


library(MASS)

library(car)

logistic_1 <- glm(response ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

 logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

logistic_2 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + previousLess_than_3_times + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    euribor3m, family = "binomial", data = train)



# checking vif for logistic_2 

vif(logistic_2)

summary(logistic_2)

#---------------------------------------------------------    

# removing "previousLess_than_3_times"since vif is high and also the variable is not significant 

logistic_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure + emp.var.rate +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_3)

vif(logistic_3)


# Removing "emp.var.rate" variable.

logistic_4 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)


summary(logistic_4)

vif(logistic_4)

# Removing "monthaug"

logistic_5 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_5)



# Removing "monthnov"

logistic_6 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)



summary(logistic_6)



# Removing "jobunemployed"


# Removing ""
logistic_7 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_7)


# Removing "educationprofessional.course"
logistic_8 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + day_of_weekmon +  
                    day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                    cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_8)


# Removing "day_of_weekthu"

logistic_9 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + day_of_weekmon +  
                    duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                    cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_9)



# Removing "day_of_weekfri"

logistic_10 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                     jobtechnician + educationTertiary_Education + contactcellular + 
                     monthjun + monthmar + monthmay + day_of_weekmon +  
                     duration + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)


summary(logistic_10)


# Removing "jobadmin." 

logistic_11 <- glm(formula = response ~ jobretired + jobstudent + 
                     jobtechnician + educationTertiary_Education + contactcellular + 
                     monthjun + monthmar + monthmay + day_of_weekmon +  
                     duration + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_11)

# Removing "jobtechnician"

logistic_12 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationTertiary_Education + contactcellular + 
                     monthjun + monthmar + monthmay + day_of_weekmon +  
                     duration + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_12)

# Removing "monthjun"

logistic_13 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationTertiary_Education + contactcellular + 
                     monthmar + monthmay + day_of_weekmon + duration +  
                     campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)
summary(logistic_13)

logistic_final <- logistic_13
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test[, -61], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf
#Accuracy : 0.9123, Sensitivity : 0.42457, Specificity : 0.97419
#Having a very low sensitivity value, let us write a function to get optimal cutoff value 

#---------------------------------------------------------    

# Let's find out the optimal probability cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 12% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.128, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

#acc 0.867271, sens 0.8649425, spec 0.8675666 
#This is a good model with accuracy, sensitivity, specificity around 87%.

################################################################################
##Let us build a model using decision tree
## Model Building- Model 2: Decision Tree

# Packages Required

library(rpart)
library(rattle)

#---------------------------------------------------------    

# Load the dataset

bank <- bank_data_DT[,-21]


# Let's split the data in training and test datasets. 

set.seed(100)
split_indices <- sample.split(bank$response, SplitRatio = 0.70)

train_dt <- bank[split_indices, ]

test_dt <- bank[!split_indices, ]

nrow(train_dt)/nrow(bank)

nrow(test_dt)/nrow(bank)

#---------------------------------------------------------    

# building a tree with arbitrary minsplit and cp
banktree_1 <-  rpart(response ~ ., data=train_dt, method= "class", 
                     control=rpart.control(minsplit=65, cp=0.001))

plot(banktree_1)

# This is clearly an overfitted tree
# Classic decision tree problem


# Increasing the minsplit two fold to 130 
banktree_2 <-  rpart(response ~ ., data=train_dt, method= "class",
                     control=rpart.control(minsplit=130, cp=0.001))

plot(banktree_2)

# This one is better, but still looks a little too complex
# install rpart.plot and load the library
library(rpart.plot)

fancyRpartPlot(banktree_2)

# Listing the variables by importance: Duration, nr.employed, euribor3m are the top 3
banktree_2$variable.importance

# We can further simplify the tree by increasing minsplit
banktree_3 <-  rpart(response ~ ., data=train_dt, method= "class",
                     control=rpart.control(minsplit=400, cp=0.001))

banktree_3$variable.importance
fancyRpartPlot(banktree_3)

# banktree_3 looks like an acceptable model; lets increase minsplit a litte more
banktree_4 <- rpart(response ~ ., data=train_dt, method= "class",
                    control=rpart.control(minsplit=800, cp=0.001))

fancyRpartPlot(banktree_4)
banktree_4$variable.importance

#---------------------------------------------------------    
## Model Evaluation for banktree_3 and banktree_4
# using test data from now on
# banktree_3
banktree_3_pred <- predict(banktree_3, test_dt[, -20], type = "class")

banktree_3_pred <- ifelse(banktree_3_pred==1,"yes","no")
test_dt$response <-ifelse(test_dt$response==1,"yes","no")

confusionMatrix(banktree_3_pred, test_dt[, 20], positive = "yes")

# Accuracy is 91.07%, sensitivity is only 50.04%

# banktree_4
banktree_4_pred <- predict(banktree_4, test_dt[, -20], type = "class")
banktree_4_pred <- ifelse(banktree_4_pred==1,"yes","no")
confusionMatrix(banktree_4_pred, test_dt[, 20], positive = "yes")

# Sensitivity is again low here; we can improve the model quite a bit since logistic model has sensitivtiy around 75%
# Though we can choose banktree_4, we should rather build a random forest instead
# It will avoid over fitting 

#---------------------------------------------------------    


#---------------------------------------------------------
# ----Model Building - Model 3:- Random forest

#---------------------------------------------------------    

# Package required for randomForest algorithm is:
# install randomForest
library(randomForest)
library(ggplot2)
#---------------------------------------------------------    

# Spliting the bank data in 70:30 ratio

set.seed(101)

bank$response <- as.factor(ifelse(bank$response==1,"yes","no"))
split_indices <- sample.split(bank$response, SplitRatio = 0.70)

train_rf <- bank[split_indices, ]

test_rf <- bank[!split_indices, ]

nrow(train_rf)/nrow(bank)

nrow(test_rf)/nrow(bank)

#---------------------------------------------------------    

# Building the model 

bank_rf <- randomForest(response ~., data = train_rf, proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred <- predict(bank_rf, test_rf[, -20], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= 0.21, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 20], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

# Accuracy 
conf_forest$overall[1]


# Final RF important variables
importance <- bank_rf$importance 

importance <- data.frame(importance)
###----------------------------###############################################
#---------------------------------------------------------    
# ------Model Evaluation----------------------------------

# Appending the probabilities and response variables to the test data

test_rf$predicted_probs <- rf_pred[, 2]

test_rf$predicted_response <- predicted_response_22

#---------------------------------------------------------    

# Creating new dataframe "test_predictions_rf"

test_predictions_rf <- test_rf[, c("response", "predicted_probs", "predicted_response")]


summary(test_predictions_rf$response)
summary(test_predictions_rf$predicted_response)


response_rate <- table(test$response)[2]/(table(test$response)[1] + table(test$response)[2])

# sorting the probabilities in decreasing order 
test_predictions_rf <- test_predictions_rf[order(test_predictions_rf$predicted_probs, decreasing = T), ]

#Downloading the data 
write.csv(test_predictions_rf,"test_prediction_rf.csv")

summary(test_predictions_rf$response[1:6800])
summary(test_predictions_rf$predicted_response[1:6800])

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

test_predictions_rf$response <- as.factor(ifelse(test_predictions_rf$response=="yes",1,0))

LG = lift(test_predictions_rf$response, test_predictions_rf$predicted_probs, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")

# Total Cost incur throught direct telemarketing 

# Let's say if you have spent 1Re for each customer
View(LG)


# The Cumulative Lift of 3.4 for top two deciles,
# means that when selecting 20% of the records based on the model, 
# one can expect 3.4 times the total number of targets (events) found by randomly 
# selecting 20%-of-records without a model. In terms of customer attrition (churn) model, 
# we can say we can cover 3.4 times the number of attritors by selecting only 20% of the
# customers based on the model as compared to 20% customer selection randomly.

### Analyzing the Charts: Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers to contact. The lift chart shows how much more likely we are to receive
# respondents than if we contact a random sample of customers. For example,
# by contacting only 10% of customers based on the predictive model we will reach 
# 3 times as many respondents as if we use no model.
############-------------------------------------------------------------########################################
  





