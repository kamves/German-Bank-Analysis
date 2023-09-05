# By Kamerin Vesajd and Louis Tu
## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.pos='H')
knitr::opts_chunk$set(echo = FALSE)


## ---- include=FALSE---------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(pander)
library(car)
library(corrplot)
library(glmnet)
library(patchwork)
library(faraway)
library(naniar)
library(ROCR)
library(MASS)
library(caret)
library(cowplot)
library(tinytex)
library(table1)
library(knitr)
library(float)
library(scales)
library(stringr)
library(flextable)

library(ISLR2)
library(class)
library(pROC)
library(e1071)


## ---- warning=FALSE---------------------------------------------------------------------------------------------
credTable <- read.csv("germancredit.csv", header = TRUE)

credTable$checkingstatus1 = factor(credTable$checkingstatus1,
  levels=c("A11", "A12", "A13", "A14"), 
  labels=c("< 0 DM", "0 <= ... < 200 DM", 
           "... >= 200 DM or salary assignment", "no checking account"))

credTable$history = factor(credTable$history, 
                            levels=c("A30", "A31", "A32", "A33", "A34"), 
                            labels=c("no credits taken/all credits paid back duly", "all credits at this bank paid back duly", "existing credits paid back duly till now", "delay in paying off in the past", "critical account/other credits existing (not at this bank)"))

credTable$purpose = factor(credTable$purpose,
                           levels=c("A40", "A41", "A42", "A43", "A44", "A45", "A46", "A48", "A49", "A410"),
                           labels=c("car (new)", "car (used)", "furniture/equipment", "radio/television", "domestic appliances", "repairs", "education", "retraining","business", "others"))

credTable$savings <- factor(credTable$savings,
                                  levels=c("A61", "A62", "A63", "A64", "A65"),
                                  labels=c("< 100 DM", ">= 100 ... < 500 DM", ">= 500 ... < 1000 DM", ">= 1000 DM", "unknown/no savings account"))


credTable$employ <- factor(credTable$employ,
                                  levels=c("A71", "A72", "A73", "A74", "A75"),
                                  labels=c("unemployed", "< 1 year", ">= 1 year ... < 4 years", ">= 4 years ... < 7 years", ">= 7 years"))

credTable$status <- factor(credTable$status ,
                                  levels=c("A91", "A92", "A93", "A94", "A95"),
                                  labels=c("male : divorced/separated", "female : divorced/separated/married", "male : single", "male : married/widowed", "female : single"))

credTable$others <- factor(credTable$others,
                                  levels=c("A101", "A102", "A103"),
                                  labels=c("none", "co-applicant", "guarantor"))

credTable$property <- factor(credTable$property,
                                  levels=c("A121", "A122", "A123", "124"),
                                  labels=c("real estate", "building society savings agreement/life insurance", "car or other", "unknown / no property"))

credTable$otherplans <- factor(credTable$otherplans,
                                  levels=c("A141", "A142", "A143"),
                                  labels=c("bank", "stores", "none"))

credTable$housing <- factor(credTable$housing,
                                  levels=c("A151", "A152", "A153"),
                                  labels=c("rent", "own", "for free"))


credTable$job <- factor(credTable$job,
                                  levels=c("A171", "A172", "A173", "A174"),
                                  labels=c("unemployed, unskilled, non-resident", 
                                           "unskilled resident", "skilled, employee", "manager, self-employed"))


credTable$tele <- factor(credTable$tele,
                                  levels=c("A191", "A192"),
                                  labels=c("none", "yes, registered under the customer name"))

credTable$foreign <- factor(credTable$foreign,
                                  levels=c("A201", "A202"),
                                  labels=c("yes", "no"))

# Missing "Purpose" predictor...
table1(~ + checkingstatus1 + history + savings +  purpose 
       + employ + status + others + property, data=credTable)




## ---------------------------------------------------------------------------------------------------------------
table1(~otherplans + housing + job + tele + foreign, data=credTable)


## ---------------------------------------------------------------------------------------------------------------
# non_numeric_cols <- credit %>% 
#   select_if(function(x) !is.numeric(x)) %>% 
#   names()


## ---------------------------------------------------------------------------------------------------------------
# Load data
credit <- read.csv("germancredit.csv", header = TRUE)

head(credit)

# Checks for missing values in the dataframe
n_miss_data = n_miss(credit)

# Checking for NA values
na_count = sum(is.na(credit))

pander(data.frame(Total_Missing_Values = n_miss_data))
pander(data.frame(Total_NAs = na_count))



## ---------------------------------------------------------------------------------------------------------------
german_credit.glm = glm(as.factor(Default) ~ . , data=credit, family="binomial")

plot(german_credit.glm, which=1)



## ---------------------------------------------------------------------------------------------------------------

plot(german_credit.glm, which=4 )


## ---------------------------------------------------------------------------------------------------------------

par(mfrow = c(1,2))

cooksd = cooks.distance(german_credit.glm)
influential = as.numeric(names(cooksd)[(cooksd > 4/nrow(credit))])

credit2 = credit[-influential,]
german_credit.glm2 = glm(Default ~ . , data=credit2, family = "binomial")

plot(german_credit.glm, which=1)
title("Original Model",
        adj = .5,
        cex.main = .9,
      line=1.3)

plot(german_credit.glm2, which=1)
title("Influentials Removed Model",
        adj = .5,
        cex.main = .9,
      line= 1.3)





## ---------------------------------------------------------------------------------------------------------------

# credit[508,]
# credit[758,]
# head(credit,10)



## ---------------------------------------------------------------------------------------------------------------
ggplot(data = credit, aes(x = Default, fill=factor(Default))) +
  geom_histogram(binwidth=1, alpha=0.9) +
  labs(x = "Default", 
       y = "Total Count",
       title="Applicants Default Distribution",
       fill = "Default Status") +
  scale_fill_manual(values=c("lightgreen", "#FFB6C1"), 
                    labels=c("Default = 0", "Default = 1")) +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
  legend.position = c(.9,.85),
  legend.title = element_text(face = "bold")) +
    scale_x_continuous(breaks = seq(0, 10, 1)) 



## ---------------------------------------------------------------------------------------------------------------

# Examining ages between 20-30 Data
credit20 = credit |> filter(between(credit$age, 20, 30))

agePlot= ggplot(data = credit, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Age", y = "Frequency") +
    scale_x_continuous(breaks = seq(0, max(credit$age), 10))
  
amountPlot = ggplot(data = credit, aes(x = amount)) +
  geom_histogram(binwidth = 500, fill = "lightgreen", color = "black") +
  labs(x = "Credit Amount", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, max(credit$amount), 2000))

durationPlot= ggplot(data = credit, aes(x = duration)) +
  geom_histogram(binwidth = 6, fill = "brown", color = "black") +
  labs(x = "Duration (months)", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, max(credit$amount), 10))

# -------- Plotting Credit Amount and Duration for Age: 20-30--------
amount20Plot = ggplot(data = credit20, aes(x = amount)) +
  geom_histogram(binwidth = 500, fill = "lightgreen", color = "black") +
  labs(x = "Credit Amount", y = "Frequency",
       title="Ages between 20-30") +
  scale_x_continuous(breaks = seq(0, max(credit$amount), 2000))

duration20Plot= ggplot(data = credit20, aes(x = duration)) +
  geom_histogram(binwidth = 6, fill = "brown", color = "black") +
  labs(x = "Duration (months)", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, max(credit$amount), 10))


# Overall Distribution
(agePlot + durationPlot) / amountPlot + plot_annotation(title = "Overall Distribution",
                               subtitle = "(Age, Duration, Credit Amount)",
                               theme = theme(plot.title = element_text(hjust = 0.5),
                                             plot.subtitle = element_text(hjust = 0.5)
                                             ))

duration20Plot / amount20Plot  + plot_annotation(title = "Ages Between 20-30 Distribution",
                           subtitle = "(Duration, Credit Amount)",
                           theme = theme(plot.title = element_text(hjust = 0.5),
                                         plot.subtitle = element_text(hjust = 0.5)
                                         ))




## ---- warning=FALSE---------------------------------------------------------------------------------------------
ggplot(credit, aes(x = age, fill = Default)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age vs Default Status",
       x= "Age",
       y="Default Density") +
  scale_x_continuous(breaks = seq(0, max(credit$age), 10))


## ---------------------------------------------------------------------------------------------------------------

ggplot(data = credit, aes(x = history , fill=factor(Default))) +
  geom_bar() +
  labs(x = "Credit History", 
       y = "Total Count",
       title="Credit History Status Distribution",
       fill="Default Status") +
  scale_fill_manual(values=c("lightgreen", "#ff9999"), 
                    labels=c("Default = 0", "Default = 1")) +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
  legend.position = c(.9,.85),
  legend.title = element_text(face = "bold"),
        axis.text.x = element_text(hjust = .5, vjust = .3, 
                                    margin = margin(t = 0, r = 3, b = 2, l = .5))) +
   scale_x_discrete(labels = c("No credits taken/ all 
credits paid back duly ", 
                               "All credits at this 
bank paid back duly", 
                               "Existing credits paid 
back duly till now", 
                               "Delay in paying 
off in the past",
                              "Critical account / other 
credits existing"))



## ---------------------------------------------------------------------------------------------------------------
# nrow(subset(credit, Default == 1 & history == "A32"))
# nrow(subset(credit, Default == 1))


## ---------------------------------------------------------------------------------------------------------------

ggplot(data = credit, aes(x = checkingstatus1 , fill=factor(Default))) +
  geom_bar() +
  labs(x = "Checking Status", 
       y = "Total Count",
       title="Checking Status Distribution",
       fill="Checking Status") +
  scale_fill_manual(values=c("lightgreen", "#ff9999"), 
                    labels=c("Default = 0", "Default = 1")) +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
  legend.title = element_text(face = "bold"),
        axis.text.x = element_text(hjust = .5, vjust = .3, 
                                    margin = margin(t = 0, r = 3, b = 2, l = .5))) +
   scale_x_discrete(labels = c("Less than 0 DM",
                               "0 to 200 DM",
                               "More than 200 DM",
                               "No Checking Account"))



## ---------------------------------------------------------------------------------------------------------------
ggplot(data = credit, aes(x = housing , fill=factor(Default))) +
  geom_bar() +
  labs(x = "Checking Status", 
       y = "Total Count",
       title="Housing Status Distribution",
       fill="Default Status") +
  scale_fill_manual(values=c("lightgreen", "#ff9999"), 
                    labels=c("Default = 0", "Default = 1")) +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
  legend.title = element_text(face = "bold"),
        axis.text.x = element_text(hjust = .5, vjust = .3, 
                                    margin = margin(t = 0, r = 1, b = 2, l = .5))) +
   scale_x_discrete(labels = c("Rent",
                               "Own",
                               "For Free"))


## ---------------------------------------------------------------------------------------------------------------
german_credit.glm = glm(Default ~ . , data=credit, family="binomial")

pred_names = names(german_credit.glm$coefficients)[-1]
# pander(data.frame(Total_predictors=length(pred_names)))

p_values = summary(german_credit.glm)$coefficients[,4]
sig_predictors = p_values[p_values<0.05]

pander(data.frame(Significant_P_Values = sig_predictors))



## ---------------------------------------------------------------------------------------------------------------
vif = vif(german_credit.glm)

vif_mc = vif[vif>10]
vif_non_mc = vif[vif<10]

pander(data.frame(Non_Collinear_Variables = vif_non_mc))


## ---------------------------------------------------------------------------------------------------------------
int_cols = sapply(credit, is.integer)
numeric_data = credit[,int_cols]

corrplot(cor(numeric_data, method="pearson"))
title(sub="Figure 1: Correlation Plot of Credit Data")


## ---------------------------------------------------------------------------------------------------------------
# Convert categorical variables to factors
credit$Default <- as.factor(credit$Default)
credit$checkingstatus1 <- as.factor(credit$checkingstatus1)
#credit$duration <- as.factor(credit$duration)
credit$history <- as.factor(credit$history)
credit$purpose <- as.factor(credit$purpose)
#credit$amount <- as.factor(credit$amount)
credit$savings <- as.factor(credit$savings)
credit$employ <- as.factor(credit$employ)
credit$installment <- as.factor(credit$installment)
credit$status <- as.factor(credit$status)
credit$others <- as.factor(credit$others)
credit$residence <- as.factor(credit$residence)
credit$property <- as.factor(credit$property)
#credit$age <- as.factor(credit$age)
credit$otherplans <- as.factor(credit$otherplans)
credit$housing <- as.factor(credit$housing)
credit$cards <- as.factor(credit$cards)
credit$job <- as.factor(credit$job)
credit$liable <- as.factor(credit$liable)
credit$tele <- as.factor(credit$tele)
credit$foreign <- as.factor(credit$foreign)

# Scale numeric variables
# credit$duration <- scale(credit$duration)
# credit$amount <- scale(credit$amount)
# credit$installment <- scale(credit$installment)
# credit$residence <- scale(credit$residence)
# credit$age <- scale(credit$age)
# credit$cards <- scale(credit$cards)
# credit$liable <- scale(credit$liable)



## ---------------------------------------------------------------------------------------------------------------
# Creating a logistic regression model with only significant predictors for high predictive accuracy.
# step_model = stepAIC(german_credit.glm, direction = "both", trace=FALSE)

set.seed(1)
# Creating a training set by 80% and testing set by 20%
train_index = sample(nrow(credit), 0.8*nrow(credit))
train_credit = credit[train_index,]
test_credit = credit[-train_index,]

# Initializing initial model and stepAIC model
initial_model = glm(Default ~ ., data=train_credit, family = binomial)
step_model = stepAIC(initial_model, direction = "both", trace=FALSE)

# Setting up the probabilities of initial model and step model
initial_prob = predict(initial_model, newdata = test_credit, type = "response")
step_prob = predict(step_model, newdata = test_credit, type = "response")

# Standardizing the dataset by 1's and 0s to properly read into performance and confusion matrix
initial_prob[initial_prob > 0.5] = 1
initial_prob[initial_prob <= 0.5] = 0
step_prob[step_prob > 0.5] = 1
step_prob[step_prob <= 0.5] = 0

# ROC Method for initial_model
roc_initial_pred = prediction(initial_prob, test_credit$Default) 
roc_initial_perf = performance(roc_initial_pred,"tpr","fpr")
# Plotting ROC performance 
plot(roc_initial_perf,colorize=TRUE, lwd = 2)
abline(a = 0, b = 1) 

# ROC Method for step_model
roc_step_pred <- prediction(step_prob, test_credit$Default)
roc_step_perf <- performance(roc_step_pred,"tpr","fpr")
# Plotting ROC performance 
plot(roc_step_perf,colorize=TRUE, lwd = 2)
abline(a = 0, b = 1)

# Performing AUC (Area Under Curve) to Initial_model and Step_model
roc_initial_auc = performance(roc_initial_pred, measure = "auc")
initial_auc = roc_initial_auc@y.values

roc_step_auc = performance(roc_step_pred, measure = "auc")

step_auc = roc_step_auc@y.values

pander(data.frame(Initial_Model_AUC = initial_auc[[1]], Step_Model_AUC = step_auc[[1]]))



## ---- message=FALSE---------------------------------------------------------------------------------------------

# Initial Model Confusion Matrix
initial_cm = confusionMatrix(factor(initial_prob), factor(test_credit$Default))
# Step Model Confusion Matrix
step_cm = confusionMatrix(factor(step_prob), factor(test_credit$Default))

#Printing confusion Matrix
print("Initial Model Confusion Matrix")
initial_cm[[2]]
print("StepAIC Model Confusion Matrix")
step_cm[[2]]

# Wrangling out the accuracy and rates from CM function
step_acc = data.frame(Step_Model_Acc = step_cm$overall[1])
initial_acc = data.frame(Initial_Model_Acc = initial_cm$overall[1])

initial_rates = data.frame(Initial_Model_Rates = initial_cm$byClass[1:2])
step_rates = data.frame(Step_Model_Rates = step_cm$byClass[1:2])

# Formatting the pander to allow two columns of 2 Dataframes
df3 = cbind(initial_rates,step_rates)
df4 = cbind(initial_acc, step_acc)
pander(df3, split.columns = c(1,2))
pander(df4, split.columns = c(1,2))


## ---------------------------------------------------------------------------------------------------------------
step_names = names(step_model$coefficients)[-1]

#Combining Regression Coefficient Est. & Std. Error
df1 = summary(step_model)$coefficients[,c(1,2,4)]
df2 = confint.default(step_model, level=0.95)
df_merge = cbind(df1, df2)

# Print out the Estimates, Std. Error, and Conf. Interval 
# of only the Statistically Sig. Predictors
print(subset(df_merge, df_merge[,3] < .05))

pander(data.frame(Total_predictors=length(step_names)))

# p_val = summary(df_merge)$coefficients[,4]
# sig_step = p_val[p_val<0.05]

# Obtain the summary output of the model
# summary_fit <- summary(step_model)
# # Extract the estimate values where the p-values are less than 0.05
# significant_coef <- coef(summary_fit)[, "Pr(>|z|)"] < 0.05
# significant_estimates <- coef(summary_fit)[significant_coef, "Estimate"]
# significant_estimates

# pander(data.frame(Significant_P_Values = sig_step))
# length(sig_step)


## ---------------------------------------------------------------------------------------------------------------
training_err = mean(step_prob != train_credit$Default)
# training_err2 = mean(initial_prob != train_credit$Default)
# pander(data.frame(Initial_Model_Error_Rate = training_err2))
pander(data.frame(Step_Model_Error_Rate = training_err))


## ---------------------------------------------------------------------------------------------------------------
# 1-exp(-.021)


## ---------------------------------------------------------------------------------------------------------------
# 1-exp(.0245)


## ---------------------------------------------------------------------------------------------------------------
# Split data to train and test
set.seed(123)
train <- sample(nrow(credit), 0.8 * nrow(credit))
credit.train <- credit[train,]
test_set <- credit[-train, ]
train.X <- data.matrix(credit[train, -1 ])
train.Y <- credit[train, 1 ]
test.X  <- data.matrix(credit[-train, -1])


## ---------------------------------------------------------------------------------------------------------------
# KNN
set.seed(123)
# Create the KNN model
knn_model <- train(Default ~ ., data = credit.train, 
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 10),
                   tuneLength = 20)

# Find the best k value for the model
k.optimal <- knn_model$bestTune[["k"]]

# Get the true outcomes for the test set
true_outcomes <- ifelse(test_set$Default == "1", 1, 0)

knn.model <- knn(train.X, test.X, train.Y, k = k.optimal, prob = T)

# Convert the predicted outcomes and true outcomes to factors with the same levels
#knn.model <- factor(knn.model, levels = levels(test_set$Default))
#true_outcomes <- factor(true_outcomes, levels = levels(test_set$Default))


confusion.matrix <- table(knn.model, true_outcomes)
knn.cm <- confusionMatrix(confusion.matrix)

knn.error_rate <- 1 - knn.cm$overall["Accuracy"]
knn.sensitivity <- knn.cm$byClass["Sensitivity"]
knn.specificity <- knn.cm$byClass["Specificity"]

# Create a prediction object
knn.pred <- prediction(attributes(knn.model)$prob, true_outcomes)

# Calculate the true positive rate (sensitivity) and false positive rate (1-specificity) for ROC curve
knn.perf <- performance(knn.pred, "tpr", "fpr")

plot(knn.perf, colorize = TRUE, lwd = 2)
abline(a = 0, b = 1) 

# Calculate the AUC
knn.auc <- performance(knn.pred, measure = "auc")
knn.auc <- knn.auc@y.values[[1]]

knn.cm$table
print(paste("Error rate:", knn.error_rate))
print(paste("Sensitivity:", knn.sensitivity))
print(paste("Specificity:", knn.specificity))
print(paste("AUC:", knn.auc))


## ---------------------------------------------------------------------------------------------------------------
# LDA
lda.model <- lda(Default ~ ., data = credit.train)
lda.pred <- predict(lda.model, newdata = test_set, type = "response")
#lda.table <- table(lda.pred$class, test_set$Default)
lda.cm <- confusionMatrix(lda.pred$class, test_set$Default)

lda.error_rate <- 1 - lda.cm$overall["Accuracy"]
lda.sensitivity <- lda.cm$byClass["Sensitivity"]
lda.specificity <- lda.cm$byClass["Specificity"]

lda.pred$posterior[,2] = ifelse((lda.pred$posterior[,2])>0.5,1,0)
lda.pred <- prediction(lda.pred$posterior[,2], test_set$Default) 

lda.perf <- performance(lda.pred,"tpr","fpr")
plot(lda.perf,colorize=TRUE, lwd = 2)
abline(a = 0, b = 1) 

lda.auc = performance(lda.pred, measure = "auc")

#lda.estimate <- predict(lda.model, newdata = test_set)$class
#lda.test_error_rate <- mean(lda.estimate != test_set$Default)

lda.cm$table
print(paste("Error rate:", lda.error_rate))
print(paste("Sensitivity:", lda.sensitivity))
print(paste("Specificity:", lda.specificity))
print(paste("AUC:", lda.auc@y.values))
#print(paste("Estimated test error rate:", lda.test_error_rate))


## ---------------------------------------------------------------------------------------------------------------
# QDA
qda.model <- qda(Default ~ ., data = credit.train)
qda.pred <- predict(qda.model, newdata = test_set, type = "response")
#qda.table <- table(qda.pred$class, test_set$Default)
qda.cm <- confusionMatrix(qda.pred$class, test_set$Default)

qda.error_rate <- 1 - qda.cm$overall["Accuracy"]
qda.sensitivity <- qda.cm$byClass["Sensitivity"]
qda.specificity <- qda.cm$byClass["Specificity"]

qda.pred$posterior[,2] = ifelse((qda.pred$posterior[,2])>0.5,1,0)
qda.pred <- prediction(qda.pred$posterior[,2], test_set$Default) 

qda.perf <- performance(qda.pred,"tpr","fpr")
plot(qda.perf,colorize=TRUE, lwd = 2)
abline(a = 0, b = 1) 

qda.auc = performance(qda.pred, measure = "auc")

#qda.estimate <- predict(qda.model, newdata = test_set)$class
#qda.test_error_rate <- mean(qda.estimate != test_set$Default)

qda.cm$table
print(paste("Error rate:", qda.error_rate))
print(paste("Sensitivity:", qda.sensitivity))
print(paste("Specificity:", qda.specificity))
print(paste("AUC:", qda.auc@y.values))
#print(paste("Estimated test error rate:", qda.test_error_rate))


## ---------------------------------------------------------------------------------------------------------------
# Naive Bayes
nb.model <- naiveBayes(Default ~ ., data = credit.train)
nb.pred <- predict(nb.model, newdata = test_set)
#nb.table <- table(nb.pred$class, test_set$Default)
nb.cm <- confusionMatrix(nb.pred, test_set$Default)

nb.error_rate <- 1 - nb.cm$overall["Accuracy"]
nb.sensitivity <- nb.cm$byClass["Sensitivity"]
nb.specificity <- nb.cm$byClass["Specificity"]

nb.pred = predict(nb.model, test_set, type = "raw")
nb.pred[,2] = ifelse((nb.pred[,2])>0.5,1,0)
nb.pred <- prediction(nb.pred[,2], test_set$Default)

# nb.pred = predict(nb.model, credit.test, type = "raw")
# nb.pred <- prediction(nb.pred[, 2], credit.test$Default) 
nb.perf <- performance(nb.pred,"tpr","fpr")
plot(nb.perf,colorize=TRUE, lwd = 2)
abline(a = 0, b = 1) 

nb.auc = performance(nb.pred, measure = "auc")

#nb.estimate <- predict(nb.model, newdata = credit.test, type = "class")
#nb.test_error_rate <- mean(nb.estimate != credit.test$Default)


nb.cm$table
print(paste("Error rate:", nb.error_rate))
print(paste("Sensitivity:", nb.sensitivity))
print(paste("Specificity:", nb.specificity))
print(paste("AUC:", nb.auc@y.values))
#print(paste("Estimated test error rate:", nb.test_error_rate))


## ---------------------------------------------------------------------------------------------------------------
results.matrix = matrix(0, nrow = 6, ncol = 4)
colnames(results.matrix) = c("ER", "SENS", "SPEC", "AUC")
rownames(results.matrix) = c("KNN", "LDA", "QDA", "LOG(IMR)", "LOG(SMR)", "NAIVE-B")
results.matrix[1,] = as.numeric( c(knn.error_rate, knn.sensitivity, knn.specificity, knn.auc))
results.matrix[2,] = as.numeric( c(lda.error_rate, lda.sensitivity, lda.specificity, lda.auc@y.values))
results.matrix[3,] = as.numeric( c(qda.error_rate, qda.sensitivity, qda.specificity, qda.auc@y.values))
results.matrix[4,] = as.numeric( c(1 - 0.7567, 0.8798, 0.4783, 0.6712))
results.matrix[5,] = as.numeric( c(1 - 0.7333, 0.8462, 0.4783, 0.6898))
results.matrix[6,] = as.numeric( c(nb.error_rate, nb.sensitivity, nb.specificity, nb.auc@y.values))

print(results.matrix)

