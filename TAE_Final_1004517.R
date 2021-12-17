
# Please report your name and ID in the two lines below
# Name: ...
# ID: ...

# Load all packages
library(caTools)      # For properly splitting data into training and test sets
library(rpart)        # For CARTs
library(rpart.plot)   # For visualizing CARTs
library(randomForest) # For Random Forests
library(caret)        # For multiple ML functions
library(flexclust)    # For processing the results of cluster analysis
library(e1071)        # Naive Bayes Classifier 


## Question 1 [2 points] ####################################################

## Task 1 ###################################################################

# Consider a hypothetical scenario in which a bank decides to adopt a data-driven algorithm to evaluate 
# credit card applications. The information used by the algorithm encompasses a broad spectrum of data, 
# such as the applicant's gender, employment, assets owned, or salary. Identify one unintentional ethical 
# issue that might surface from the application of such algorithm. Justify your answer in one or two sentences.

# WRITE YOUR ANSWER HERE This approach may reveal a lot of information for its users. Therefore, the financial and individual information is unsafe to collect once it is release by evil people.

## Task 2 ###################################################################

# Consider a hypothetical modeling scenario in which you have solved a binary classification problem with 
# the aid of a CART. Is it possible to evaluate the Out-Of-Bag (OOB) error? Justify your answer in one or two sentences.

# WRITE YOUR ANSWER HERE 


#############################################################################


## Question 2 [16 points] ###################################################

# Our primary goal in this exercise is to understand the relationship between the first six variables 
# and `time_in_affairs`, and then develop data-driven models that predict it. 
# Before carrying out these tasks, we load the data:

# Remove all variables from the R environment to create a fresh start
rm(list=ls())
setwd("C:\\Users\\34940\\Desktop\\TAE_Final")
getwd()
# Create dataframe
exm <- read.csv("extramarital.csv")

## Task 1 ###################################################################

# A person has an affair if the variable `time_in_affairs` is larger than 0. In the dataset, 
# what percentage of the readers have had affairs?

# WRITE YOUR ANSWER HERE 
summary(exm$time_in_affairs>0)
2053/nrow(exm)
## Task 2 ###################################################################
  
# For each value, or level, of the variable `marriage_rating`, compute the number of readers who have had 
# an affair and the number of readers who have not had an affair. Based on this, do you think that readers 
# who had affairs tended to be less happy with their marriage than readers who did not have affairs? 
# Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE
sum(exm$time_in_affairs==0&exm$marriage_rating==1)
sum(exm$time_in_affairs!=0&exm$marriage_rating==1)

sum(exm$time_in_affairs=="0"&exm$marriage_rating=="2")
sum(exm$time_in_affairs!= "0" &exm$marriage_rating==2)

sum(exm$time_in_affairs=="0"&exm$marriage_rating=="3")
sum(exm$time_in_affairs!=0&exm$marriage_rating==3)

sum(exm$time_in_affairs=="0"&exm$marriage_rating=="4")
sum(exm$time_in_affairs!=0&exm$marriage_rating==4)

sum(exm$time_in_affairs!=0&exm$marriage_rating==5)
sum(exm$time_in_affairs=="0"&exm$marriage_rating=="5")

#I have found that those with affairs tend to be less happier according to the ratio

# Task 3 ####################################################################

# For each value, or level, of the variable `yrs_married`, compute the number of readers who have had 
# an affair and the number of readers who have not had an affair. Based on this, do you think that readers 
# who had affairs tended to be married for fewer years than readers who did not have affairs? 
# Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE

sum(exm$time_in_affairs==0&exm$yrs_married==0.5)
sum(exm$time_in_affairs!=0&exm$yrs_married==0.5)

sum(exm$time_in_affairs=="0"&exm$yrs_married==2.5)
sum(exm$time_in_affairs!= "0" &exm$yrs_married==2.5)

sum(exm$time_in_affairs=="0"&exm$yrs_married==6)
sum(exm$time_in_affairs!=0&exm$yrs_married==6)

sum(exm$time_in_affairs=="0"&exm$yrs_married==9)
sum(exm$time_in_affairs!=0&exm$yrs_married==9)

sum(exm$time_in_affairs!=0&exm$yrs_married==13)
sum(exm$time_in_affairs=="0"&exm$yrs_married==13)

sum(exm$time_in_affairs!=0&exm$yrs_married==16.5)
sum(exm$time_in_affairs=="0"&exm$yrs_married==16.5)

sum(exm$time_in_affairs!=0&exm$yrs_married==23)
sum(exm$time_in_affairs=="0"&exm$yrs_married==23)

# I found there is not clear pattern for people to have an affair of all ages
# Task 4 ####################################################################

# Calculate the correlation between each variable in the dataframe `exm` and `time_in_affairs`. 
# Does the value of the correlation reflect the answers you gave in Task 2 and Task 3?

# WRITE YOUR ANSWER HERE
cor(exm$time_in_affairs,exm)
# it corresponds to my answer that marraige rating is negatively related while yrs_married has a weak correlation
# Task 5 ####################################################################

# Suppose we want to build a regression model that predicts `time_in_affairs` as a function of the other variables. 
# Do you foresee any problem, or challenge, that we may face? 
# Justify your answer in one or two sentences. Hint: plot the variable `time_in_affairs`.

# WRITE YOUR ANSWER HERE
plot(exm$time_in_affairs)
# the number of time_in_affair is too dense, therefore the regression will face strong pressure and inaccuracy

# Task 6 ####################################################################

# We will now turn this problem into a multi-class classification problem by creating a new dependent variable. 
# Our new dependent variable, called `Affair`, will take three different values: `No`, `Minor`, and `Major`. 
# Create the variable by running the following code:

exm$affair <- factor(ifelse(exm$time_in_affairs == 0, "No", ifelse(exm$time_in_affairs >= 5, "Major", "Minor")))

# What is the fraction of readers that have had a major affair?

# WRITE YOUR ANSWER HERE
summary(exm$affair)
#fraction is 149/6366=0.02340559


# Task 7 ####################################################################

# Randomly split the dataset `exm` into a training set, containing 70% of the observations, and a testing set, 
# containing 30% of the observations. Use the following code:

set.seed(100)                                   
spl   <- sample.split(exm$affair,SplitRatio=0.7) 
train <- subset(exm,spl==TRUE);             
test  <- subset(exm,spl==FALSE);  

# Build a CART model to predict `affair` using all of the other variables as independent variables except for 
# `time_in_affairs`. Use the training set to build the model and the default settings in fitting the CART model. 
# Before running the algorithm, set the seed of the R random number generator to 100 using `set.seed(100)`. 
# Which variable does the tree split on at the top node?

# WRITE YOUR ANSWER HERE
cart1 <- rpart(affair~ .-time_in_affairs,data=train,method="class")
prp(cart1,type=1)
# top node marriage<4
# Task 8 ####################################################################

# The CART model you just built never predicts one of the three outcomes. Which one?

# WRITE YOUR ANSWER HERE
#except for major


# Task 9 ####################################################################

# Consider a person who is 31 years old and is strongly religious. Would the CART model predict that he / she 
# would have no, minor, or major affair?

# WRITE YOUR ANSWER HERE
summary(predict(cart1,age=31,religiosity=1,type="class"))
#would probabiliy to be No with 0.8366248 probability
# Task 10 ###################################################################

# Make predictions on the test set, and then create a confusion matrix. What is the overall accuracy of the model?

# WRITE YOUR ANSWER HERE
predictcart1 <- predict(cart1,newdata=test,type="class")
table(test$affair,predictcart1)

#accuracy= (202+1175)/(4+41+202+369+119+1175)=0.7209424

# Task 11 ###################################################################

# Look at the `cp` table created during the model identification process. Is it necessary to prune the tree? 
# If yes, what value of `cp` should we use? Explain your reasoning in one sentence.

# WRITE YOUR ANSWER HERE
printcp(cart1)
opt <- which.min(cart1$cptable[,"xerror"]) 
cp <- cart1$cptable[opt, "CP"] 

#there is need to prune the tree since the current xerror is too high

# Task 12 ###################################################################

# Now build a CART based on the answer you gave to the previous question (for example, you could prune 
# the original tree to build a smaller one, or decide to build a deeper tree). If you use the `rpart` function, 
# set the seed of the R random number generator to 100 using `set.seed(100)`. How many terminal leaves does the tree have?

# WRITE YOUR ANSWER HERE
??cart
set.seed(100)
cart2<- rpart(affair~ .-time_in_affairs,data=train,method="class",cp=0.001)
# Task 13 ###################################################################

# Using the new CART (created at Task 12), make predictions on the test set and then create a confusion matrix. 
# What is the overall accuracy of the model?

# WRITE YOUR ANSWER HERE
predictcart2 <- predict(cart2,newdata=test,type="class")
table(test$affair,predictcart2)
#new accuracy=(231+1146)/(6+39+234+340+148+1146)=0.7198118

# Task 14 ###################################################################

# Let's now move to a different model, Random Forests. Before running the algorithm, set the seed of the R 
# random number generator to 100 using `set.seed(100)`. Using the function `randomForest`, build a model 
# to predict `affair` using all of the other variables as independent variables except for `time_in_affairs`. 
# Use the default setting for the function `randomForest`. 
# What is the value of the Out-of-bag estimate of misclassification error?

# WRITE YOUR ANSWER HERE
library(randomForest)
set.seed(100)
rf1 <- randomForest(affair~ .-time_in_affairs, data = train)

# Task 15 ###################################################################

# Finally, use the model trained in the previous task to make predictions on the test set and create a confusion matrix. 
# What is the overall accuracy of the model?

# WRITE YOUR ANSWER HERE
pred_1_l <- predict(rf1, newdata = test)
table_1_l <- table(test$affair, pred_1_l)
sum(diag(table_1_l))/sum(table_1_l)
#accuracy is 0.7198953

# Task 16 ###################################################################

# Which is the most important variable (predictor) for the random forest?

# WRITE YOUR ANSWER HERE

varImpPlot(rf1)
importance(rf1)
#marriage_rating
#############################################################################


## Question 3 [12 points] ###################################################

# Our goal is to predict `RainTomorrow` using the other variables, or predictors, available in the dataset. 
# To that purpose, we will apply a technique known as cluster-then-predict.

# Remove all variables from the R environment to create a fresh start
rm(list=ls())

# Create dataframe
weatherAUS <- read.csv("weatherAUS.csv")

# Remove missing entries
weatherAUS <- na.omit(weatherAUS)

# Before beginning the exercise, we create a training and testing dataset, named `train` and `test`. 
# We then remove `RainTomorrow` (dependent variable) and `Date` from both sets, thereby creating 
# two additional datasets, `limitedTrain` and `limitedTest`. 
# Use the following code to carry out these operations:
  
# Create train and test datasets
idx <- c(1:2977)
spl <- c(1:2977)
spl[idx <= 2000] <- "TRUE"
spl[idx  > 2000] <- "FALSE"
train <- subset(weatherAUS,spl==TRUE)
test  <- subset(weatherAUS,spl==FALSE) 

# Create limitedTrain and limitedTest
limitedTrain <- train
limitedTrain$RainTomorrow  <- NULL
limitedTrain$Date  <- NULL
limitedTest <- test
limitedTest$RainTomorrow <- NULL
limitedTest$Date <- NULL

## Task 1 ###################################################################

# We are almost ready for the clustering process. The last pre-processing step is to normalize the data 
# by the mean and standard deviation of the variables in the training set. We can do this by using 
# the `preProcess()` function (`caret` package), which normalizes variables by subtracting the mean and dividing 
# by the standard deviation. Note that the function is also applied to the `limitedTest` dataset, 
# which will be used later. Use the following code:

# Normalize
preproc   <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest  <- predict(preproc, limitedTest)

# Why is it necessary to normalize the data? Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE
#to reduce the impact of some factors with extreme large values which affect the accuracy of the model

## Task 2 ###################################################################

# What are the minimum and maximum values for the variable `MinTemp` in the original (`limitedTrain`) 
# and transformed (`normTrain`) training sets?

# WRITE YOUR ANSWER HERE
summary(limitedTrain$MinTemp)
summary(normTrain$MinTemp)

#the values are -2.8,28.3 and -1.9628, 3.1494 respectively

# Task 3 ####################################################################

# We will now run the Hierarchical clustering algorithm on the normalized data (`normTrain` only). 
# The method requires us to first compute the Euclidean distance between the observations. 
# How many pairwise distances do we need to calculate? Suppose the dataset had 10,000 observations: 
# How many pairwise distances would we need to calculate?

# WRITE YOUR ANSWER HERE

distances <- dist(normTrain[,1:11], method="euclidean")
dim(normTrain)
#2000

# Task 4 ####################################################################

# Run the Hierarchical clustering algorithm using two different options for the linkage function: 
# `complete` and `ward.D2`. Each option yields a different dendrogram. How many clusters do we create 
# if we cut each dendrogram at a height equal to 10 units? Why is the number of clusters different? 
# Justify your answer in one sentence.

# WRITE YOUR ANSWER HERE

clusterWeather1 <- hclust(distances, method="ward.D2")
clusterGroups1 <- cutree(clusterWeather1, k=10)
plot(clusterWeather1)
plot(clusterGroups1)

clusterWeather2 <- hclust(distances, method="complete")
clusterGroups2 <- cutree(clusterWeather2, k=10)
# 1.We need to create 10 clusters. 2.because their methods are different

# Task 5 ####################################################################

# Let's keep the results obtained with the the linkage function `ward.D2`. Cut the dendrogram and create 5 clusters. 
# Which cluster has the highest average value of the variable `MinTemp`? 
# Note: To answer this question, you can use either the normalized data or original data.

# WRITE YOUR ANSWER HERE

clusterWeather1 <- hclust(distances, method="ward.D2")
clusterGroups1 <- cutree(clusterWeather1, k=5)

a<-subset(normTrain, clusterGroups1 == 1)
max(a$MinTemp)
#cluster 1 with value 3.149369
     
# Task 6 ####################################################################

# We now move to the k-means algorithm: set the random seed to 100 (by using the command `set.seed(100)`), 
# and run k-means clustering with 3 clusters on `normTrain`, with 50 restarts of the algorithm (option `nstart`). 
# Store the result in an object called `km`. Which cluster has the smallest number of observations? 

# WRITE YOUR ANSWER HERE
set.seed(100)

km <- kmeans(normTrain[,1:11],centers=3,nstart=50)
names(sort(table(km$cluster), decreasing = FALSE)[1])

#the second cluster

# Task 7 ####################################################################

# We are almost ready for cluster-then-predict: use the following code to obtain training set and testing set 
# cluster assignments for our observations and to do the predictions:

km.kcca      <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest  <- predict(km.kcca, newdata=normTest)

# How many test-set observations were assigned to Cluster 1, 2, and 3 for `clusterTest`?

# WRITE YOUR ANSWER HERE

table(clusterTest)
#335 173 469 respectively

# Task 8 ####################################################################

# Using the `subset()` function, build the dataframes `train1`, `train2`, and `train3`, containing 
# the elements in the original training set (`train`) assigned to clusters 1, 2, and 3, respectively. 
# Similarly build `test1`, `test2`, and `test3` from the original testing set (`test`). 
# Which training set has the highest number of rainy days?

# WRITE YOUR ANSWER HERE

train1 <- subset(train, clusterTrain == 1)
train2 <- subset(train, clusterTrain == 2)
train3 <- subset(train, clusterTrain == 3)
test1 <- subset(test, clusterTest == 1)
test2 <- subset(test, clusterTest == 2)
test3 <- subset(test, clusterTest == 3)

summary(train1$RainTomorrow=="Yes")
summary(train2$RainTomorrow=="Yes")
summary(train3$RainTomorrow=="Yes")
# the second train set

# Task 9 [2 points] #########################################################

# Build three Naive Bayes Classifiers, named `model1`, `model2`, and `model3`, 
# trained on `train1`, `train2`, and `train3`, respectively. When training the models, drop the variable `Date`. 
# Report the accuracy of each model on the corresponding testing set, that is, `test1`, `test2`, and `test3.`

# WRITE YOUR ANSWER HERE

model1 <- naiveBayes(as.factor(RainTomorrow)~.-Date,data=train1)
model2 <- naiveBayes(as.factor(RainTomorrow)~.-Date,data=train2)  
model3 <- naiveBayes(as.factor(RainTomorrow)~.-Date,data=train3)


predict1 <- predict(model1,newdata=test1,type="class")
table_1 <-table(predict1,test1$RainTomorrow)
sum(diag(table_1))/sum(table_1)
#accuracy for model 1 is 0.8268657

predict2 <- predict(model2,newdata=test2,type="class")
table_2 <-table(predict2,test2$RainTomorrow)

sum(diag(table_2))/sum(table_2)

#accuracy for model 1 is 0.6936416

predict3 <- predict(model3,newdata=test3,type="class")
table_3 <-table(predict3,test3$RainTomorrow)
sum(diag(table_3))/sum(table_3)

#accuracy for model 1 is 0.8742004

# Task 10 [2 points] ########################################################

# Finally, compute the overall test-set accuracy of the cluster-then-predict approach.

# WRITE YOUR ANSWER HERE
(sum(diag(table_1)) + sum(diag(table_2)) + sum(diag(table_3))) / sum(table_1+table_2+table_3)
#overall accuracy is 0.825998

############################################ END OF EXAM ###################################################