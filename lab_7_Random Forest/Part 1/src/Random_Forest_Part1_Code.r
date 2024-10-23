############################################################
##  Intro to random forest analysis in R - Part 1
##  Data Prep and Basic Analysis
##  German Credit data originally prepared by Prof. Dr. Hans Hofmann
##  Data made publicly available through UCI Machine Learning Repository
##  Data available here: https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29
############################################################


############################################################
##  Load Libraries
##  randomForest, caret
############################################################

library(randomForest)

library(caret)

library(ggplot2)

library(randomForestExplainer)

###########################################################
##  Set working directory
##  Load data
###########################################################

##  Define file path as string and set wd
path <- "/Volumes/KINGSTON UF/KINGSTON COPY/R Class/Lectures/Random Forest/Part 1/"
setwd("")

##  Set random seed so analysis is repeatable
set.seed(42)

##  Read in dataset csv
gc <- read.csv("german_credit.csv")

###########################################################
##  Prep data for analysis
##  Data conversion and train/test subsets
###########################################################

##  Examine the data structure
str(gc)

##  The response variable (the one we want to predict) is creditability
##  This variable must be converted to factor as it is categorical
gc$Creditability <- as.factor(gc$Creditability)

ggplot(data = gc, aes(x = Age..years., fill = Creditability, colour = Creditability)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.25) + theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

ggplot(data = gc, aes(x = Credit.Amount, fill = Creditability, colour = Creditability)) + 
  geom_histogram(binwidth = 1000, position = "identity", alpha = 0.2) + theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))



##  Create training and testing sets
partition <- sample(2, nrow(gc), replace = TRUE, prob = c(0.8, 0.2))




summary(partition)
table(partition)
train <- gc[partition==1,]
test <- gc[partition==2,]

##  Examine balance of datasets

table(gc$Creditability)
table(train$Creditability)
table(test$Creditability)

###########################################################
##  Perform analysis and assess model accuracy
##  Random forest model, accuracy assessment, improve model
###########################################################

##  Run random forest model
rf <- randomForest(Creditability~., data = train, localImp = TRUE)
rf
explain_forest(rf) # Takes a few moments to run

##  Predict creditability in training subset using random forest model
predtrain <- predict(rf, train)

##  Assess accuracy
confusionMatrix(predtrain, train$Creditability)

##  Predict creditability in test subset using random forest model
predtest <- predict(rf, test)

##  Assess accuracy
confusionMatrix(predtest, test$Creditability)

##  Improve random forest model
##  Assess appropriate number of trees
plot(rf)

##  Assess appropriate number of variables sampled at each split
##  Create a variable of type double and length of 20
ooberror <- double(20)

##  mtry is number of Variables randomly chosen at each split
##  Create for loop that cycles through all possible values of mtry and stores oob error
for(mtry in 1:20){
  ##  Run random forest model with mtry variable
  rf <- randomForest(Creditability~., data = train, mtry = mtry, ntree = 140)
  ##  Error of all trees fitted
  ooberror[mtry] <- rf$err.rate[140]
}

##  Plot mtry versus oob error
plot(1:mtry, ooberror, type ="b", xlab = "Number of Variables Considered",
     ylab = "OOB Error", xaxt = "n")
axis(1, at = seq(1, 20, 1), cex.axis = 0.8)


#looks at error rate changes based on number of vars 
  #you want to choose something further out and then we can calculate mtry

##  Re-run random forest model with optimized parameters
rf <- randomForest(Creditability~., data = train, mtry = 9, ntree = 140) #mtry is number of variables optmizing hyper param

##  Predict creditability in training subset using random forest model
predtrain <- predict(rf, train)

##  Assess accuracy
confusionMatrix(predtrain, train$Creditability)

##  Predict creditability in test subset using random forest model
predtest <- predict(rf, test)

##  Assess accuracy
confusionMatrix(predtest, test$Creditability)


###########################################################
##  Examine model and make inferences about variables
##  Plot variable importance, relationships among variables
###########################################################

##  Examine variable importance measured by Gini
##  Gini is essentially the probability of a new record being incorrectly classified
varImpPlot(rf)
importance(rf)

##  Explore relative relationship between variable and random forest classification
partialPlot(rf, train, Credit.Amount, "1") # on a scale of more to less likely
partialPlot(rf, train, Credit.Amount, "0")
partialPlot(rf, train, Account.Balance, "1")
partialPlot(rf, train, Account.Balance, "0")
partialPlot(rf, train, Age..years., "1")
partialPlot(rf, train, Age..years., "0")

#########################################################
##  Save output and random forest model
#########################################################

save.image(paste(path, "output/German_Credit.RData", sep=""))




















