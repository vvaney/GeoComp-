############################################################
##  Intro to random forest analysis in R - Part 2
##  Data Prep and Basic Analysis
##  Diamond attribute data
##  Data made publicly available through ggplot2 package by Hadley Wickham
############################################################


############################################################
##  Load Libraries
##  randomForest, caret
############################################################

library(randomForest)

library(caret)

library(ggplot2)

library(githubinstall)
githubinstall("jcolors")

library(jcolors)

###########################################################
##  Set path and random seed
##  Load data
###########################################################

##  Define file path as string
data("diamonds")

##  Set random seed so analysis is repeatable
set.seed(42)

###########################################################
##  Prep data for analysis
##  Data conversion and train/test subsets
###########################################################

##  Examine the data structure
str(diamonds)

##  Exploratory plot of data
ggplot(data = diamonds[diamonds$carat < 2.5,], aes(x = carat, y = price, colour = clarity)) +
  geom_point(alpha = 0.1) + scale_color_jcolors("rainbow") +
  geom_smooth() + theme_light_bg()

##  Create training and testing sets
partition <- sample(2, nrow(diamonds), replace = TRUE, prob = c(0.8, 0.2))
train <- diamonds[partition==1,]
test <- diamonds[partition==2,]


###########################################################
##  Perform analysis and assess model accuracy
##  Random forest model, accuracy assessment, improve model
###########################################################

##  Run random forest model
rf <- randomForest(price~., data = train)

##  Predict diamond price in training subset using random forest model
predtrain <- predict(rf, train)

##  Assess accuracy via these due to y being continuous 
##  RMSE
sqrt(mean((predtrain-train$price)^2))
##  MAE
mean(abs(predtrain-train$price))
##  MAPE
mean(abs((train$price-predtrain)/train$price)*100)
##  R-squared
cor(predtrain, train$price)^2

##  Predict diamond price in test subset using rand forest model
predtest <- predict(rf, test)

##  Assess accuracy
##  RMSE
sqrt(mean((predtest-test$price)^2))
##  MAE
mean(abs(predtest-test$price))
##  MAPE
mean(abs((test$price-predtest)/test$price)*100)
##  R-squared
cor(predtest, test$price)^2

##  Improve random forest model
##  Assess appropriate number of trees
plot(rf)

##  Assess appropriate number of variables sampled at each split
##  Create a variable of type double and length of 9
ooberror <- double(9)

##  mtry is number of Variables randomly chosen at each split
##  Create for loop that cycles through all possible values of mtry and stores oob error
for(mtry in 1:9){
  ##  Run random forest model with mtry variable
  rf <- randomForest(price~., data = train, mtry = mtry, ntree = 300)
  ##  Error of all trees fitted
  ooberror[mtry] <- rf$mse[300]
}

##  Plot mtry versus oob error
plot(1:mtry, ooberror, type = "b", xlab = "Number of Variables Considered",
     ylab = "OOB Error", xaxt = "n")
axis(1, at = seq(1, 9, 1), cex.axis = 0.8)

##  Re-run random forest model with optimized parameters
rf <- randomForest(price~., data = train, mtry = 6, ntree = 300, importance = T)

##  Predict diamond price in training subset using random forest model
predtrain <- predict(rf, train)

##  Assess accuracy
##  RMSE
sqrt(mean((predtrain-train$price)^2))
##  MAE
mean(abs(predtrain-train$price))
##  MAPE
mean(abs((train$price-predtrain)/train$price)*100)
##  R-squared
cor(predtrain, train$price)^2

##  Predict diamond price in test subset using random forest model
predtest <- predict(rf, test)

##  Assess accuracy
##  RMSE
sqrt(mean((predtest-test$price)^2))
##  MAE
mean(abs(predtest-test$price))
##  MAPE
mean(abs((test$price-predtest)/test$price)*100)
##  R-squared
cor(predtest, test$price)^2

###########################################################
##  Examine model and make inferences about variables
##  Plot variable importance, relationships among variables
###########################################################

##  Examine variable importance measured by %IncMSE and IncNodePurity
##  %IncMSE is measured by MSE, this is analogous to accuracy-based importance
##  IncNodePurity is measured by residual sum of squares, this is analogous to Gini-based importance

varImpPlot(rf)
importance(rf)

##  Explore relative relationship between variable and random forest classification
partialPlot(rf, as.data.frame(train), color)
partialPlot(rf, as.data.frame(train), clarity)
partialPlot(rf, as.data.frame(train), carat)
partialPlot(rf, as.data.frame(train), y)

#########################################################
##  Save output and random forest model
#########################################################

save.image(paste(path, "output/Diamond_Price.RData", sep=""))










