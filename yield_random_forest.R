# How to use the Random Forest Machine Learning Algorithm 

# Set-up
################

# if you don't have them already install the randomForest and ggplot packages
install.packages("randomForest")
install.packages("ggplot2")

# load the packages
library(randomForest)
library(ggplot2)

# Classification
################

# load our data; the location of the data will depend on your own personal setup; will put notes in the tutorial on how to find location path
climate_data <- read.csv("C:/Users/aliav/OneDrive/Documents/Working Directory/Comp Bio/climate_features2.csv")

# here we want to predict the variable "Stability" from all our other variables
# in order for randomForest to predict classifications "Stability" needs to be a factor
# to check if "Stability" is a factor:
is.factor(climate_data$Stability)

# since Stability is not a factor in our case we need to turn it into a factor:
climate_data$Stability <- factor(climate_data$Stability)

# in order to make our results reproducible we need to set the seed
set.seed(33)

# Now we split the dataset into a training and testing dataset; here we will do 20% testing and 80% training
trainIndex <- sample(1:nrow(climate_data), 0.8*nrow(climate_data))
trainData <- climate_data[trainIndex, ]
testData <- climate_data[-trainIndex, ]

# Now we will create the random forest model based on the training set
climate.rf <- randomForest(Stability ~.,  # Here we signify that we want the model to predict/classify Stability based on all the other variables
                           data = trainData,
                           importance = TRUE,
                           proximity = TRUE,
                           ntree = 500, # number of trees our model will use; if unspecified default is 500
                           mtry = 3) # number of variables to be tried at each split; automatically uses sqrt(# of predictor variables in whole dataset)

# We can then print our model to see what it looks like
print(climate.rf)
# this shows us the correct and incorrect classifications that were made
# for example 21 samples were correctly classified as high
# 7 were correctly classified as low, but 3 were incorrectly classified as medium when they should've bee classified as low
# percent of missclassifications is referred to as OOB (out-of-bag) error

# Tuning the model
# to improve the accuracy of our model we can adjust the hyperparmeter settings using tuneRF()
# or by altering them in the model itself

model_tuned <- tuneRF(
  x=climate_data[,-11], # define predictor variables: everything but the last column
  y=climate_data$Stability, # define response variable
  ntreeTry=500, # number of trees to build
  mtryStart=3, # starting number of predictor variables to consider at each split
  stepFactor=1.5, # factor to increase by until the out-of-bag estimated error stops improving by a certain amount
  improve=0.01, # amount that the out-of-bag error needs to improve by to keep increasing the step factor
  trace=FALSE # don't show real time progress
)

# tuneRF will automatically print a graph showing how out-of-bag error changes with
# the number of variables we try at each split

# from this we can see that 4 seems to be an optimal number of predictors to try at each split
# Re-adjusting the model:
climateB.rf <- randomForest(Stability ~.,
                           data = trainData,
                           importance = TRUE,
                           proximity = TRUE,
                           ntree = 500, 
                           mtry = 4) 

# Print the new model
print(climateB.rf)
# we can see that OOB error has been reduced to 6.82%

# Now we make predictions based on the test set
predictions <- predict(climateB.rf, newdata = testData)

# To evaluate the accuracy of our model we plot a confusion matrix of actual vs predicted values
confMatrix <- table(Predicted = predictions, Actual = testData$Stability)

confMatrixDF <- as.data.frame(confMatrix)
colnames(confMatrixDF) <- c("Predicted", "Actual", "Count")

ggplot(data = confMatrixDF, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "purple") +
  theme_minimal() +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# if we wanted to we could also plot the confusion matrix of our un-tuned model
# and we would be able to see that unlike our improved model, where no
# misclassifications happened, our un-tuned model had 1 misclassification

# to produce a variable importance plot we can use the command varImpPlot()
varImpPlot(climateB.rf)

############

# Regression
############

# Regressional models are mostly the same as classification models except for a few
# minor differences in assessing accuracy

# First we'll make a model predicting Annual_Yield from the other variables
# following the same steps as before

climate_data2 <- read.csv("C:/Users/aliav/OneDrive/Documents/Working Directory/Comp Bio/climate_features.csv")

set.seed(33)

trainIndex2 <- sample(1:nrow(climate_data2), 0.8*nrow(climate_data2))
trainData2 <- climate_data2[trainIndex2, ]
testData2 <- climate_data2[-trainIndex2, ]

climate2.rf <- randomForest(Annual_Yield ~.,  
                           data = trainData2,
                           importance = TRUE,
                           proximity = TRUE,
                           ntree = 500, 
                           mtry = 3)

# To display the model
climate2.rf
# this shows the amount of variance explained by the model
# as well as Mean of squared residuals (MSR) which is a measure of how much predicted
# values vary from actual values

# you can use the following command to find the number of trees used in the
# model that produced the lowest Mean Squared Error (MSE)
which.min(climate2.rf$mse)
# The following will find the Root Mean Squared Error (RMSE) of the best model
sqrt(climate2.rf$mse[which.min(climate2.rf$mse)]) 

# you can use plot to find how the Mean Squared Error (MSE)
# changes with the number of trees
plot(climate2.rf)
