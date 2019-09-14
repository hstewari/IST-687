  # Url link to read the data
  UrlToRead <- "http://lib.stat.cmu.edu/datasets/boston"
  
  # Read the dataset present in the url
  library(readr)
  DataRead <- read_table("http://lib.stat.cmu.edu/datasets/boston", skip = 22, col_names = FALSE)
  
  # Extract odd rows
  DR1 <- DataRead[seq(1, 1011, 2), ]
  
  # Add column names in the dataset
  colnames(DR1) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO")
  
  # Extract even rows
  DR2 <- DataRead[seq(2, 1012, 2), 1:3]
  
  # Add column names in the dataset
  colnames(DR2) <- c("B", "LSTAT", "MEDV")
  
  # Combining two datasets in one dataframe
  
  myData <- data.frame(c(DR1), c(DR2))
  
  myData

# Multiple Regression to predict the MEDV 

PredictMed <- lm(formula = MEDV ~ ., data = myData)

predict(PredictMed)

#Summary of Regression Analysis
summary(PredictMed)

# Remove those coefficients whose has not impact of Regression -- INDUS & Age
Model1 <- update(PredictMed,~. -INDUS)
Model11 <- update(Model1,~. -AGE)

#Summary of Regression Analysis after removing insignificant coefficients
Model <- summary(Model11)
Model

# get the residuals
res <- residuals(Model)
res

# Create a dataframe of residuals
res<- as.data.frame(res)


# Install ggplot
library(ggplot2)
# Create histogram of residuals
R_hist <- ggplot(res,aes(res)) +  geom_histogram(fill='brown',alpha=0.5)

R_hist
# Plotting of Multiple Regression Analysis
plot(PredictMed)


# Create a neural network model to get the predicted result
# reference - https://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html

# Get the head of the data with 2 rows
head(myData, 2)

## Normalize the data using min-max normalization##

# Create column max and min with vector
maxs <- apply(myData[,1:14], 2, max)
mins <- apply(myData[,1:14], 2, min)

# Scale the data
scale.data <- as.data.frame(scale(myData, center = mins, scale = maxs - mins))

# Get the head of the scale data
head(scale.data,2)

# Get the structure of the scale data
str(scale.data)

# set the seed to 112
set.seed(112)

#install caTools package
library(caTools)

# Get 75% of the sample data from scale data
sample.data <- sample.split(scale.data$INDUS, SplitRatio = 0.75)

# Get the training data
Train<- subset(scale.data, sample.data == TRUE)

# Get the test data
Test <- subset(scale.data, sample.data == FALSE)

# Install neuralnet package
library(neuralnet)

# Create a formaula to inculde in neural network
a <- colnames(scale.data[1:13])
a
Formula <- paste(a, collapse = ' + ')
Formula <- paste('MEDV ~ ', Formula)
Formula

# Creata the neural network using training data
NN <- neuralnet(Formula, Train, hidden = c(10,10,10), linear.output = TRUE, threshold = 0.1, rep = 10)

# Plot the neural network model
plot(NN, rep= "best")

# Compute Predictions off Test Set
predicted.values <- compute(NN, Test[1:13])

# Check out net result
print(head(predicted.values$net.result))

# head of predicted value
head(predicted.values$net.result)

# Put actual and prediction into a dataframe for comparison purposes
Result <- data.frame(actual = round(Test$MEDV), prediction = round(predicted.values$net.result))
Result

# Count the number of result where actual outcome matched predicted outcome
Prediction <- sum(round(Test$MEDV)==Result$prediction)
Prediction
