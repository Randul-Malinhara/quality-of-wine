# Import libraries
library(readxl)
library(neuralnet)
library(caTools)
library(MLmetrics)
library(Metrics)
library(xlsx)

# Build the Assumed Values and real Values table
predictedVsreal <- function (assumed, mlp_test) {
  assumedTest <- cbind(assumed, as.data.frame(mlp_test$net.result))
  colnames(assumedTest) <- c("Expected Output", "NeuralNetwork Output")
  return(assumedTest)
}

# Evaluation Function
rmse <- function (real, predicted) 
{
  RMSE <- sqrt(mean((real - predicted)^2))
  return(RMSE)
}
evaluationFunction <- function(real,predict) {
  rmse_mlp <- rmse(real = real, predicted = predict)
  mae_mlp <- Metrics::mae(real = real, predicted = predict)
  mape_mlp <- MAPE(y_pred = predict, y_true = real)
  return(c(rmse_mlp, mae_mlp, mape_mlp))
}

# Import Processed Data set
powerOutage <- read_xlsx("C:/Users/RANDUL/Desktop/2nd Semester of 2nd Year/5DATA001C.2 Machine Learning and Data Mining/CW/scaled_powerusage_10.xlsx")

# # For 9th Hour and 10th Hour 
# (Note: Run the both file individually to get the input as 9th hour and 10th hour)
# powerOutage <- read_xlsx("C:/Users/RANDUL/Desktop/2nd Semester of 2nd Year/5DATA001C.2 Machine Learning and Data Mining/CW/powerUsage-scaled-09.xlsx")
# powerOutage <- read_xlsx("C:/Users/RANDUL/Desktop/2nd Semester of 2nd Year/5DATA001C.2 Machine Learning and Data Mining/CW/powerUsage-scaled-10.xlsx")

# Split Data
set.seed(123)
splitRule <- sample(seq_len(nrow(powerOutage)), size = 430)
train <- powerOutage[splitRule, ]
test <- powerOutage[-splitRule, ]

# Define X values of test data
x_test <- test[-1]

# Define Y values of test data
y_test <- test[1]

# MLP NN - Configuration 1
relation1 <- as.formula("wineD_original_data~v2+v3+v4+v5+v6")
mlp1 <- neuralnet(formula = relation1, data = train,hidden = c(4,3), stepmax = 1e+10)
plot(mlp1)

# Make prediction on X test data
y_pred1 <- neuralnet::compute(mlp1, x_test)
evaluationData1 <- predictedVsreal(y_test, y_pred1)
mlpTestResult1 <- evaluationFunction(real = evaluationData1$`Expected Output`, predict = evaluationData1$`NeuralNetwork Output`)

# MLP NN - Configuration 2
relation2 <- as.formula("wineD_original_data~v2+v3+v4+v5+v6")
mlp2 <- neuralnet(formula = relation2, data = train,hidden = c(4,3), stepmax = 1e+10, learningrate = 0.001)
plot(mlp2)

# Make prediction on X test data
y_pred2 <- neuralnet::compute(mlp2, x_test)
evaluationData2 <- predictedVsreal(y_test, y_pred2)
mlpTestResult2 <- evaluationFunction(real = evaluationData2$`Expected Output`, predict = evaluationData2$`NeuralNetwork Output`)

# MLP NN - Configuration 3
relation3 <- as.formula("wineD_original_data~v2+v3+v4+v5+v6")
mlp3 <- neuralnet(formula = relation3, data = train,hidden = c(3,2), stepmax = 1e+10)
plot(mlp3)

# Make prediction on X test data
y_pred3 <- neuralnet::compute(mlp3, x_test)
evaluationData3 <- predictedVsreal(y_test, y_pred3)
mlpTestResult3 <- evaluationFunction(real = evaluationData3$`Expected Output`, predict = evaluationData3$`NeuralNetwork Output`)

# MLP NN - Configuration 4
relation4 <- as.formula("wineD_original_data~v2+v3+v4+v5+v6")
mlp4 <- neuralnet(formula = relation4, data = train,hidden = c(5,2), stepmax = 1e+10, learningrate = 0.001)
plot(mlp4)

# Make prediction on X test data
y_pred4 <- neuralnet::compute(mlp4, x_test)
evaluationData4 <- predictedVsreal(y_test, y_pred4)
mlpTestResult4 <- evaluationFunction(real = evaluationData4$`Expected Output`, predict = evaluationData4$`NeuralNetwork Output`)

# MLP NN - Configuration 5
train0 <- train[1:4]
relation5 <- as.formula("wineD_original_data~v2+v3+v4")
mlp5 <- neuralnet(formula = relation5, data = train0,hidden = c(4,3), stepmax = 1e+10, learningrate = 0.001)
plot(mlp5)

x_test0 <- test[2:4]

# Make prediction on X test data
# (Note: First try with configuration 1 and run this 98:100 and then 2, 3, 4, 5)
y_pred5 <- neuralnet::compute(mlp5, x_test0)
evaluationData5 <- predictedVsreal(y_test, y_pred5)
mlpTestResult5 <- evaluationFunction(real = evaluationData5$`Expected Output`, predict = evaluationData5$`NeuralNetwork Output`)

comp <- rbind(mlpTestResult1, mlpTestResult2, mlpTestResult3, mlpTestResult4, mlpTestResult5)
colnames(comp) <- c("RMSE","MAE","MAPE")
rownames(comp) <- c("config-1","config-2","config-3","config-4","config-5")
comp

write.xlsx(comp,
           file = "C:/Users/RANDUL/Desktop/2nd Semester of 2nd Year/5DATA001C.2 Machine Learning and Data Mining/CW/comparison-powerusage.xlsx"
           ,col.names = TRUE, append = TRUE, row.names = TRUE)