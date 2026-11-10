# Banknote data: 4 wavelet features + fraud label (0/1)
# File has no header row (header = FALSE), so columns are named below
data <- read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/banknoteFraud.csv", header = FALSE, sep = ",")
names(data) <- c("varWave", "skewWave", "kurtWave", "entropyWave", "fraud")
# Factor response -> gausspr does classification (not regression)
#data[, 5] <- as.factor(data[, 5])
# Given split: seed 111, 1000 random rows for training, rest (372) test
data <- data[,c("varWave","fraud")]
data$fraud <- as.factor(data$fraud)
count <- nrow(data)
idx <- sample(1:count, floor(count*0.8))
train <-data[idx,]
test <- data[-idx,]
library(kernlab)

#KERNLAB STYLE KERNEL
SEkernelKL <- function(sigmaf = 1, ell = 1) {
  rval <- function(x, y = NULL) {
    # Euclidean distance, works for vector inputs too
    r <- sqrt(sum((x - y)**2))
    return(sigmaf**2 * exp(-(r**2 / (2 * ell**2))))
  }
  # Class "kernel" makes gausspr/kernelMatrix accept it as a kernel
  class(rval) <- "kernel"
  return(rval)
}

# Calculate Gradient
ComputeGradient <-function(g, X, epsilon=0.01){
  g_1 <- g(X + epsilon)
  g_2 <- g(X - epsilon)
  return((g_1 - g_2)/(2*epsilon))
}

UpdateParameters <- function(parameters, loss, gradient, learning_rate=0.01){
  parameters <- parameters - learning_rate*gradient
  return(parameters)
}

# Calculate Loss function
NegativeLogLikelihood <- function(y_pred, y){
  n <- length(y_pred)
  loss <- -sum(y*log(y_pred)+(1-y)*log(1-y_pred))/n
  return(loss)
}

performGP <- function(K){
  kern <- rbfdot(sigma = 1 / (2 * exp(2*K)))
  GPfitFraud <- gausspr(fraud ~ varWave, data = train, kernel=kern, type="classification")
  pred_test <- predict(GPfitFraud, test, type="probabilities")[,2]
  loss <- NegativeLogLikelihood(pred_test, as.numeric(as.character(test$fraud)))
  return(loss)
}  




kern <- SEkernelKL(sigmaf = 1, ell = 1)

set.seed(12345)

l=1
K = log(l)
# g has to be a function g(ell) -> loss
# g(w) -> set kernel -> perform GP with kernel -> predict -> call error -> spit out


best_K <- K
lowest_err <- Inf


for(i in 1:5){
  if(i > 1){
    lowest_err <- min(lowest_err, loss)
    if(loss == lowest_err){
      best_K = K
    }
  }
  loss <- performGP(K)
  gradient <- ComputeGradient(performGP, K)
  K <- UpdateParameters(K, gradient, 0.1)
}


# kern <- rbfdot(sigma = 1 / (2 * l**2))
# GPfitFraud <- gausspr(fraud ~ varWave, data = train, kernel=kern, type="classification")
# pred_test <- predict(GPfitFraud, test, type="probabilities")[,2]
# loss <- NegativeLogLikelihood(pred_test, as.numeric(test$fraud))
# grad <- ComputeGradient()


# 
# 
# SelectTraining <- sample(1:dim(data)[1], size = 1000, replace = FALSE)
# 
# cm_train <- table(actual = train$fraud, predicted = pred_train)
# # Accuracy = correct (diagonal) / total
# accuracy_train <- sum(diag(cm_train)) / sum(cm_train)
# cm_train
# accuracy_train
# # 100 x 100 grid over the range of the two covariates
# x1 <- seq(min(train$varWave), max(train$varWave), length = 100)
# x2 <- seq(min(train$skewWave), max(train$skewWave), length = 100)
# # expand.grid varies x1 fastest; column names must match the formula