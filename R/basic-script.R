# # set path
setwd('SET SOME PATH')

# # libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(xgboost)
library(ranger)
library(rpart)
library(rpart.plot)
library(GGally)

# # functions
source('https://raw.githubusercontent.com/kartheekpnsn/machine-learning-codes/master/R/functions.R')
source('https://raw.githubusercontent.com/kartheekpnsn/machine-learning-codes/master/R/xgb_tune.R')

# # set seed
set.seed(294056)

# # load data
data = data.table(A = runif(1000), B = runif(1000) * runif(1000) * 10, C = factor(sample(letters, 1000, replace = TRUE)))
data[, target := ifelse(
					(A < 0.5 & B < 5) |
					(C %in% c('b', 'o', 'y') & A > 0.8) |
					(C %in% c('g', 'i', 'r', 'l') & B > 8), 
						1, 0)]

# # dependent column name
dependent = 'Y'
setnames(data, dependent, 'target')

# # split into independent and dependent variables
X = data[, !c('target'), with = FALSE]
Y = data[, target]

# # basic checks
str(X)
dim(X)
head(X)

# # check for class balance
prop.table(table(Y))

# # if imbalance - balance?
# data = balanceClasses(data, target = 'target', how = 'smote', over = 500, under = 200)

# # check for missing values
data.frame(flag = apply(X, 2, function(x) any(is.na(x))))

# # drop constant columns - if any
X = drop_const_cols(X)

# # remove outliers - if any
X = remove_outliers(X)

# # EDA
imp_matrix = importantFeatures(X, Y)
plot_data(X, Y)
print(imp_matrix)

# # feature engineering

# # again EDA
# imp_matrix = importantFeatures(X, Y)
# plot_data(X, Y)
# print(imp_matrix)

# # data splitting
index = dataSplit(Y, split_ratio = c(0.7), regression = FALSE)
train = X[index$train, ]
test = X[-index$train, ]
test_Y = Y[-index$train]
train_Y = Y[index$train]

# # model functions
# 1) glm
glm_data = cbind(train, target = train_Y)
glm_fit = glm(factor(target) ~ ., data = glm_data, family = 'binomial')
glm_pred = predict(glm_fit, newdata = test, type = 'response')
performance_measure(predicted = glm_pred, actual = test_Y, optimal_threshold = TRUE, how = 'accuracy', regression = FALSE)

# 2) decision tree
rpart_data = cbind(train, target = train_Y)
rpart_fit = rpart(factor(target) ~ ., data = rpart_data)
rpart.plot(rpart_fit)
rpart_pred = predict(rpart_fit, test)
performance_measure(predicted = rpart_pred[, 2], actual = test_Y, optimal_threshold = TRUE, how = 'accuracy', regression = FALSE)

# 2) random forest
rf_data = cbind(train, target = train_Y)
rf_fit = ranger(factor(target) ~ ., data = rf_data, probability = TRUE)
rf_pred = predict(rf_fit, test)$predictions
performance_measure(predicted = rf_pred[, 2], actual = test_Y, optimal_threshold = TRUE, how = 'accuracy', regression = FALSE)

# 3) xgboost ?
xgb_fit = xgb_train(X = train, Y = train_Y, X_test = test, Y_test = test_Y, multi_class = FALSE, regression = FALSE, cv = TRUE)
xgb_fit$metric

# # tuned - xgboost
xgb_fit = xgb_train(hyper_params = list(nrounds = 200, max_depth = 3, eta = 0.05), X = train, Y = train_Y, X_test = test, Y_test = test_Y, multi_class = FALSE, regression = FALSE, cv = FALSE)