# # Parameters:
# X = Training independent features (DataFrame/DataTable)
# Y = Training target vector
# X_test = Testing independent features (DataFrame/DataTable)
# Y_test = Testing target vector
# If X_test = NULL AND Y_test = NULL:
# 	- Internally it splits X, Y into Training and Testing (75-25 split)
# If hyper_params = NULL
# 	- It loops all possibilities
# 	- This kills your machine
# Else
# 	- Example of hyper_params = expand.grid(nrounds = c(100, 200), max_depth = c(2, 10))
# If extra_params = NULL
# 	- It takes the below things:
# 			- verbose = FALSE
# 			- objective = objective
# 			- eval_metric = eval_metric
# 			- showsd = TRUE
# Else
# 	- Example of extra_params = list(verbose = FALSE, objective = 'logloss', eval_metric = 'binary:logistic')
# verbose = Flag indicating Whether to print what Parameters are getting tried or not
# multi_class = Flag indicating Whether the problem is of multi_class or not
# regression = Flag indicating Whether the problem is of regression problem or not
# eval_metric = What eval_metric to be chosen in xgboost
# nfold = Number of folds of CV

# # Returns:
# parameters tried
# evaluation metric on test set

source('https://raw.githubusercontent.com/kartheekpnsn/machine-learning-codes/master/R/functions.R')

xgb_tune = function(X, Y, X_test = NULL, Y_test = NULL, hyper_params = NULL, extra_params = NULL, verbose = TRUE, multi_class = FALSE, regression = FALSE, eval_metric = 'logloss', nfold = 5) {
	required_packages = c('Matrix', 'xgboost', 'caret', 'MLmetrics', 'InformationValue')
	cat('==> Loading Required Libraries\n')
	if(any(required_packages %in% rownames(installed.packages())) == FALSE) {
		stop('> To run this we need the following packages: \n', paste(required_packages, collapse = '\n'))
	}
	library(Matrix)
	library(xgboost)
	library(caret)
	library(MLmetrics)
	library(InformationValue)
	cat('Done <==\n')

	objective = 'binary:logistic'

	cat('==> Starting data split into train and test\n')
	# # data split # #
	if(is.null(Y_test) | is.null(X_test)) {
		if(regression) {
				eval_metric = 'rmse'
				objective = 'reg:linear'
				index = 1:length(Y)
				index = sample(index, round(length(Y) * 0.75))
				X_train = X[index, ]
				Y_train = Y[index]
				X_test = X[-index, ]
				Y_test = Y[-index]
			} else {
				# # convert Y to numeric # #
				Y = as.numeric(as.factor(Y)) - 1
				index = dataSplit(Y, split_ratio = c(0.75))
				# index = createDataPartition(Y, p = 0.75, list = FALSE)
				X_train = X[index$train, ]
				Y_train = Y[index$train]
				X_test = X[-index$train, ]
				Y_test = Y[-index$train]
			}
	
	} else {
		X_train = X
		Y_train = Y
		if(!regression){
			Y_test = as.numeric(as.factor(Y_test)) - 1
		}
	}
	cat('Done <==\n')

	# # data preparation - one hot encoding # #
	cat('==> Starting One hot encoding\n')
	train_matrix = sparse.model.matrix( ~. -1, data = X_train)
	test_matrix = sparse.model.matrix( ~. -1, data = X_test)
	cat('Done <==\n')

	# # form xgb DMatrix # #
	cat('==> Forming XGB DMatrix\n')
	options(na.action = 'na.pass')
	dtrain = xgb.DMatrix(data = as.matrix(train_matrix), label = Y_train)
	dtest = xgb.DMatrix(data = as.matrix(test_matrix), label = Y_test)
	cat('Done <==\n')

	# # form watch list # #
	cat('==> Forming Watchlist\n')
	watchlist = list(train = dtrain, eval = dtest)
	cat('Done <==\n')

	if(multi_class) {
		eval_metric = "mlogloss"
		objective = "multi:softprob"
		num_class = length(unique(Y))
	}

	cat('==> Forming Hyper parameters grid\n')
	if(is.null(hyper_params)) {
		hyper_params = expand.grid(nrounds = c(50, 100, 200),
							eta = c(0.1, 0.2, 0.3),
							max_depth = c(2, 3, 4),
							min_child_weight = c(4, 5),
							colsample_bylevel = c(0.7, 0.8, 0.9),
							subsample = c(0.5, 0.6, 0.7, 1),
							colsample_bytree = c(0.5, 0.75, 1),
							scale_pos_weight = c(1, 2, 5))
	}
	if(is.null(extra_params)) {
		extra_params = list(verbose = FALSE,
			objective = objective,
			eval_metric = eval_metric,
			showsd = TRUE
		)
	}
	if(multi_class) {
		extra_params = c(extra_params, list(num_class = num_class))
	}
	cat('Done <==\n')
	cat('==> Starting Grid Search\n')
	grid_metric = apply(hyper_params, 1, function(eachRow) {
		params = c(as.list(eachRow), extra_params)
		if('nrounds' %in% names(params)) {
			nrounds = params[['nrounds']]
		} else {
			nrounds = 100
		}
		if(verbose) {
			cat('> Running for:')
			verbose_print = params
			verbose_print$watchlist = NULL
			cat(data.table(do.call('cbind', verbose_print)))
		}
		xgb_fit = xgb.train(data = dtrain, params = params, nfold = nfold, nrounds = nrounds, print_every_n = 10, watchlist = watchlist)
		predicted = predict(xgb_fit, dtest)
		if(regression) {
			return(sqrt(sum((Y_test - predicted) ** 2)/length(predicted)))
		} else if(multi_class) {
			predicted = predict(xgb_fit, dtest)
			predicted = data.table(matrix(predicted, ncol = num_class, byrow = T))
			return(MultiLogLoss(predicted, Y_test))
		} else {
			predicted = predict(xgb_fit, dtest)
			return(performance_measure(predicted = predicted, actual = Y_test, metric = 'fscore', beta = 5))
		}
	})
	cat('Done <==\n')
	return(list(params = hyper_params, metric = grid_metric))
}

# # Similarly once tuned - send the tuned parameters to xgb_fit # #
xgb_train = function(X, Y, X_test = NULL, Y_test = NULL, hyper_params = NULL, extra_params = NULL, multi_class = FALSE, regression = FALSE, eval_metric = 'logloss', nfold = 5, cv = TRUE) {
        required_packages = c('Matrix', 'xgboost', 'caret', 'MLmetrics', 'InformationValue')
        if(any(required_packages %in% rownames(installed.packages())) == FALSE) {
                stop('> To run this we need the following packages: \n', paste(required_packages, collapse = '\n'))
        }
        cat('==> Loading libraries\n')
        library(Matrix)
        library(xgboost)
        library(caret)
        library(MLmetrics)
        library(InformationValue)
        cat('Done <==\n')
        objective = 'binary:logistic'

        # # convert Y to numeric # #
        cat('==> Choosing Eval Metric and Objective\n')
        if(regression) {
                eval_metric = 'rmse'
                objective = 'reg:linear'
        } else {
        	Y = as.numeric(as.factor(Y)) - 1
        }
        cat('Done <==\n')
        # # data split # #
        cat('==> Data Split into train and test\n')
        if(is.null(Y_test) | is.null(X_test)) {
                if(regression) {
                                index = 1:length(Y)
                                index = sample(index, round(length(Y) * 0.75))
								X_train = X[index, ]
                				Y_train = Y[index]
                				X_test = X[-index, ]
                				Y_test = Y[-index]

                        } else {
                                index = dataSplit(Y, split_ratio = c(0.75))
                                # index = createDataPartition(Y, p = 0.75, list = FALSE)
                				X_train = X[index$train, ]
                				Y_train = Y[index$train]
                				X_test = X[-index$train, ]
                				Y_test = Y[-index$train]
                        }
        
        } else {
                X_train = X
                Y_train = Y
                Y_test = as.numeric(as.factor(Y_test)) - 1
        }
        cat('Done <==\n')
        # # data preparation - one hot encoding # #
        cat('==> One hot encoding\n')
        train_matrix = sparse.model.matrix( ~. -1, data = X_train)
        test_matrix = sparse.model.matrix( ~. -1, data = X_test)
        cat('Done <==\n')

        cat('==> Form DMatrix\n')
        # # form xgb DMatrix # #
        options(na.action = 'na.pass')
        dtrain = xgb.DMatrix(data = as.matrix(train_matrix), label = Y_train)
        dtest = xgb.DMatrix(data = as.matrix(test_matrix), label = Y_test)
        cat('Done <==\n')

        # # form watch list # #
        cat('==> Form Watch List\n')
        watchlist = list(train = dtrain, eval = dtest)
        cat('Done <==\n')

        if(multi_class) {
                eval_metric = "mlogloss"
                objective = "multi:softprob"
                num_class = length(unique(Y))
        }

        cat('==> Choosing Extra Params\n')
        if(is.null(extra_params)) {
                extra_params = list(verbose = FALSE,
                        objective = objective,
                        eval_metric = eval_metric,
                        showsd = TRUE
                )
        }
        if(multi_class) {
                extra_params = c(extra_params, list(num_class = num_class))
        }
        cat('Done <==\n')

        cat('==> Choosing hyper params\n')
        params = c(hyper_params, extra_params)
        if('nrounds' %in% names(params)) {
                nrounds = params[['nrounds']]
        } else {
                nrounds = 100
        }
        cat('Done <==\n')
        if(cv) {
        		cat('\t ==> Running XGB CV\n')
                xgb_cv_fit = xgb.cv(data = dtrain, params = params, nfold = nfold, nrounds = nrounds, print_every_n = 10, watchlist = watchlist)
                cat('\t Done <==\n')
        }
        cat('==> Fitting XGB Train\n')
        xgb_fit = xgb.train(data = dtrain, params = params, nrounds = nrounds, print_every_n = 10, watchlist = watchlist)
        cat('<== Done\n')

        cat('==> Predict on local test\n')
        predicted = predict(xgb_fit, dtest)
        cat('Done <==\n')

        cat('==> Calculating Metrics\n')
        if(regression) {
                metric = (sqrt(sum((Y_test - predicted) ** 2)/length(predicted)))
        } else if(multi_class) {
                predicted = predict(xgb_fit, dtest)
                predicted = data.table(matrix(predicted, ncol = num_class, byrow = T))
                metric = (MultiLogLoss(predicted, Y_test))
        } else {
                predicted = predict(xgb_fit, dtest)
                metric = (performance_measure(predicted = predicted, actual = Y_test, metric = 'all', beta = 5))
        }
        cat('Done <==\n')
        cat('Done with XGBOOST <==\n')
        return(list(fit = xgb_fit, metric = metric))
}

# # Similarly once fitted - send the fitted model and new data to xgb_predict # #
xgb_predict = function(fit, newdata, multi_class = FALSE, class_names) {
	cat('===> Started Prediction\n')
	cat('==> Converting test data into DMatrix\n')
	test_matrix = sparse.model.matrix( ~. -1, data = newdata)
	options(na.action = 'na.pass')
	dtest = xgb.DMatrix(data = as.matrix(test_matrix))
	cat('Done <==\n')
	cat('==> Predicting\n')
	predicted = predict(fit, dtest)
	if(multi_class) {
		predicted = data.table(matrix(predicted, ncol = length(class_names), byrow = T))
		names(predicted) = class_names
	}
	cat('Done with Prediction <===\n')
	return(predicted)
}
