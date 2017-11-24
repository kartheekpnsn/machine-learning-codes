grid_search_tuning = function(X, Y, X_test = NULL, Y_test = NULL, hyperparameter_grid = NULL, 
						gridsearch_params = list(cv = 5, eval_metric = 'error'), 
						verbose = TRUE, plotting = TRUE, regression = FALSE,
						multi_class = FALSE, early_stopping_rounds = NULL) {
	# # Parameters # #
	# # X = Training independent features (DataFrame/DataTable)
	# # Y = Training target vector
	# # X_test = Testing independent features (DataFrame/DataTable)
	# # Y_test = Testing target vector
	# #If X_test = NULL AND Y_test = NULL:
	# 	- Internally it splits X, Y into Training and Testing (75-25 split)
	# # hyperparameter_grid : list of lists - [[Grid_Dictionary_1], [Grid_Dictionary_2], ...]
	# 		Grid_Dictionary_i = [key_of_hyperparameter : [list of values to try]]
	# 		Eg: hyperparameter_grid = list(list(eta = c(0.1, 0.2, 0.3), nrounds = c(50, 75, 100)),
	#										list(max_depth = c(3, 4), min_child_weight = c(1, 2)),
	#										list(colsample_bylevel = c(0.7, 0.8, 0.9), subsample = c(0.5, 0.6, 0.7)),
	#										list(scale_pos_weight = c(1, 2, 3, 4)))
	# # gridsearch_params : additional gridsearch parameters
	# 		Usually has cv rounds, eval_metric: Could take values like 'auc', 'rmse', 'logloss', 'error', 'mlogloss'
	# # verbose : bool, default = TRUE
	# 		Printing Gridsearch Results to Screen
	# # plotting : bool, default = TRUE
	#		Plotting Gridsearch Results
	# # regression : bool, default = FALSE
	#		Whether the data belongs to a classification or regression problem
	# # multi_class : bool, default = FALSE
	#		Whether the data is a multi-class classification or not
	# # early_stopping_rounds : integer, default = NULL
	#		Number of rounds to do early stopping
	# # End Parameters # #

	if(is.null(hyperparameter_grid)) {
		cat("> Not a valid Parameter value: hyperparameter_grid\n")
		cat("> Example is given below:\n")
		cat("> hyperparameter_grid = list(list(eta = c(0.1, 0.2, 0.3), nrounds = c(50, 75, 100)),\n\t\tlist(max_depth = c(3, 4), min_child_weight = c(1, 2))\n")
		cat('> Choosing default value\n')
		hyperparameter_grid = list(list(eta = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3), nrounds = seq(100, 500, 100)), list(max_depth = seq(3, 11, 2), min_child_weight = seq(1, 7, 2)), list(subsample = seq(0.6, 0.9, 0.1), colsample_bytree = seq(0.6, 0.9, 0.1)))
	} else if(!all(unlist(lapply(hyperparameter_grid, is.list)))) {
		cat("> Not a valid Parameter value: hyperparameter_grid\n")
		cat("> Example is given below:\n")
		cat("> hyperparameter_grid = list(list(eta = c(0.1, 0.2, 0.3), nrounds = c(50, 75, 100)),\n\t\tlist(max_depth = c(3, 4), min_child_weight = c(1, 2))\n")
		stop("# # # #\n")
	}

	if(!is.list(gridsearch_params)) {
		cat('> Not a valid Parameter value: gridsearch_params\n')
		cat('> gridsearch_params needs to be a list\n')
		# stop('# # # #\n')
		cat('> Taking default values:\n')
		gridsearch_params = list(cv = 5, eval_metric = 'error')
	}

	if(!names(gridsearch_params) %in% c('cv')) {
		gridsearch_params[['cv']] = 5
	}

	if(!names(gridsearch_params) %in% c('eval_metric')) {
		gridsearch_params[['eval_metric']] = 'logloss'
	}

	if(!gridsearch_params[['eval_metric']] %in% c('auc', 'rmse', 'logloss', 'error', 'mlogloss')) {
		cat('> Not a valid Parameter value: gridsearch_params\n')
		cat("> Can take values of 'auc', 'rmse', 'logloss', 'error', 'mlogloss'\n")
		# stop('# # # #\n')
		cat('> Taking default values:\n')
		gridsearch_params[['eval_metric']] = 'logloss'
	}

	eval_metric = gridsearch_params[['eval_metric']]

	required_packages = c('Matrix', 'xgboost', 'caret', 'MLmetrics', 'InformationValue', 'ggplot2', 'gridExtra')
	cat('==> Loading Required Libraries\n')
	if(any(required_packages %in% rownames(installed.packages())) == FALSE) {
		stop('> To run this we need the following packages: \n', paste(required_packages, collapse = '\n'))
	}

	library(Matrix)
	library(xgboost)
	library(caret)
	library(MLmetrics)
	library(InformationValue)
	library(ggplot2)
	library(gridExtra)
	cat('Done <==\n')

	objective = 'binary:logistic'
	eval_metric = gridsearch_params[['eval_metric']]

	# # data split # #
	if(is.null(Y_test) | is.null(X_test)) {
		cat('==> Starting data split into train and test\n')
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
				X_train = X[index$train, ]
				Y_train = Y[index$train]
				X_test = X[-index$train, ]
				Y_test = Y[-index$train]
			}
		cat('Done <==\n')
	} else {
		X_train = X
		Y_train = Y
		if(!regression){
			Y_test = as.numeric(as.factor(Y_test)) - 1
		}
	}

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
		eval_metric = "merror"
		objective = "multi:softmax"
		num_class = length(unique(Y))
	}

	cat('==> Choosing Extra Params\n')
	extra_params = list(verbose = verbose,
		objective = objective,
		eval_metric = eval_metric,
		showsd = TRUE,
		silent = 1
	)

	if(multi_class) {
		extra_params = c(extra_params, list(num_class = num_class))
	}
	cat('Done <==\n')

	# # start the grid search
	cat('==> Starting Grid Search\n')
	ct = 1 # maintain count
	for(hyper_params in hyperparameter_grid) {
		cat(paste0('\t==> Round:', ct, '\n'))
		grid_params = do.call('expand.grid', hyper_params)
		grid_metric = apply(grid_params, 1, function(eachRow) {
			cat('Starting ================================>\n')
			eachRow = as.list(eachRow)
			params = c(eachRow, extra_params)
			if('nrounds' %in% names(params)) {
				nrounds = params[['nrounds']]
			} else {
				nrounds = 10000
				early_stopping_rounds = 10
			}
			xgb_cv_fit = xgb.cv(data = dtrain, params = params, nfold = gridsearch_params[['cv']], nrounds = nrounds, print_every_n = 10, watchlist = watchlist, metrics = eval_metric, early_stopping_rounds = early_stopping_rounds)
			metric = tail(xgb_cv_fit$evaluation_log, 1)
			metric = data.table(metric[[2]], metric[[3]], metric[[4]], metric[[5]])
			metric = cbind(do.call('cbind', eachRow), metric)
			colnames(metric)[(ncol(grid_params) + 1) : ncol(metric)] = paste(c('train', 'train_sd', 'test', 'test_sd'), eval_metric, sep = '_')
			cat('Ending <================================\n')
			metric
		})
		grid_metric = do.call('rbind', grid_metric)
		if(plotting) {
			plot_data = copy(grid_metric)
			if(ncol(plot_data) == 6) {
				plot_data[[1]] = factor(plot_data[[1]])
				plot_data[[2]] = factor(plot_data[[2]])
				ylim = c(min(c(plot_data[[3]], plot_data[[5]])) * (1 - 0.05), max(c(plot_data[[3]], plot_data[[5]])) * 1.05)
				p1 = ggplot() +
						geom_line(data = plot_data, aes_string(x = colnames(plot_data)[2], y = colnames(plot_data)[3], color = colnames(plot_data)[1], group = colnames(plot_data)[1])) +
						geom_point(data = plot_data, aes_string(x = colnames(plot_data)[2], y = colnames(plot_data)[3], color = colnames(plot_data)[1], group = colnames(plot_data)[1])) +
						xlab(colnames(plot_data)[1]) + ylab(eval_metric) + ggtitle('Train') + ylim(ylim)
				p2 = ggplot() +
						geom_line(data = plot_data, aes_string(x = colnames(plot_data)[2], y = colnames(plot_data)[5], color = colnames(plot_data)[1], group = colnames(plot_data)[1])) +
						geom_point(data = plot_data, aes_string(x = colnames(plot_data)[2], y = colnames(plot_data)[5], color = colnames(plot_data)[1], group = colnames(plot_data)[1])) +
						xlab(colnames(plot_data)[1]) + ylab(eval_metric) + ggtitle('Test') + ylim(ylim)
				p = arrangeGrob(grobs = list(p1, p2), ncol = 2)
				p
			} else {
				plot_data[[1]] = factor(plot_data[[1]])
				ylim = c(min(c(plot_data[[2]], plot_data[[4]])) * (1 - 0.05), max(c(plot_data[[2]], plot_data[[4]])) * 1.05)
				p1 = ggplot() +
						geom_line(data = plot_data, aes_string(x = colnames(plot_data)[1], y = colnames(plot_data)[2])) +
						geom_point(data = plot_data, aes_string(x = colnames(plot_data)[1], y = colnames(plot_data)[2])) +
						xlab(colnames(plot_data)[1]) + ylab(eval_metric) + ggtitle('Train') + ylim(ylim)
				p2 = ggplot() +
						geom_line(data = plot_data, aes_string(x = colnames(plot_data)[1], y = colnames(plot_data)[4])) +
						geom_point(data = plot_data, aes_string(x = colnames(plot_data)[1], y = colnames(plot_data)[4])) +
						xlab(colnames(plot_data)[1]) + ylab(eval_metric) + ggtitle('Test') + ylim(ylim)
				p = arrangeGrob(grobs = list(p1, p2), ncol = 2)
				p
			}
			x11()
		}
		extra_params = c(extra_params, as.list(grid_metric[which.min(grid_metric[[paste0('test', '_', eval_metric)]]), colnames(grid_params), with = FALSE]))
		cat('\tDone<==')
	}
	cat('Done <==\n')
	return(extra_params)
}
