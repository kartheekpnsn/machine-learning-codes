# # == Function for KMEANS to select optimal K == # #
# # Parameters
# dataset = dataset on which the algorithm needs to be applied
# maxK = What is the maximum K that needs to be tried (default = 10)
# threshold = covers 70% of the within sum of squares
# scale = a boolean flag to tell the function to normalize the data or not
# plot = A boolean value to tell whether to plot the elbow graphs or not (TRUE or FALSE)
optimalKMEANS = function(dataset = NULL, maxK = 10, threshold = 0.7, scale = FALSE, plot = TRUE) {
	if(scale) {
		mns = apply(dataset, 2, mean)
		sds = apply(dataset, 2, sd)
		dataset = scale(dataset)
	}
	if(is.null(dataset)) {
		stop("ERROR: THE DATASET IS NOT PROVIDED")
	}
	# # For K-Means
	# returns total within sum of squares
	kmeans.wss.k = function(dataset, k)
	{
	 km = kmeans(dataset, k)
	 return (km$tot.withinss)
	}
	# calculates within sum of squares for clusters 2 to maxK
	kmeans.dis = function(dataset, maxk)
	{
	 dis = (nrow(dataset) - 1) * sum(apply(dataset, 2, var))
	 # we neglect 1 because, we should not take 1 as number of clusters
	 dis[2:maxk] = sapply (2:maxk, kmeans.wss.k, dataset = dataset)
	 return(dis)
	}
	distance = kmeans.dis(dataset, maxK)
	if(plot) {
		plot(1:maxK, distance, type='b', xlab="Number of Clusters", ylab="Distortion", col="blue")
	}
	cumDistance = cumsum(distance/sum(distance)) # to get in %
	count = 1
	repeat {
		if(cumDistance[count] > threshold) { break; }
		count = count + 1
	}
	if(scale) {
		return(list(scale = list(means = mns, sd = sds), optimalK = count, withinss = distance))
	} else {
		return(list(optimalK = count, withinss = distance))
	}
}

# # == Function for KNN to select optimal K == # #
# # Parameters
# dataset = dataset on which the algorithm needs to be applied
# testset = testset to calculate test error
# target = name of the target variable
# maxK = What is the maximum K that needs to be tried (default = 10)
# scale = a boolean flag to tell the function to normalize the data or not
optimalKNN = function(dataset = NULL, testset = NULL, target = "Y", maxK = 10, scale = FALSE) {
	if(scale) {
		mns = apply(dataset, 2, mean)
		sds = apply(dataset, 2, sd)
		dataset = scale(dataset)
	}
	if(is.null(dataset)) {
		stop("ERROR: THE DATASET IS NOT PROVIDED")
	}

	# Change the target column name to "Y"
	colnames(dataset)[which(colnames(dataset) == target)] = "Y"

	# if testset not provided, create one from the main dataset
	if(is.null(testset)) {
		# install caret for balanced data partition into train and test
		if("caret" %in% rownames(installed.packages()) == FALSE) {
			install.packages("caret", repos = "http://cran.us.r-project.org/")
		}
		library(caret)
		index = createDataPartition(dataset$Y, p = 0.75, times = 1, list = FALSE)
		train = dataset[index, ]
		test = dataset[-index, ]
	} else {
		train = dataset
		test = testset
	}
	# # KNN
	if("class" %in% rownames(installed.packages()) == FALSE) {
		install.packages("class", repos = "http://cran.us.r-project.org/")
	}
	library(class)
	x = train[, setdiff(colnames(train), "Y")]
	y = as.factor(train$Y)

	x_test = test[, setdiff(colnames(test), "Y")]
	y_test = as.factor(test$Y)

	train_error = test_error = numeric(maxK)

	fit = list()
	for(i in 1:maxK) {
		trainFit = knn(x, x, y, k = i)
		train_error[i] = 1 - sum(y == trainFit) / length(y)
		
		testFit = knn(x, x_test, y, k=i)
		test_error[i] = 1 - sum(y_test == testFit) / length(y_test)
		fit[[i]] = trainFit
	}

	# give more weightage to test error than that of trainerror (to avoid overfitting)
	trainwt = 0.2
	testwt = 0.8
	fError = (train_error * trainwt) + (test_error * testwt)

	# the optimal K will be having less error in both train and test
	optimalK = which(fError == min(fError))

	if(scale) {
		return(list(scale = list(means = mns, sd = sds), optimalK = optimalK, fit = fit[[optimalK]]))
	} else {
		return(list(optimalK = optimalK, fit = fit[[optimalK]]))
	}
}

# # == Function to get cutoff by f-score and plotROC if needed == # #
# # Parameters
# probabilities = output probabilities thrown out of a model on the data
# original = original 2 class target values (either 0 or 1)
# beta = a trade off used to give account for precision or recall (more the beta value, more tradeoff given for recall)
# how = based on what metric the cutoff should be chosen
getCutoff = function(probabilities, original, beta = 1, how = 'accuracy') {
	if(!how %in% c('auc', 'accuracy', 'fscore', 'error')) {
		stop('> how parameter can have only below values:\nauc\naccuracy\nfscore\nlogloss\nerror')
	}
	print('==> Checking if given values are Class values or Probability Values')
	if(length(unique(probabilities)) < 3) {
		stop('> probabilities parameter only accepts probability values not class values')
	}
	print('Checks passed <==')

	print('==> Start checking all cutoffs')
	original = as.numeric(as.factor(original)) - 1
	cutoffs = seq(0, 1, 0.01)
	perf = c()
	for(eachCutoff in cutoffs) {
		predicted = as.numeric(probabilities >= eachCutoff)
		if(how == 'accuracy') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'accuracy', optimal_threshold = FALSE))
		} else if(how == 'auc') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'auc', optimal_threshold = FALSE))
		} else if(how == 'error') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'error', optimal_threshold = FALSE))
		} else if(how == 'fscore') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'fscore', optimal_threshold = FALSE))
		}
	}
	print('Done with checking <==')
	print('==> Returning the best cutoff')
	print('Done <==')
	if(how %in% c('error')) {
		return(cutoffs[which.min(perf)])
	} else {
		return(cutoffs[which.max(perf)])
	}
}

# # == Function to calculate similarity measures == # #
# # Parameters
# ob1 = object 1
# ob2 = object 2
# measure = measure that you want to use for calculating similarity [euc, jac, cos, pearson, man]
# # where:
# # # euc = euclidean distance [lesser the better]
# # # jac = jaccard distance [lesser the better]
# # # man = manhattan distance [lesser the better]
# # # cos = cosine similiarity measure [more the better (-1 to 1)]
# # # pearson = pearsons correlation coefficient [more the better (-1 to 1)]
similarityMeasure = function(ob1, ob2, measure = "pearson") {
	if(measure == "euc") { # euclidean distance - lesser the better
		return(sqrt(sum((ob1 - ob2)^2)))
	} else if(measure == "jac") { # jaccard distance - lesser the better
		# # intersect(ob1, ob2)/union(ob1, ob2) - Hamming score
		ob1 = which(ob1 != 0)
		ob2 = which(ob2 != 0)
		return(length(intersect(ob1, ob2))/ length(union(ob1, ob2)))
	} else if(measure == "man") { # manhattan distance - lesser the better
		return(sum(abs(a - b)))
	} else if(measure == "cos") { # cosine similarity measure
		numerator = sum(ob1 * ob2)
		denominator = sqrt(sum(ob1 * ob1)) * sqrt(sum(ob2 * ob2))
		return(numerator/denominator)
	} else if(measure == "pearson") { # pearsons correlation coefficient
		# cosine - mean
		ob1 = ob1 - mean(ob1)
		ob2 = ob2 - mean(ob2)
		return(similarityMeasure(ob1, ob2, measure = "cos"))
	}
}

# # == Function to calculate mode == # #
# # Parameters
# vector - an input categorical vector for which mode has to be found
mode = function(vector) {
	return(unique(vector)[which.max(table(vector))])
}

# # == Function to do min max normalization == # #
# # Parameters
# series = a vector of numbers that needs to be scaled
# new_min = a minimum value that the new series should have
# new_max = a maximum value that the new series should have
min_max_norm = function(series, new_min, new_max) {
	(((series - min(series))/(max(series) - min(series))) * (new_max - new_min)) + new_min
}
		
# # == Function to do class balancing == # #
# # Parameters
# data = data that is to be balanced
# target = depedent value based on which the data is to be balanced
# type = type of balancing to be done (either over/under/cost sensitive learning)
# seed = to ensure reproducability
balanceClasses = function(data, target = 'Y', type = 'over', seed = 294056) {
	if(target != 'Y') {
		colnames(data)[which(colnames(data) == target)] = 'Y'
	}
	if("ROSE" %in% rownames(installed.packages()) == FALSE) {
		install.packages("ROSE", repos = "http://cran.us.r-project.org/")
	}
	library(ROSE)
	n_class_values = table(data$Y)
	n_classes = length(n_class_values)
	if(type == 'over') {
		# # oversampling 
		balanced = ovun.sample(Y ~ ., 
			data = data, 
			method = "over", 
			N = round(max(n_class_values) + sum(max(n_class_values) * runif((n_classes - 1), 0.5, 0.8))), 
			seed = seed)$data
	} else if(type == 'under') {
		# # under sampling
		balanced = ovun.sample(Y ~ .,
			data = data,
			method = "under", 
			N = round(min(n_class_values) + sum(min(n_class_values) * runif((n_classes - 1), 1.2, 1.5))),
			seed = seed)$data
	} else if(type == 'csl') {
		# # cost sensitive learning
		balanced = ROSE(Y ~ ., 
			data = data,
			seed = seed)$data
	} else {
		cat("Use type as one of the following: 'over', 'under', 'csl'\n")
		cat("'over'\t:\tdoes oversampling\n")
		cat("'under'\t:\tdoes undersampling\n")
		cat("'csl'\t:\tdoes cost sensitive learning\n")
		stop()
	}
	return(balanced)
} 

# # == Function to do a data splitting into train, test and validation sets == # #
# # Parameters
# target = the original target values based on which the split to happen
# split_ratio = ratio of split of the data
# seed = for reproducability
dataSplit = function(target, split_ratio = c(0.7, 0.2), seed = 294056, regression = FALSE) {
	set.seed(seed)
	if(regression) {
		print('==> Starting split for regression')
		print('		Note: Since regression the split ratio is defaulted to 75:25')
		index = 1:length(target)
		index = sample(index, round(length(target) * 0.75))
		print('Done <==')
		return(index)
	}
	else {
		print('==> Starting split for classification')
		if(sum(split_ratio) == 1) {
			stop('Sum of split_ratio should be less than 1')
		}
		u_classes = unique(target)
		index = list()
		index = c(index, rep(1, length(split_ratio) + 1))
		for(eachClass in u_classes) {
			temp_index = which(target == eachClass)
			temp_index = sample(temp_index)
			lengths = cumsum(round(length(temp_index) * split_ratio))
			lengths = c(0, lengths, length(temp_index))
			for(each in 1:(length(lengths)-1)) {
				index[[each]] = c(index[[each]], temp_index[(lengths[each] + 1) : lengths[each + 1]])
			}
		}
		index = lapply(index, function(x) x[2:length(x)])
		if(length(split_ratio) + 1 == 3) {
			names(index) = c('train', 'valid', 'test')
			print('		==> Returning train, valid, test indexes')
		} else if(length(split_ratio) + 1 == 2) {
			names(index) = c('train', 'test')
			print('		==> Returning train, test indexes')
		}
		print('Done <==')
		return(index)
	}
}

# # == Function to calculate the performance measures for the prediction == # #
# # Parameters
# actual = actual values
# predicted = predicted probability values
# metric = what performance metric needs to be calculated
# optimal_threshold = whether to calculate the optimal threshold or not
# how = based on what metric the cutoff should be chosen
# regression = flag indicating whether it is a regression problem or not
# plotROC_flag = flag indicating whether to plot ROC flag or not
# beta = beta value for fbeta score, where if beta value is high - we expect more recall
performance_measure = function(predicted, actual, threshold = 0.5, metric = 'all', optimal_threshold = FALSE, how = 'fscore', regression = FALSE, plotROC_flag = FALSE, beta = 1) {
	print('==> Doing basic checks')
	if(length(predicted) != length(actual)) {
		stop('> Length of Predicted and Actual not matching')
	}
	print('Done (Passed) <==')
	if(regression) {
		print('==> Calculating performance measure for regression')
		mae = sum(abs(predicted - actual))/length(actual)
		mse = sum((predicted - actual) ** 2)/length(actual)
		rmse = sqrt(mse)
		print('Done <==')
		if(metric == 'mae') {
			return(rmse)
		} else if(metric == 'mse') {
			return(mse)
		} else if (metric == 'rmse') {
			return(mae)
		} else if(metric == 'all') {
			return(data.table(mae = mae, mse = mse, rmse = rmse))
		} else {
			stop('> For regression use metric as "rmse" (or) "mse" (or) "mae" (or) "all"')
		}
	} else {
		print('==> Calculating performance measure for classification')
		actual = as.numeric(as.factor(actual)) - 1
		if(plotROC_flag) {
			print('==> Plotting ROC Curve')
			library(InformationValue)
			if("InformationValue" %in% rownames(installed.packages()) == FALSE) {
				install.packages("InformationValue", repos = "http://cran.us.r-project.org/")
			}
			InformationValue::plotROC(actuals = actual, predictedScores = predicted)
			print('Done <==')
		}
		if(optimal_threshold) {
			print('		==> Getting optimal threshold')
			threshold = getCutoff(probabilities = predicted, original = actual, beta = beta, how = how)
			print(paste('> Threshold chosen:', threshold))
			print('		Done <==')
		}
		predicted_probabilities = predicted
		predicted = as.numeric(predicted >= threshold)
		accuracy = sum(predicted == actual)/length(actual)
		error = 1 - accuracy
		tp = length(which(predicted == 1 & actual == 1))
		fp = length(which(predicted == 1 & actual == 0))
		fn = length(which(predicted == 0 & actual == 1))
		precision = tp/(tp + fp)
		recall = tp/(tp + fn)
		fscore = ((1 + (beta ** 2)) * precision * recall)/(((beta ** 2) * precision) + recall)
		mse = sum((predicted_probabilities - actual) ** 2)/length(actual)
		mae = sum(abs(predicted_probabilities - actual))/length(actual)
		rmse = sqrt(mse)
		logloss = - sum((actual * log(predicted_probabilities)) + ((1 - actual) * log((1 - predicted_probabilities))))/length(actual)
		if("Metrics" %in% rownames(installed.packages()) == FALSE) {
			install.packages("Metrics", repos = "http://cran.us.r-project.org/")
		}
		library(Metrics)
		auc = Metrics::auc(predicted = predicted_probabilities, actual = actual)
		print('Done <==')
		if(metric == 'accuracy') {
			return(accuracy)
		} else if(metric == 'precision') {
			return(precision)
		} else if(metric == 'recall') {
			return(recall)
		} else if(metric == 'fscore') {
			return(fscore)
		} else if(metric == 'rmse') {
			return(rmse)
		} else if(metric == 'auc') {
			return(auc)
		} else if(metric == 'mse') {
			return(mse)
		} else if(metric == 'error') {
			return(error)
		} else if(metric == 'mae') {
			return(mae)
		} else if(metric == 'logloss') {
			return(logloss)
		} else if(metric == 'all') {
			metrics = data.table(accuracy = accuracy, precision = precision, recall = recall, fscore = fscore,
							auc = auc, error = error, logloss = logloss, mae = mae, mse = mse, rmse = rmse)
			return(metrics)
		} else {
			cat('> For classification use metric as:\n')
			cat('accuracy\nprecision\nrecall\nfscore\nauc\nerror\nlogloss\nrmse\nmse\nmae\nall\n')
			stop('> Invalid metric used')
		}
	}
}

# # == Function to calculate the important features == # #
# # Parameters
# X = independent features
# Y = target dependent variable
importantFeatures = function(X, Y) {
	print('==> Getting Numerical columns')
	numerics = colnames(X)[which(sapply(X, class) %in%  c('numeric', 'integer'))]
	print('Done <==')
	print('==> Getting Categorical columns')
	categoricals = setdiff(colnames(X), numerics)
	print('Done <==')

	if(length(categoricals) != 0) {
		# # Chi-Square Test for Categorical vs Target # #
		print('==> Starting Chi-Square test')
		df = copy(X)
		df = setDF(df)
		chisq = data.table(feature = character(), p_value = numeric())
		for(eachVar in categoricals){
			tab = table(df[, eachVar], Y)
			chi = chisq.test(tab)
			chisq = rbind(chisq, data.table(feature = eachVar, p_value = chi$p.value))
		}
		chisq[, significant := (p_value <= 0.05)]
		chisq[, p_value := round(p_value, 5)]
		print('Done with Chi-Square test')
	
		# # Weight of Evidence and Information Value # #
		print('==> Starting WOE and IV')
		if("InformationValue" %in% rownames(installed.packages()) == FALSE) {
			install.packages("InformationValue", repos = "http://cran.us.r-project.org/")
		}
		library(InformationValue)
		important_features = data.frame()
		for(eachVar in categoricals) {
			options(scipen = 999, digits = 4)
			IV = as.numeric(IV(X = X[[eachVar]], Y = Y))
			howgood = attr(IV(X = X[[eachVar]], Y = Y), 'howgood')
			important_features = rbind(important_features, data.frame(feature = eachVar, IV = IV, howgood = howgood))
		}
		important_features = setDT(important_features)
		setorder(important_features, -IV)
		print('Done with WOE and IV <==')
	} else {
		print('==> No Categorical Variables in the data')
	}

	# # ANOVA - For Continuous and Target variable # #
	print('==> Starting with ANOVA')
	anova = data.table(feature = character(), p_value = numeric())
	for(eachVar in numerics) {
		data = cbind(X, Y = Y)
		formula = as.formula(paste(eachVar, '~ Y'))
		aov_test = aov(formula, data = data)
		p_value = summary(aov_test)[[1]][["Pr(>F)"]][1]
		anova = rbind(anova, data.table(feature = eachVar, p_value = p_value))
	}
	anova[, significant := (p_value <= 0.05)]
	anova[, p_value := round(p_value, 5)]
	print('Done with ANOVA <==')

	print('==> Returning Outputs')
	print('Done <==')
	return(list(chisq = chisq, IV = important_features, anova = anova))
}

# # == Function to plot the data == # #
# # Parameters
# X = independent features (data.frame or data.table)
# Y = dependent target vector
# append = text to be appended at the end of the name of the plot while saving
plot_data = function(X, Y, append = 'plot') {
	if(any(class(X) != 'data.table')) {
		print('==> Converting X to Data.table')
		library(data.table)
		X = setDT(X)
		print('Done <==')
	}
	print('==> Creating plots directory')
	dir.create('plots', showWarnings = FALSE)
	print('Done <==')
	print('==> Getting Numerical columns')
	numerics = colnames(X)[which(sapply(X, class) %in% c('numeric', 'integer'))]
	print('Done <==')
	print('==> Getting Categorical columns')
	factors = colnames(Y)[which(sapply(X, class) %in% c('character', 'factor'))]
	print('Done <==')
	# # for numeric - plot box plot and histogram # #
	print('==> Running for Numerical columns')
	dir.create('plots\\continuous', showWarnings = FALSE)
	for(eachVar in numerics) {
		print(paste0('		==> For: ', eachVar))
		library(gridExtra)
		library(ggplot2)
		# # box plot # #
		subset_data = cbind(X[, eachVar, with = FALSE], Y = Y)
		setnames(subset_data, eachVar, 'Variable')
		subset_data[, Y := factor(Y)]
		print(paste0('				==> Box plot for: ', eachVar))
		p1 = ggplot(data = subset_data, aes(x = Y, y = Variable, color = Y)) + geom_boxplot(outlier.color = 'red', alpha = 0.3) +
				xlab('Target Variable') + ylab(eachVar) + ggtitle(paste0('Box plot for: ', eachVar))
		# compute lower and upper whiskers
		ylim1 = boxplot.stats(subset_data[, Variable])$stats[c(1, 5)]
		# scale y limits based on ylim1
		p2 = p1 + coord_cartesian(ylim = ylim1 * 1.05) + ggtitle(paste0('Box plot for: ', eachVar, ' (without Outliers)'))
		p = grid.arrange(grobs = list(p1, p2), ncol = 2)
		ggsave(paste0('plots\\continuous\\boxplot_', eachVar, '_', append, '.jpg'), p)
		print('						Done <==')
		# # histogram # #
		p = list()
		ct = 1
		colors = c('lightblue', 'lightgreen', 'coral', 'darkblue', 'darkgreen', 'darkred')
		print(paste0('				==> Histogram for: ', eachVar))
		for(eachLevel in unique(subset_data[, Y])) {
			p[[ct]] = ggplot() + geom_histogram(data = subset_data[Y == eachLevel], 
										aes(Variable), fill = colors[ct], color = 'grey') +
						xlab(eachVar) + ylab('Frequency') + ggtitle(paste0('For Y = ', eachLevel))
			ct = ct + 1
		}
		p = grid.arrange(grobs = p, ncol = 1)
		ggsave(paste0('plots\\continuous\\histogram_', eachVar, '_', append, '.jpg'), p)
		print('						Done <==')
	}
	print('Done with Numerical columns <==')
	# # for character - plot stacked barchart # #
	print('==> Running for Categorical columns')
	dir.create('plots\\categorical', showWarnings = FALSE)
	for(eachVar in factors) {
		if(length(unique(X[[eachVar]])) <= 20) {
			print(paste0('		==> For: ', eachVar))
			subset_data = cbind(X[, eachVar, with = FALSE], Y = Y)
			subset_data = subset_data[, .N, .(get(eachVar), Y)]
			subset_data[, Y := factor(Y)]
			subset_data$N = as.numeric(subset_data$N)
			eachLevel = unique(subset_data[, get])
			values = c()
			for(each in eachLevel) {
				values = length(X[[eachVar]][X[[eachVar]] == each])
				subset_data[get == each, N := (N/values)]
			}
			p = ggplot(data = subset_data, aes(x = get, y = N, fill = Y)) + 
					geom_bar(stat = 'identity') + xlab(eachVar)
			ggsave(paste0('plots\\categorical\\stacked_barchart_', eachVar, '_', append, '.jpg'), p)
			print(paste0('		Done <=='))
		}
	}
	print('Done with Categorical columns <==')
}


# # == Function to remove outliers from numerical independent vectors == # #
# # Parameters
# X = independent features (data.frame/data.table)
# Y = dependent target vector
# na.rm = Flag to indicate whether to remove NA or not
remove_outliers = function(X, Y, na.rm = TRUE) {
	if(any(class(X) != 'data.table')) {
		print('==> Converting X to Data.table')
		library(data.table)
		X = setDT(X)
		print('Done <==')
	}
	print('==> Getting numeric columns')
	numerics = colnames(X)[which(sapply(X, class) %in% c('numeric', 'integer'))]
	print(paste0('Numeric columns: ', paste0(numerics, collapse = ', ')))
	print('Done <==')
	for(eachVar in numerics) {
		print(paste0('==> Checking for: ', eachVar))
		for(eachClass in unique(Y)) {
			x = X[[eachVar]][Y == eachClass]
			qnt = quantile(x, probs = c(.25, .75), na.rm = na.rm)
			H = 1.5 * IQR(x, na.rm = na.rm)
			y = x
			if(length(X[[eachVar]][Y == eachClass][y < qnt[1] - H]) > 0) {
				print(paste0('Removing lower level outliers in: ', eachVar))
				X[[eachVar]][Y == eachClass][y < (qnt[1] - H)] = (qnt[1] - H)
			}
			if(length(X[[eachVar]][Y == eachClass][y > (qnt[2] + H)]) > 0) {
				print(paste0('Removing top level outliers in: ', eachVar))
				X[[eachVar]][Y == eachClass][y > (qnt[2] + H)] = (qnt[2] + H)
			}
		}
		print(paste0('Done with: ', eachVar, '<=='))
	}
	print('Done <==')
	return(X)
}
