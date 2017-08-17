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
	cat('==> Checking if given values are Class values or Probability Values\n')
	if(length(unique(probabilities)) < 3) {
		stop('> probabilities parameter only accepts probability values not class values')
	}
	cat('Checks passed <==\n')

	cat('==> Start checking all cutoffs\n')
	original = as.numeric(as.factor(original)) - 1
	cutoffs = seq(0, 1, 0.01)
	perf = c()
	for(eachCutoff in cutoffs) {
		predicted = as.numeric(probabilities >= eachCutoff)
		if(how == 'accuracy') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'accuracy', optimal_threshold = FALSE, verbose = FALSE))
		} else if(how == 'auc') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'auc', optimal_threshold = FALSE, verbose = FALSE))
		} else if(how == 'error') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'error', optimal_threshold = FALSE, verbose = FALSE))
		} else if(how == 'fscore') {
			perf = c(perf, performance_measure(predicted = predicted, actual = original, beta = beta, metric = 'fscore', optimal_threshold = FALSE, verbose = FALSE))
		}
	}
	cat('Done with checking <==\n')
	cat('==> Returning the best cutoff\n')
	cat('Done <==\n')
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
	vector = vector[!is.na(vector)]
	vector = vector[vector != '']
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
balanceClasses = function(data, target = 'Y', how = 'smote', seed = 294056, over = 500, under = 100) {
	set.seed(seed)
	library(DMwR)
	data[[target]] = as.factor(data[[target]])
	# # process
	# Say the data has 30 0's and 10 1's and over = 500 and under = 200
	# first it oversamples the minority class (i.e. 1's) to:
	#	1st Class SMOTE CASES: (10 * (over/100)) = 50
	#	ORIGINAL CASES: 10
	#	Total = 10 + (10 * (over/100)) = 60
	# Now there are 60 1's and 30 0's
	# second it perfoms this on the majority class (i.e. 0's) to:
	#	2nd Class SMOTE CASES: (under/100) * SMOTE CASES
	data = SMOTE(as.formula(paste0(target, ' ~ .')), data = data, perc.over = over, perc.under = under)
	return(data)
} 

# # == Function to do a data splitting into train, test and validation sets == # #
# # Parameters
# target = the original target values based on which the split to happen
# split_ratio = ratio of split of the data
# seed = for reproducability
dataSplit = function(target, split_ratio = c(0.7, 0.2), seed = 294056, regression = FALSE) {
	set.seed(seed)
	if(regression) {
		cat('==> Starting split for regression\n')
		cat('\t Note: Since regression the split ratio is defaulted to 75:25\n')
		index = 1:length(target)
		index = sample(index, round(length(target) * 0.75))
		cat('Done <==\n')
		return(index)
	}
	else {
		cat('==> Starting split for classification\n')
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
			cat('\t ==> Returning train, valid, test indexes\n')
		} else if(length(split_ratio) + 1 == 2) {
			names(index) = c('train', 'test')
			cat('\t ==> Returning train, test indexes\n')
		}
		cat('Done <==\n')
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
# verbose = whether to print messages or not
performance_measure = function(predicted, actual, threshold = 0.5, metric = 'all', optimal_threshold = FALSE, how = 'fscore', regression = FALSE, plotROC_flag = FALSE, beta = 1, verbose = TRUE) {
	if(verbose)
	cat('==> Doing basic checks\n')
	if(length(predicted) != length(actual)) {
		stop('> Length of Predicted and Actual not matching')
	}
	if(verbose)
	cat('Done (Passed) <==\n')
	if(regression) {
		if(verbose)
		cat('==> Calculating performance measure for regression\n')
		mae = sum(abs(predicted - actual))/length(actual)
		mse = sum((predicted - actual) ** 2)/length(actual)
		rmse = sqrt(mse)
		if(verbose)
		cat('Done <==\n')
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
		if(verbose)
		cat('==> Calculating performance measure for classification\n')
		actual = as.numeric(as.factor(actual)) - 1
		if(plotROC_flag) {
			if(verbose)
			cat('\t ==> Plotting ROC Curve\n')
			library(InformationValue)
			if("InformationValue" %in% rownames(installed.packages()) == FALSE) {
				install.packages("InformationValue", repos = "http://cran.us.r-project.org/")
			}
			InformationValue::plotROC(actuals = actual, predictedScores = predicted)
			if(verbose)
			cat('\t Done <==\n')
		}
		if(optimal_threshold) {
			if(verbose)
			cat('\t ==> Getting optimal threshold\n')
			threshold = getCutoff(probabilities = predicted, original = actual, beta = beta, how = how)
			# if(verbose)
			cat(paste('> Threshold chosen:', threshold, '\n'))
			if(verbose)
			cat('\t Done <==\n')
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
		if(verbose)
		cat('Done <==\n')
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
			if(verbose)
			cat('> For classification use metric as:\n')
			if(verbose)
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
	if(!any(class(X) == 'data.table')) {
		cat('==> Converting X to Data.table\n')
		library(data.table)
		X = setDT(X)
		cat('Done <==\n')
	}
	Y = as.numeric(as.factor(as.character(Y))) - 1
	cat('==> Getting Numerical columns\n')
	numerics = colnames(X)[which(sapply(X, class) %in%  c('numeric', 'integer'))]
	cat('Done <==\n')
	cat('==> Getting Categorical columns\n')
	categoricals = colnames(X)[which(sapply(X, class) %in%  c('factor', 'character'))]
	X[, (categoricals) := lapply(.SD, as.factor), .SDcols = categoricals]
	cat('Done <==\n')
	
	return_value = list()
	if(length(categoricals) != 0) {
		# # Chi-Square Test for Categorical vs Target # #
		cat('\t ==> Starting Chi-Square test\n')
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
		cat('\t Done with Chi-Square test <==\n')
	
		# # Weight of Evidence and Information Value # #
		cat('\t ==> Starting WOE and IV\n')
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
		cat('\t Done with WOE and IV <==\n')
		return_value = c(return_value, list(chisq = chisq, IV = important_features))
	} else {
		cat('\t ==> No Categorical Variables in the data\n')
	}

	# # ANOVA - For Continuous and Target variable # #
	cat('\t ==> Starting with ANOVA\n')
	anova = data.table(feature = character(), p_value = numeric())
	for(eachVar in numerics) {
		if(length(unique(X[[eachVar]])) == 1) {
			cat(paste0('\t \t Removing constant column: ', eachVar, '\n'))
		}
		data = cbind(X, Y = Y)
		formula = as.formula(paste(eachVar, '~ Y'))
		aov_test = aov(formula, data = data)
		p_value = summary(aov_test)[[1]][["Pr(>F)"]][1]
		anova = rbind(anova, data.table(feature = eachVar, p_value = p_value))
	}
	anova[, significant := (p_value <= 0.05)]
	anova[, p_value := round(p_value, 5)]
	cat('\t Done with ANOVA <==\n')

	cat('==> Returning Outputs\n')
	cat('Done <==\n')
	return_value = c(return_value, list(anova = anova))
	return(return_value)
}

# # == Function to drop constant columns in the data == # #
# # Parameters
# data = input data
drop_const_cols = function(data) {
	if(any(class(data) != 'data.table')) {
		cat('==> Converting data to Data.table\n')
		library(data.table)
		data = setDT(data)
		cat('Done <==\n')
	}

	flags = apply(data,  2 , function(x)
		all(x == x[1], na.rm = TRUE)
	)

	cat(paste0('==> Dropped ', length(which(flags)), ' columns\n'))
	
	if(length(which(flags)) > 0) {
		cat(paste0('==> Dropped: ', paste0(colnames(data)[which(flags)], collapse = ', ')), '\n')
		return(data[, !colnames(data)[which(flags)], with = FALSE])
	} else {
		return(data)
	}
}
			
# # == Function to plot the data == # #
# # Parameters
# X = independent features (data.frame or data.table)
# Y = dependent target vector
# append = text to be appended at the end of the name of the plot while saving
# # == Function to plot the data == # #
# # Parameters
# X = independent features (data.frame or data.table)
# Y = dependent target vector
# append = text to be appended at the end of the name of the plot while saving
plot_data = function(X, Y, append = 'plot', scatter_cols = 'all') {
	if(!any(class(X) == 'data.table')) {
		cat('==> Converting X to Data.table\n')
		library(data.table)
		X = setDT(X)
		cat('Done <==\n')
	}
	cat('\t ==> Creating plots directory\n')
	dir.create('plots', showWarnings = FALSE)
	cat('\t Done <==\n')
	cat('==> Getting Numerical columns\n')
	numerics = colnames(X)[which(sapply(X, class) %in% c('numeric', 'integer'))]
	cat('Done <==\n')
	cat('==> Getting Categorical columns\n')
	factors = colnames(X)[which(sapply(X, class) %in% c('character', 'factor'))]
	cat('Done <==\n')
	# # for numeric - plot box plot and histogram # #
	cat('\t ==> Running for Numerical columns\n')
	dir.create('plots\\continuous', showWarnings = FALSE)
	if(scatter_cols == 'numeric') {
		scatter_cols = numerics
	} else if(scatter_cols == 'factor') {
		scatter_cols = factors
	} else if(scatter_cols == 'all') {
		scatter_cols = colnames(X)
	}
	scatter_matrix_data = cbind(X[, scatter_cols, with = FALSE], target = factor(Y))
	if(scatter_cols != 'none') {
		s = ggpairs(scatter_matrix_data, aes(colour = target, alpha = 0.4), cardinality_threshold = 30)
	}
	ggsave(paste0('plots\\continuous\\scatter_matrix.jpg'), s)
	for(eachVar in numerics) {
		cat(paste0('\t \t ==> For: ', eachVar, '\n'))
		library(gridExtra)
		library(ggplot2)
		library(GGally)
		# # box plot # #
		subset_data = cbind(X[, eachVar, with = FALSE], Y = Y)
		setnames(subset_data, eachVar, 'Variable')
		subset_data[, Y := factor(Y)]
		cat(paste0('\t \t \t ==> Box plot for: ', eachVar, '\n'))
		p1 = ggplot(data = subset_data, aes(x = Y, y = Variable, color = Y)) + geom_boxplot(outlier.color = 'red', alpha = 0.3) +
				xlab('Target Variable') + ylab(eachVar) + ggtitle(paste0('Box plot for: ', eachVar))
		# compute lower and upper whiskers
		ylim1 = boxplot.stats(subset_data[, Variable])$stats[c(1, 5)]
		# scale y limits based on ylim1
		p2 = p1 + coord_cartesian(ylim = ylim1 * 1.05) + ggtitle(paste0('Box plot for: ', eachVar, ' (without Outliers)'))
		p = grid.arrange(grobs = list(p1, p2), ncol = 2)
		ggsave(paste0('plots\\continuous\\boxplot_', eachVar, '_', append, '.jpg'), p)
		cat('\t \t \t Done <==\n')
		# # histogram # #
		colors = c('lightblue', 'coral', 'lightgreen', 'darkblue', 'darkgreen', 'darkred')
		cat(paste0('\t \t \t ==> Histogram for: ', eachVar, '\n'))
		p = ggplot(data = subset_data, aes(Variable, fill = Y)) + geom_histogram(alpha = 0.4, color = 'grey') + scale_fill_manual(values = colors)
		ggsave(paste0('plots\\continuous\\histogram_', eachVar, '_', append, '.jpg'), p)
		cat('\t \t \t Done <==\n')
	}
	cat('\t Done with Numerical columns <==\n')
	# # for character - plot stacked barchart # #
	cat('\t ==> Running for Categorical columns\n')
	dir.create('plots\\categorical', showWarnings = FALSE)
	for(eachVar in factors) {
		if(length(unique(X[[eachVar]])) <= 26) {
			cat(paste0('\t \t ==> For: ', eachVar, '\n'))
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
			cat(paste0('\t \t Done <==\n'))
		}
	}
	cat('\t Done with Categorical columns <==\n')
}
			
# # == Function for plotting simple plot between two variables color coding by target == # #
# # Parameters
# data = dataset on which the plots to be plotted
# c1 = column name 1
# c2 = column name 2
# target = target variable name
simple_pair_plot = function(data, c1 = NULL, c2 = NULL, target = 'Y') {
	library(ggplot2)
	library(gridExtra)
	library(data.table)

	if(any(is.null(c1) | is.null(c2))) {
		stop('Give Column names in c1 and c2 parameters\n')
	}

	if(!any(class(data) == 'data.table')) {
		data = data.table(data)
	}

	if(!all(c(c1, c2, target) %in% colnames(data))) {
		stop('Given columns are not present in the data\n')
	}

	X1 = data[[c1]]
	X2 = data[[c2]]
	Y = data[[target]]

	# check whether Y is factor or not
	if(!is.factor(Y)) {
		cat('==> Converting Y as factor\n')
		Y = as.factor(Y)
		cat('Done <==\n')
	}
	colors = c('lightblue', 'coral', 'lightgreen', 'darkblue', 'darkgreen', 'darkred')

	if(class(X1) == 'numeric' & class(X2) == 'numeric') {
		cat('==> Plotting two numeric columns\n')
		p = ggplot() + geom_point(aes(x = X1, y = X2, colour = Y)) + scale_fill_manual(values = colors) +
					xlab(c1) +
					ylab(c2)
		cat('Done <==\n')
	} else if(class(X1) %in% c('factor', 'character') & class(X2) %in% c('factor', 'character')) {
		cat('==> Plotting two categorical columns\n')
		X1 = factor(X1)
		X2 = factor(X2)
		axis = which.max(c(nlevels(X1), nlevels(X2)))
		p = list()
		ct = 1
		if(axis == 1) {
			for(eachLevel in levels(X1)) {
				subset_data = data.table(X1 = X2[X1 == eachLevel])
				subset_data[[target]] = Y[X1 == eachLevel]
				p[[ct]] = ggplot() + 
								geom_bar(data = subset_data, aes_string('X1', fill = target), 
									stat = "count", position = position_dodge()) +
								xlab(paste0('Subsetted: ', c1, '(level = ', eachLevel, ') & X-axis: ', c2)) +
								ylab('Count')
				ct = ct + 1
			}
		} else {
			for(eachLevel in levels(X2)) {
				subset_data = data.table(X1 = X1[X2 == eachLevel])
				subset_data[[target]] = Y[X2 == eachLevel]
				p[[ct]] = ggplot() + 
								geom_bar(data = subset_data, aes_string('X1', fill = target), 
									stat = "count", position = position_dodge()) +
								xlab(paste0('Subsetted: ', c1, '(level = ', eachLevel, ') & X-axis: ', c2)) +
								ylab('Count')
				ct = ct + 1
			}
		}
		diff = c()
		for(i in seq(3, 30, 3)) {
			diff = c(diff, i - length(p))
		}
		nrow = which(diff >= 0 & diff == min(diff[diff >= 0]))
		p = grid.arrange(grobs = p, ncol = 3, nrow = nrow)
		cat('Done <==\n')
	} else if(class(X1) == 'numeric' & class(X2) %in% c('factor', 'character')) {
		cat('==> Plotting a numeric and categorical columns\n')
		p = ggplot(data = data, aes(x = X2, y = X1, color = Y)) + 
				geom_boxplot(outlier.color = 'red', alpha = 0.3) +
				scale_fill_manual(values = colors) +
				xlab(c2) + ylab(c1)
		cat('Done <==\n')
	} else if(class(X1) %in% c('factor', 'character') & class(X2) == 'numeric') {
		cat('==> Plotting a numeric and categorical columns\n')
		p = ggplot(data = data, aes(x = X1, y = X2, color = Y)) + 
				geom_boxplot(outlier.color = 'red', alpha = 0.3) +
				scale_fill_manual(values = colors) +
				xlab(c1) + ylab(c2)
		cat('Done <==\n')
	} 
	return(p)
}

# # == Function to remove outliers from numerical independent vectors == # #
# # Parameters
# X = independent features (data.frame/data.table)
# Y = dependent target vector
# na.rm = Flag to indicate whether to remove NA or not
remove_outliers = function(X, na.rm = TRUE) {
	X_copy = X
	if(is.vector(X)) {
		X_temp = data.table(X)
		X = copy(X_temp)
	}
	if(any(class(X) != 'data.table')) {
		cat('==> Converting X to Data.table\n')
		library(data.table)
		X = setDT(X)
		cat('Done <==\n')
	}
	cat('==> Getting numeric columns\n')
	numerics = colnames(X)[which(sapply(X, class) %in% c('numeric', 'integer'))]
	cat(paste0('Numeric columns: ', paste0(numerics, collapse = ', '), '\n'))
	cat('Done <==\n')
	for(eachVar in numerics) {
		cat(paste0('\t ==> Checking for: ', eachVar, '\n'))
		x = X[[eachVar]]
		qnt = quantile(x, probs = c(.25, .75), na.rm = na.rm)
		H = 1.5 * IQR(x, na.rm = na.rm)
		y = x
		if(length(X[[eachVar]][y < qnt[1] - H]) > 0) {
			cat(paste0('\t \t Capping lower level outliers in: ', eachVar, '\n'))
			X[[eachVar]][y < (qnt[1] - H)] = (qnt[1] - H)
		}
		if(length(X[[eachVar]][y > (qnt[2] + H)]) > 0) {
			cat(paste0('\t \t Capping top level outliers in: ', eachVar, '\n'))
			X[[eachVar]][y > (qnt[2] + H)] = (qnt[2] + H)
		}
		cat(paste0('\t Done with: ', eachVar, '<==\n'))
	}
	cat('Done <==\n')
	if(is.vector(X_copy)) {
		return(X[[colnames(X)]])
	} else {
		return(X)
	}
}

# # == Function to install required packages == # #
install_packages = function() {
	required_packages = c('data.table', 'ggplot2', 'gridExtra', 'h2o', 'xgboost', 'ranger', 'rpart', 'rpart.plot', 'GGally', 'devtools', 'caret', 'class', 'DMwR', 'InformationValue', 'Metrics', 'Matrix', 'MLmetrics', 'devtools', 'catboost', 'lightgbm', 'randomForest', 'e1071', 'mlr', 'klaR')

	to_install = required_packages[which(!required_packages %in% rownames(installed.packages()))]
	cat(paste0('Installing the following packages: ', paste0(to_install, collapse = ', '), '\n'))
	for(each in to_install) {
		cat(paste0('\t==> Installing:', each, '\n'))
		if(!each %in% c('lightgbm', 'catboost')) {
			install.packages(each, repos='http://cran.us.r-project.org')
		} else if(each == 'catboost') {
			cat('\t\tNote: Catboost install may fail at times\n')
			devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
		} else if(each == 'lightgbm') {
			cat('\t\t== # # Install the lightgbm using below commands in CLI # # ==\n')
			cat('\t\t> git clone --recursive https://github.com/Microsoft/LightGBM\n')
			cat('\t\t> cd LightGBM/R-package\n')
			cat('\t\t> R CMD INSTALL --build . --no-multiarch\n')
		}	
		cat('\tDone <==\n')
	}
}
	
# # == Function to replace special characters in names == # #
# Parameters
# names = column names of the data
clean_names = function(names) {
	names = gsub("[[:punct:]]", "_", names)
	names = gsub("_+", "_", names)
	names = gsub('_$', '', names)
	names = gsub('^_', '', names)
	return(names)
}

# # == Function to check factor levels in train and test data == # #
# Parameters
# train = train data
# test = test data
check_factors = function(train, test) {
	intersect_cols = intersect(colnames(train), colnames(test))
	train = train[, intersect_cols, with = FALSE]
	test = test[, intersect_cols, with = FALSE]
	print(train[, lapply(.SD, function(x) length(levels(x))), .SDcols = colnames(train)[which(sapply(train, class) == 'factor')]])
	print(test[, lapply(.SD, function(x) length(levels(x))), .SDcols = colnames(test)[which(sapply(test, class) == 'factor')]])
}

# # == Function to check missing values and return the flag == # #
# Parameters
# data = input data
check_missing = function(data) {
	class = sapply(data, class)
	missing_flag = apply(data, 2, function(x) any(is.na(x)))
	blank_flag = apply(data, 2, function(x) any(x == '', na.rm = T))
	data.table(name = colnames(data), class, missing_flag, blank_flag)
}

# # == Function to summarize the data
# Parameters
# data = input data
summarize = function(data) {
	class = sapply(data, class)
	# # for numeric/integer
	numeric_cols = colnames(data)[class %in% c('numeric', 'integer')]
	min_data = apply(data[, numeric_cols, with = FALSE], 2, min, na.rm = T)
	max_data = apply(data[, numeric_cols, with = FALSE], 2, max, na.rm = T)
	mean_data = apply(data[, numeric_cols, with = FALSE], 2, mean, na.rm = T)
	median_data = apply(data[, numeric_cols, with = FALSE], 2, median, na.rm = T)
	missing_data = apply(data[, numeric_cols, with = FALSE], 2, function(x) any(is.na(x)))
	quantile_data = data.table(t(apply(data[, numeric_cols, with = FALSE], 2, function(x) quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = TRUE))))
	colnames(quantile_data) = c('Q0', 'Q25', 'Q50', 'Q75', 'Q100')
	outlier_data = data.table(t(apply(data[, numeric_cols, with = FALSE], 2, function(x) {
						qnt = quantile(x, probs = c(.25, .75), na.rm = T)
						H = 1.5 * IQR(x, na.rm = T)
						y = x
						lower_outlier = FALSE
						upper_outlier = FALSE
						if(length(x[y < qnt[1] - H]) > 0) {
							lower_outlier = TRUE
						}
						if(length(x[y > (qnt[2] + H)]) > 0) {
							upper_outlier = TRUE
						}
						return(c(lower_outlier, upper_outlier))
					})))
	colnames(outlier_data) = c('lower_outlier', 'upper_outlier')
	distinct_data = apply(data[, numeric_cols, with = FALSE], 2, function(x) length(unique(x, na.rm = TRUE)))
	numeric_data = data.table(col_name = numeric_cols, class = 'numeric', min_values = min_data, max_values = max_data, mean_values = mean_data, median_values = median_data, missing_flag = missing_data, n_distinct = distinct_data)
	numeric_data = cbind(numeric_data, quantile_data, outlier_data)
	# # for factor
	# mode
	# missing_flag
	# blank_flag
	# nlevels
	factor_cols = colnames(data)[class %in% c('character', 'factor')]
	data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
	mode_data = apply(data[, factor_cols, with = FALSE], 2, mode)
	missing_data = apply(data[, factor_cols, with = FALSE], 2, function(x) any(is.na(x)))
	blank_data = apply(data[, factor_cols, with = FALSE], 2, function(x) any(x == '', na.rm = T))
	nlevels_data = apply(data[, factor_cols, with = FALSE], 2, function(x) length(unique(x)))
	factor_data = data.table(col_name = factor_cols, class = 'factor', mode_values = mode_data, missing_flag = missing_data, blank_flag = blank_data, nlevels = nlevels_data)
	return(list(numeric_data = numeric_data, factor_data = factor_data))
}

