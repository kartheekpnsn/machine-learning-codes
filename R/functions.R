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
# all = A boolean flag that tells to return all values or just the cutoff value
# plotROC = A boolean flag that tells to plot an ROC curve or not
getCutoff = function(probabilities, original, beta = 1, all = TRUE, plotROC = TRUE) {
	if(length(unique(probabilities)) <= 2) {
		stop("ERROR: WE NEED PROBABILITIES GIVEN BY THE MODEL TO GET A CUTOFF (paremeter = probabilities)")
	}
	
	performances = data.frame()
	allThresholds = seq(0, 1, 0.01)
	for(each in allThresholds) {
		predicted = as.numeric(probabilities >= each)
		performances = rbind(performances, performance(predicted, original, beta = beta, format = "dataframe"))
	}
	# dfCutoff = dataframe with cutoffs and respective thresholds
	dfCutoff = data.frame(cutoff = allThresholds, tpr = performances$recall, fpr = performances$fpr,
		fscores = performances$fscore)
	dfCutoff = dfCutoff[order(dfCutoff$fscores, decreasing = T), ]
	# fCutoff = final cutoff that is to be selected
	fCutoff = dfCutoff$cutoff[1]
	if(plotROC) {
		if("Metrics" %in% rownames(installed.packages()) == FALSE) {
			install.packages("Metrics", repos = "http://cran.us.r-project.org/")
		}
		library(Metrics)
		auc = auc(original, probabilities)
		if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
			install.packages("ggplot2", repos = "http://cran.us.r-project.org/")
		}
		library(ggplot2)
		d = ggplot(data = performances, aes(x = fpr, y = recall, group = 1))
		d = d + geom_line(linetype = "dashed")
		d = d + geom_point(size = 1)
		d = d + geom_abline(intercept = 0, color = "red")
		d = d + geom_text(x = 0.8, y = 0.1, label = paste("AUC:", round(auc, 4)))
		print(d)
		# plot(performances$fpr, performances$recall, type = "o")
	}
	if(all) {
		return(list(dfCutoff = dfCutoff, fCutoff = fCutoff, performances = performances, auc = auc))
	}
	else {
		return(fCutoff)
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
dataSplit = function(target, split_ratio = c(0.7, 0.2), seed = 294056) {
	set.seed(seed)
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
	} else if(length(split_ratio) + 1 == 2) {
		names(index) = c('train', 'test')
	}
	return(index)
}

# # == Function to calculate the performance measures for the prediction == # #
# # Parameters
# actual = actual values
# predicted = predicted probability values
# metric = what performance metric needs to be calculated
# optimal_threshold = whether to calculate the optimal threshold or not
# regression = flag indicating whether it is a regression problem or not
# plotROC_flag = flag indicating whether to plot ROC flag or not
# beta = beta value for fbeta score, where if beta value is high - we expect more recall
performance_measure = function(predicted, actual, threshold = 0.5, metric = 'all', optimal_threshold = TRUE, regression = FALSE, plotROC_flag = FALSE, beta = 1) {
	if(length(predicted) != length(actual)) {
		stop('> Length of Predicted and Actual not matching')
	}
	if(regression) {
		mae = sum(abs(predicted - actual))/length(actual)
		mse = sum((predicted - actual) ** 2)/length(actual)
		rmse = sqrt(mse)
		if(metric == 'rmse') {
			return(rmse)
		} else if(metric == 'mse') {
			return(mse)
		} else if (metric == 'mae') {
			return(mae)
		} else if(metric == 'all') {
			return(data.table(mae = mae, mse = mse, rmse = rmse))
		} else {
			stop('> For regression use metric as "rmse" (or) "mse" (or) "mae" (or) "all"')
		}

	} else {
		actual = as.numeric(as.factor(actual)) - 1
		if(plotROC_flag) {
			plotROC(actuals = actual, predictedScores = predicted)
		}
		if("InformationValue" %in% rownames(installed.packages()) == FALSE) {
			install.packages("InformationValue", repos = "http://cran.us.r-project.org/")
		}
		if("ROCR" %in% rownames(installed.packages()) == FALSE) {
			install.packages("ROCR", repos = "http://cran.us.r-project.org/")
		}
		library(ROCR)
		library(InformationValue)
		if(optimal_threshold) {
			threshold = InformationValue::optimalCutoff(predictedScores = predicted, actuals = actual, optimiseFor = 'Both')
			print(paste('> Threshold chosen:', threshold))
		}
		predicted_probabilities = predicted
		predicted = as.numeric(predicted >= threshold)
		accuracy = sum(predicted == actual)/length(actual)
		error = 1 - accuracy
		precision = InformationValue::precision(predictedScores = predicted, actuals = actual)
		recall = InformationValue::sensitivity(predictedScores = predicted, actuals = actual)
		fscore = ((1 + (beta ** 2)) * precision * recall)/(((1 + (beta ** 2)) * precision) + recall)
		mse = sum((predicted_probabilities - actual) ** 2)/length(actual)
		mae = sum(abs(predicted_probabilities - actual))/length(actual)
		rmse = sqrt(mse)
		auc = ROCR::performance(prediction(predicted_probabilities, actual), "auc")
		auc = auc@y.values[[1]]
		concordance = InformationValue::Concordance(actuals = actual, predictedScores = predicted)$Concordance
		logloss = - sum((actual * log(predicted_probabilities)) + ((1 - actual) * log((1 - predicted_probabilities))))/length(actual)
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
		} else if(metric == 'concordance') {
			return(concordance)
		} else if(metric == 'error') {
			return(error)
		} else if(metric == 'mae') {
			return(mae)
		} else if(metric == 'logloss') {
			return(logloss)
		} else if(metric == 'all') {
			metrics = data.table(accuracy = accuracy, precision = precision, recall = recall, fscore = fscore,
							auc = auc, concordance = concordance, error = error, logloss = logloss, mae = mae, mse = mse, rmse = rmse)
			return(metrics)
		} else {
			cat('> For classification use metric as:\n')
			cat('accuracy\nprecision\nrecall\nfscore\nauc\nconordance\nerror\nlogloss\nrmse\nmse\nmae\nall\n')
			stop('> Invalid metric used')
		}
	}
}

# # == Function to calculate the important features == # #
# # Parameters
# X = independent features
# Y = target dependent variable
importantFeatures = function(X, Y) {
	numerics = colnames(X)[which(sapply(X, class) %in%  c('numeric', 'integer'))]
	categoricals = setdiff(colnames(X), numerics)

	# # Chi-Square Test for Categorical vs Target # #
	df = copy(X)
	df = setDF(df)
	chisq = data.table(feature = character(), p_value = numeric())
	for(eachVar in categoricals){
		tab = table(df[, eachVar], Y)
		chi = chisq.test(tab)
		chisq = rbind(chisq, data.table(feature = eachVar, p_value = chi$p.value))
	}
	chisq[, significant := (p_value <= 0.05)]
	
	# # Weight of Evidence and Information Value # #
	if("InformationValue" %in% rownames(installed.packages()) == FALSE) {
		install.packages("InformationValue", repos = "http://cran.us.r-project.org/")
	}
	library(InformationValue)
	important_features = data.frame()
	for(eachVar in setdiff(colnames(X), numerics)) {
		options(scipen = 999, digits = 4)
		IV = as.numeric(IV(X = X[[eachVar]], Y = Y))
		howgood = attr(IV(X = X[[eachVar]], Y = Y), 'howgood')
		important_features = rbind(important_features, data.frame(feature = eachVar, IV = IV, howgood = howgood))
	}
	important_features = setDT(important_features)
	setorder(important_features, -IV)

	# # ANOVA - For Continuous and Target variable # #
	anova = data.table(feature = character(), p_value = numeric())
	for(eachVar in numerics) {
		data = cbind(X, Y = Y)
		formula = as.formula(paste(eachVar, '~ Y'))
		aov_test = aov(formula, data = data)
		p_value = summary(aov_test)[[1]][["Pr(>F)"]][1]
		anova = rbind(anova, data.table(feature = eachVar, p_value = p_value))
	}
	anova[, significant := (p_value <= 0.05)]
	return(list(chisq = chisq, IV = important_features, anova = anova))
}
