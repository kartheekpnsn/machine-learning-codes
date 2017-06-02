# # Functions to add
# concordance
# roc
# lift, gain
# woe
# binning


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

# # == Function to calculate the error measure for the prediction == # #
# # Parameters
# original = actual values
# predicted = predicted/forecasted values (for classification it needs to be rounded off)
# loss = the loss function that needs to be calculated
	# "rmse" = root mean squared error
	# "mse" = mean squared error
	# "mape" = mean absolute percentage error
	# "mae" = mean absolute error
	# "logistic" = logistic loss function 
error_measure = function(original, predicted, loss = "rmse") {
	if(loss == "mse") {
		return(sum((original - predicted)^2)/length(original))
	} else if(loss == "rmse") {
		return(sqrt(sum((original - predicted)^2)/length(original)))
	} else if(loss == "mape") {
		term1 = abs(original - predicted)
		term2 = abs(term1 / original)
		term3 = (100/length(original)) * sum(term2)
		return(term3)
	} else if(loss == "mae") {
		return(sum(abs(original - predicted))/length(original))
	} else if(loss == "logistic") {
		term1 = original * log(predicted)
		term2 = (1 - original) * log((1 - predicted))
		term3 = sum(term1 + term2)
		term4 = - term3 * (1 / length(original))
		return(term4)
	}
}
