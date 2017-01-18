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

# # == Function to tune random forest model to achieve best output == # #
# # grid search - tune parameters
# # To do:
# play with no.of.trees
# play with mtry
# play with node size
fineTuneRF = function(dataset = NULL, mtryRange = 1:15, ntreeRange = seq(1000, 2500, 500), seed = NULL) {
	# # Install required packages if not available
	if("caret" %in% rownames(installed.packages()) == FALSE) {
		install.packages("caret", repos = "http://cran.us.r-project.org/")
	}
	if("randomForest" %in% rownames(installed.packages()) == FALSE) {
		install.packages("randomForest", repos = "http://cran.us.r-project.org/")
	}
	if("e1071" %in% rownames(installed.packages()) == FALSE) {
		install.packages("e1071", repos = "http://cran.us.r-project.org/")
	}

	library(caret)
	
	customRF = list(type = "Classification", library = "randomForest", loop = NULL)
	customRF$parameters = data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
	customRF$grid = function(x, y, len = NULL, search = "grid") {}
	customRF$fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
	  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
	}
	customRF$predict = function(modelFit, newdata, preProc = NULL, submodels = NULL)
	   predict(modelFit, newdata)
	customRF$prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
	   predict(modelFit, newdata, type = "prob")
	customRF$sort = function(x) x[order(x[,1]),]
	customRF$levels = function(x) x$classes

	control = trainControl(method = "repeatedcv", number = 10, repeats = 3)
	tunegrid = expand.grid(.mtry = mtryRange, .ntree = ntreeRange)
	if(!is.null(seed)) {
		set.seed(seed)
	}
	custom = train(factor(Target) ~ ., data = dataset, method = customRF, metric = "Accuracy",
		tuneGrid = tunegrid, trControl = control)
	plot(custom)
	return(list(fit = custom, summary = summary(custom)))
}


# # == Function to build ensemble models by using a list of models == # #
# # Parameters
# original = A vector of original target values (2 classes) with values either 0 or 1
# modelPredictions = A list of model probabilities predictions (each item is a vector of probabilities from a particular model)
# trainSplit = Percentage Split to seperate train and test sets
# allTests = A boolean flag to print all possible tests or not
# ensembleModel = model to use as ensembleModel - currently "glm" or "randomForest" or "deepnet" is accepted as parameters
# scaleFlag = a flag that tells us to normalize the data or not
ensemble = function(original = vector(), modelPredictions = list(), trainSplit = 0.75, allTests = FALSE,
	ensembleModel = "glm", scaleFlag = TRUE) {
	if(length(original) == 0) {
		stop("== ERROR: THE LENGTH OF ORIGINAL TARGET VALUES SHOULD NOT BE ZERO (parameter = original) ==")
	}
	if(length(modelPredictions) == 0) {
		stop("== ERROR: WE NEED THE MODEL PREDICTIONS (modelPredictions parameter) ==")
	} else if(length(modelPredictions) == 1) {
		stop("== ERROR: WE NEED MORE THAN ONE MODEL TO BUILD ENSEMBLE ==")
	}

	# # install packages if not available
	if("caret" %in% rownames(installed.packages()) == FALSE) {
		install.packages("caret", repos = "http://cran.us.r-project.org/")
	}
	
	library(caret)
	index = createDataPartition(original, p = trainSplit, list = F, times = 1)
	
	# # Split original into train and test
	train = original[index]
	test = original[-index]

	# # Split model prediction (mp) values into train and test
	mpTrain = lapply(modelPredictions, function(x) {
		x[index]
	})
	mpTest = lapply(modelPredictions, function(x) {
		x[-index]
	})

	trainSet = data.frame(cbind(do.call(cbind, mpTrain), Y = train))
	testSet = data.frame(cbind(do.call(cbind, mpTest), Y = test))

	# # Ensemble - Logistic
	if(ensembleModel == "glm") {
		fit = glm(factor(Y) ~ ., data = trainSet, family = "binomial")
	} else if(ensembleModel == "randomForest") {
		# # Ensemble - RANDOM FOREST
		if("randomForest" %in% rownames(installed.packages()) == FALSE) {
			install.packages("randomForest", repos = "http://cran.us.r-project.org/")
		}
		library(randomForest)
		fit = randomForest(factor(Y) ~ ., data = trainSet)
	} else if(ensembleModel == "deepnet") {
		# # Ensemble - NEURAL NETS
		if("deepnet" %in% rownames(installed.packages()) == FALSE) {
			install.packages("deepnet", repos = "http://cran.us.r-project.org/")
		}
		library(deepnet)
		x = as.matrix(trainSet[, -ncol(trainSet)])
		means = apply(x, 2, mean)
		sds = apply(x, 2, sd)
		if(scaleFlag) {
			x = (x - means)/sds # SCALING
		}
		y = as.numeric(as.character(trainSet$Y))
		fit = dbn.dnn.train(x = x, y = y, learningrate = 0.2, momentum = 0.8, hidden = c(1), activationfun = "sigm")
	}

	# # Print all test results (all performances) for all cases
	if(allTests) {
		modelNames = vector()
		# # Individual model performances
		# ~ print(" --- Individual model performances --- ")
		indPerf = lapply(modelPredictions, function(x) {
			performance(as.numeric(x >= 0.5), original)
		})
		modelNames = c(modelNames, paste("individual - ", 1:length(indPerf)))
		# ~ print(indPerf)
		performances = do.call(rbind, indPerf)

		# # Max voting - basic method
		# ~ print(" --- Max voting - basic method --- ")
		maxVoting = Reduce("+", lapply(modelPredictions, function(x) {
			x * (1/length(modelPredictions))
		}))
		modelNames = c(modelNames, "maxVoting")
		# ~ print(performance(as.numeric(maxVoting >= 0.5), original))
		performances = rbind(performances, performance(as.numeric(maxVoting >= 0.5), original))

		# # Ensemble - Logistic
		if(ensembleModel == "glm") {
			# ~ print(" --- Ensemble - Logistic --- ")
			modelNames = c(modelNames, "Logistic")
			# ~ print(performance(as.numeric(predict(fit, testSet, type = "response") >= 0.5), testSet$Y))
			performances = rbind(performances, performance(as.numeric(predict(fit, testSet, type = "response") >= 0.5),
				testSet$Y))
		} else if(ensembleModel == "randomForest"){
			# # Ensemble Modelling - RANDOM FOREST
			# ~ print(paste0(" --- Ensemble - ", as.character(ensembleModel), " --- "))
			modelNames = c(modelNames, "randomForest")
			# ~ print(performance(predict(fit, testSet), testSet$Y))
			performances = rbind(performances, performance(predict(fit, testSet), testSet$Y))
		} else if(ensembleModel == "deepnet") {
			# ~ print(paste0(" --- Ensemble - ", as.character(ensembleModel), " --- "))
			modelNames = c(modelNames, "deepnet")
			testX = as.matrix(testSet[, -ncol(testSet)])
			if(scaleFlag) {
				testX = (testX - means)/sds
			}
			# ~ print(performance(as.numeric(as.vector(nn.predict(fit, testX)) >= 0.5), testSet$Y))
			performances = rbind(performances, performance(as.numeric(as.vector(nn.predict(fit, testX)) >= 0.5), testSet$Y))
		}
		performances$modelNames = modelNames
		return(list(performances = performances, fit = fit))
	} else {
		return(fit)
	}
}

# # == Function to compute PCA - Remove target variable then pass it to this function == # #
# # Parameters
# trainData = data used to train the PCA model
# testData = if specified we can predict and get the final rotated matrix with selected eigens
# thresholdVariance = % of variance used as threshold to select the top eigens (i.e top % of eigens to be selected)
computePCA = function(trainData, testData = NULL, thresholdVariance = 0.97) {
	mData = as.matrix(trainData) # matrix of data
	pcal = prcomp(mData, scale. = TRUE, center = TRUE) # Apply PCA
	vars = apply(pcal$x, 2, var)  # calculate variance for each eigen (component)
	props = vars / sum(vars) # Variance proportions (sum of them = 1)
	cumvar = cumsum(props) # Cumulative sum of variances

	count = 1
	repeat {
	if(cumvar[count] > thresholdVariance) { break }
		count = count + 1
	}
	selectedEigens = count  # number of eigenvectors to explain thresholdVariance% of variance
	rotMatrix = pcal$rotation # rotation matrix
	PC = mData %*% rotMatrix[, 1:selectedEigens] # rotate the original matrix w.r.t the rotation matrix (with selected eigens)
	if(is.null(testData)) {
		return(list(pcFinal = data.frame(PC), nEigens = selectedEigens, pcaObj = pcal))
	} else {
		tPred = predict(pcal, testData)
		return(list(pcFinal = data.frame(PC), nEigens = selectedEigens, pcaObj = pcal, testOP = data.frame(tPred)))
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

# # == Function for calculating performance measures - precision, recall=tpr=sensitivity, specificity=tnr,
# # fpr, fnr, mse, rmse, fscore, accuracy == # #
# # Parameters
# predicted = either probabilities or predicted outcome values (either 0 or 1)
# original = original 2 class target values (either 0 or 1)
# beta = a trade off used to give account for precision or recall (more the beta value, more tradeoff given for recall)
# format = return data type format for all the performance measures
# cutoff = a threshold to seperate the probabilities into two classes 0 or 1
performance = function(predicted, original, beta = 1, format = "dataframe", prob = FALSE, cutoff = 0.5) {
	if(prob) {
		predicted = as.numeric(predicted >= cutoff)
	}
	if(length(unique(predicted)) > 2) {
		predicted = as.numeric(predicted >= cutoff)
	}
	if(is.factor(predicted)) {
		predicted = as.numeric(as.character(predicted))
	}
	if(is.factor(original)) {
		original = as.numeric(as.character(original))
	}
	tp = length(which(predicted == 1 & original == 1))
	tn = length(which(predicted == 0 & original == 0))
	fp = length(which(predicted == 1 & original == 0))
	fn = length(which(predicted == 0 & original == 1))

	precision = ifelse((tp + fp) == 0, 0 , tp/(tp + fp))
	recall = ifelse((tp + fn) == 0, 0, tp/(tp + fn))
	accuracy = ifelse((tp + tn + fp + fn) == 0, 0, (tp+tn)/(tp + tn + fp + fn))
	specificity = ifelse((fp + tn) == 0, 0, tn/(tn + fp))
	fnr = ifelse((fn + tp) == 0, 0, fn/(fn + tp)) # 1 - recall
	fpr = ifelse((fp + tn) == 0, 0, fp/(fp + tn)) # 1- specificity
	mse = sum((predicted - original)^2)/length(original)
	rmse = sqrt(mse)
	
	if((precision != 0 & recall != 0)) {
		fscore = ((1 + beta * beta) * precision * recall)/((beta * beta * precision) + recall)
	} else {
		fscore = 0
	}

	if(format == "string") {
		return(paste0("Precision = ", precision, ", Recall = ", recall, ", FScore = ", fscore,
			", Accuracy = ", accuracy, ", Specificity = ", specificity, ", FNR = ", fnr,
			", FPR = ", fpr, ", MSE = ", mse, ", RMSE = ", rmse))
	} else if(format == "dataframe") {
		return(data.frame(precision = precision, recall = recall, fscore = fscore,
			accuracy = accuracy, specificity = specificity, fnr = fnr, fpr = fpr,
			mse = mse, rmse = rmse))
	} else {
		return(list(precision = precision, recall = recall, fscore = fscore,
			accuracy = accuracy, specificity = specificity, fnr = fnr, fpr = fpr,
			mse = mse, rmse = rmse))
	}
}

# # == Function to process imbalanced datasets == # #
# # Parameters
# inputData = imbalanced data
# target = name of the target variable
# trainSplit = percentage split of training and testing data
# cutoff = a threshold to seperate the probabilities into two classes 0 or 1
# modelName = "glm" or "randomForest" or "rpart" as modelling functions
# balancePerc = 0.6 i.e. 60% of high numbered class and 40% as low numbered class in the data
# seed = seed value to avoid randomization
imbalanced = function(inputData, target = "Y", trainSplit = 0.75, cutoff = 0.5, modelName = "glm",
	balancePerc = 0.6, seed = 1, beta = 1) {
	# # https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
	colnames(inputData)[which(colnames(inputData) == target)] = "Y"
	# create a formula
	n = colnames(inputData)
	f = as.formula(paste("Y ~", paste(n[!n %in% "Y"], collapse = " + ")))
	print(f)

	if("caret" %in% rownames(installed.packages()) == FALSE) {
		install.packages("caret", repos = "http://cran.us.r-project.org/")
	}
	if("ROSE" %in% rownames(installed.packages()) == FALSE) {
		install.packages("ROSE", repos = "http://cran.us.r-project.org/")
	}

	library(caret)
	library(ROSE)

	# # split into training and testing
	index = createDataPartition(inputData$Y, p = trainSplit, list = F, times = 1)

	# # Split original into train and test
	train = inputData[index, ]
	test = inputData[-index, ]

	# # store performances here
	performances = data.frame()

	nOnes = length(which(train$Y == 1))
	nZeroes = length(which(train$Y == 0))


	# # normal fit
	nFit = glm(f, data = train, family = "binomial")
	performances = rbind(performances, performance(as.numeric(predict(nFit, test) >= cutoff), test$Y, beta = beta))

	# # oversampling fit
	# no. of sample after 60% Zeroes and 40% ones will be
	nSamples = round(nZeroes / balancePerc)
	# left over ones that covers 40% of the nSamples
	lOnes = nSamples - nZeroes
	overSize = nrow(train) + lOnes # ~ abs(length(which(train$Y == 1)) - length(which(train$Y == 0)))
	print(overSize)
	over = ovun.sample(f, data = train, method = "over", N = overSize)$data
	oFit = glm(f, data = over, family = "binomial")
	performances = rbind(performances, performance(as.numeric(predict(oFit, test) >= cutoff), test$Y, beta = beta))

	# # undersampling fit
	# get no.of samples such that the ones are of (1-balancePerc)%
	nSamples = round(nOnes / (1 - balancePerc))
	underSize = nSamples
	print(underSize)
	under = ovun.sample(f, data = train, method = "under", N = underSize, seed = seed)$data
	uFit = glm(f, data = under, family = "binomial")
	performances = rbind(performances, performance(as.numeric(predict(uFit, test) >= cutoff), test$Y, beta = beta))

	# # both over and undersampling fit
	both = ovun.sample(f, data = train, method = "both", p = balancePerc, N = nrow(train), seed = seed)$data
	bFit = glm(f, data = both, family = "binomial")
	performances = rbind(performances, performance(as.numeric(predict(bFit, test) >= cutoff), test$Y, beta = beta))

	# # ROSE fit - cost sensitive learning
	rose = ROSE(f, data = train, seed = seed)$data
	rFit = glm(f, data = rose, family = "binomial")
	performances = rbind(performances, performance(as.numeric(predict(rFit, test) >= cutoff), test$Y, beta = beta))

	performances$type = c("normal fit", "over-sampled fit", "under-sampled fit", "both over and under sampled fit",
		"ROSE fit")
	return(list(fits = c(nFit, oFit, uFit, bFit, rFit), performances = performances))
}

# # == Function to perform matrix factorization == # #
# # Parameters
# R = ratings matrix (N x M)
# N = no. of users
# M = no. of movies
# K = 2 (no. of features i.e. compare atmost 2 users)
# P = random matrix (N x K)
# Q = random matrix (K x M)
# steps = max no. of steps to reach minimum
# alpha = learning rate
# beta = regularization term (to avoid overfitting)
# Aim P %*% Q ~ R (with minimum error using gradient descent)
matrixFactorization = function(R, K = 2, steps = 5000, alpha = 0.0002, beta = 0.02) {
	# # Source = http://www.quuxlabs.com/blog/2010/09/matrix-factorization-a-simple-tutorial-and-implementation-in-python/
	# ~ sample R = matrix(c(5,3,0,1,4,0,0,1,1,1,0,5,1,0,0,4,0,1,5,4), nrow = 5, byrow = T)
	N = nrow(R)
	M = ncol(R)
	P = matrix(runif(N * K), nrow = N)
	Q = matrix(runif(M * K), nrow = K)
	for(eachStep in 1:steps) {
		for(i in 1:nrow(R)) {
			for(j in 1:ncol(R)) {
				if(R[i, j] > 0) {
					eij = R[i, j] - as.numeric(matrix(P[i, ], nrow = 1) %*% matrix(Q[, j]))
					for(k in 1:K) {
						P[i, k] = P[i, k] + alpha * (2 * eij * Q[k, j] - beta * P[i, k])
						Q[k, j] = Q[k, j] + alpha * (2 * eij * P[i, k] - beta * Q[k, j])
					}
				}
			}
		}
		eR = P %*% Q
		e = 0
		for(i in 1:nrow(R)) {
			for(j in 1:ncol(R)) {
				if(R[i, j] > 0) {
					e = e + (R[i, j] - as.numeric(matrix(P[i, ], nrow = 1) %*% matrix(Q[, j])))**2
					for(k in 1:K) {
						e = e + (beta/2) * (P[i, k] ** 2) + (Q[k, j] ** 2)
					}
				}
			}
		}
		if(e < 0.001) {
			break;
		}
	}
	return(P %*% Q)
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
		
# # == Function to calculate SVM loss from the classification == # #
# # Parameters
# scores = scores obtained out of a classification model
# class = index of the original class value (i.e. if there are two classes 'Y' and 'N' and the data point is having 'Y' then class will be 1)
svmLoss = function(scores, class) {
	sum(sapply(scores[-class]-scores[class]+1, function(x) max(0, x)))
}

# # == Function to calculate Entropy/Log loss from the classification == # #
# # Parameters
# scores = scores obtained out of a classification model
# class = index of the original class value (i.e. if there are two classes 'Y' and 'N' and the data point is having 'Y' then class will be 1)
# prob = flag that says if scores are probabilities or not
logLoss = function(scores, class, prob = TRUE) {
	if(prob) {
		-log10(scores)[class]
	} else {
		-log10(exp(scores)/sum(exp(scores)))[class]
	}
}
