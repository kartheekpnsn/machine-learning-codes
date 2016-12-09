rm(list = ls())
gc()

source("https://raw.githubusercontent.com/kartheekpnsn/MyScripts/master/R-Snippets/functions.R")

# # == perfect two circles == # #
	r1 = 0.4
	r2 = 0.2
	t = 2*pi*runif(2000)
	d1 = data.frame(x = r1 * cos(t), y = r1 * sin(t))
	d2 = data.frame(x = r2 * cos(t), y = r2 * sin(t))
	d1$Y = 1
	d2$Y = 0
	d = rbind(d1, d2)

	library(caret)
	index = createDataPartition(d$Y, times = 1, list = F, p = 0.75)
	train = d[index, ]
	test = d[-index, ]

	# # SVM
	library(e1071)
	svFit = svm(factor(Y) ~ ., data = train)
	plot(svFit, d, x ~ y,xlim=c(-1,1),ylim=c(-1,1))
	print(performance(predicted = predict(svFit, test), original = test$Y))
	# # RANDOM FOREST
	library(randomForest)
	rfFit = randomForest(factor(Y) ~ ., data = train)
	print(performance(predicted = predict(rfFit, test), original = test$Y))

# # == circle (negatives) and ellipse (positives) == # #
# # == NOTE: OVERLAPPING EXISTS  (observe value of b and r1, b = r1) == # #
	r1 = 0.8
	a = 0.4; b = 0.8
	t = 2*pi*runif(100)
	d1 = data.frame(x = r1 * cos(t), y = r1 * sin(t))
	d2 = data.frame(x = a * cos(t), y = b * sin(t))
	d1$Y = 1
	d2$Y = 0
	d = rbind(d1, d2)

	library(caret)
	index = createDataPartition(d$Y, times = 1, list = F, p = 0.75)
	train = d[index, ]
	test = d[-index, ]

	# # SVM
	library(e1071)
	svFit = svm(factor(Y) ~ ., data = train, kernel = "radial")
	plot(svFit, d, x ~ y,xlim=c(-1,1),ylim=c(-1,1))
	print(performance(predicted = predict(svFit, test), original = test$Y))
	# # RANDOM FOREST
	library(randomForest)
	rfFit = randomForest(factor(Y) ~ ., data = train)
	print(performance(predicted = predict(rfFit, test), original = test$Y))

# # == circle (negatives) and ellipse (positives) == # #
# # == NOTE: NO OVERLAPPING EXISTS BUT MARGIN IS VERY CLOSE (observe value of b and r1) == # #
	r1 = 0.8
	a = 0.4; b = 0.7
	t = 2*pi*runif(100)
	d1 = data.frame(x = r1 * cos(t), y = r1 * sin(t))
	d2 = data.frame(x = a * cos(t), y = b * sin(t))
	d1$Y = 1
	d2$Y = 0
	d = rbind(d1, d2)

	library(caret)
	index = createDataPartition(d$Y, times = 1, list = F, p = 0.75)
	train = d[index, ]
	test = d[-index, ]

	# # SVM
	library(e1071)
	svFit = svm(factor(Y) ~ ., data = train, kernel = "radial")
	plot(svFit, d, x ~ y,xlim=c(-1,1),ylim=c(-1,1))
	print(performance(predicted = predict(svFit, test), original = test$Y))
	# # RANDOM FOREST
	library(randomForest)
	rfFit = randomForest(factor(Y) ~ ., data = train)
	print(performance(predicted = predict(rfFit, test), original = test$Y))

# # == circle (negatives) and ellipse (positives) == # #
# # == NOTE: NO OVERLAPPING EXISTS BUT MARGIN IS FAR (observe value of b and r1) == # #
	r1 = 0.8
	a = 0.4; b = 0.6
	t = 2*pi*runif(100)
	d1 = data.frame(x = r1 * cos(t), y = r1 * sin(t))
	d2 = data.frame(x = a * cos(t), y = b * sin(t))
	d1$Y = 1
	d2$Y = 0
	d = rbind(d1, d2)

	library(caret)
	index = createDataPartition(d$Y, times = 1, list = F, p = 0.75)
	train = d[index, ]
	test = d[-index, ]

	# # SVM
	library(e1071)
	svFit = svm(factor(Y) ~ ., data = train, kernel = "radial")
	plot(svFit, d, x ~ y,xlim=c(-1,1),ylim=c(-1,1))
	print(performance(predicted = predict(svFit, test), original = test$Y))
	# # RANDOM FOREST
	library(randomForest)
	rfFit = randomForest(factor(Y) ~ ., data = train)
	print(performance(predicted = predict(rfFit, test), original = test$Y))
