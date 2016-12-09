# # http://stats.stackexchange.com/questions/76867/random-forest-importance-differs-between-rfimportance-and-importance?rq=1
library(caret)
library(randomForest)
applyRandomForest = function(dataFile, beta, ntree)
{
	flag = 0
	while(flag == 0)
	{
		dataFile$Y = as.factor(dataFile$Y)
		rf_fit = randomForest(Y ~ ., data = dataFile, ntree = ntree, importance = True)
		votes = rf_fit$votes[,2]
		cutoffs = getCutoff(dataFile, votes, beta)
		cutoff = cutoffs[[1]]
		cutoffs = cutoffs[[2]]
		imp_df2 = data.frame(importance(rf_fit))
		to_remove = rownames(imp_df2[imp_df$MeanDecreaseAccuracy < 0, ])
		if(length(to_remove) != 0)
		{
			print("Removed variables: ")
			print(to_remove)
			rm(imp_df2)
			dataFile = subset(dataFile, select = setdiff(colnames(dataFile), to_remove))
		}
		else
		{
			flag = 1
		}
		imp_df = data.frame(Importance = rf_fit$importance[order(rf_fit$importance, decreasing = T), ])
		if(flag == 1)
		{
			print(rf_fit$confusion)
			print(table(votes>cutoff, dataFile$Y))
			print(cutoff)			
		}
	}
	return(list(fit = rf_fit, cutoff = cutoff, cutoffs = cutoffs, imp_df = imp_df))
}
getCutoff = function(TestData, votes, beta)
{
	cutoffs = data.frame()
	for(i in seq(0, 1, 0.01))
	{
		pred = as.factor(as.numeric(votes > i))
		levels(pred) = c(0,1)
		tpr = table(pred, TestData$Y)[2,2] / length(TestData$Y[TestData$Y==1])
		fpr = table(pred, TestData$Y)[2,1] / length(TestData$Y[TestData$Y==0])
		Precision = table(pred, TestData$Y)[2,2] / length(pred[pred==1])
		FBetaScore<-((1+(beta*beta))*Precision*tpr)/(((beta*beta)*Precision)+tpr)
		cutoffs = rbind(cutoffs, data.frame(cutoff = i, tpr = tpr, fpr = fpr, Precision = Precision, FBetaScore = FBetaScore))
	}
	cutoffs = cutoffs[order(cutoffs$FBetaScore, cutoffs$cutoff, decreasing=T), ]
	return(list(cutoffs$cutoff[1], cutoffs))
}
