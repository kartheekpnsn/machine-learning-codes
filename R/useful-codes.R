# Important points in R

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # To install a package in R (if it is not working)
  install.packages("randomForest", repos="http://cran.cnr.berkeley.edu")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # To install and run RHive package
  URL: https://github.com/nexr/RHive
  #!/usr/bin/env Rscript
  Sys.setenv("HIVE_HOME"="/usr/lib/hive")
  Sys.setenv("HADOOP_HOME"="/usr/lib/hadoop")
  Sys.setenv("HADOOP_CONF_DIR"="/usr/lib/hadoop/etc/hadoop")
  # Sys.setenv(RHIVE_FS_HOME="/rhive")
  library("rJava")
  library("RHive")
  rhive.init()
  # rhive.connect("localhost")
  rhive.connect("IP-OF-MACHINE-THAT-RUNS-HIVE",defaultFS="hdfs://HDFS-IP-ADDRESS:PORT-NUMBER")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # How to get a cut off and Plot an ROC Curve with Area UnderCurve?
  # # # For more help visit - https://www.youtube.com/watch?v=lHa1UYAxGxs (watch two - parts)
  library(ROCR)
  rus_prob = predict(rus_model, newdata = myData_test)
  rus_pred = prediction(rus_prob$prob[,2], myData_test$Y)
  rus_perf = performance(rus_pred, measure = "tpr", x.measure = "fpr")
  # # Area Under Curve
  rus_auc <- performance(rus_pred, measure = "auc")
  rus_auc <- round(rus_auc@y.values[[1]],2)
  rus_rocData <- data.frame(fpr=unlist(rus_perf@x.values),tpr=unlist(rus_perf@y.values))
  plot(rus_rocData$fpr,rus_rocData$tpr,type="l",col="green",ann=F)
  
  # # Area Under Curve - Version 2.0
  library(pracma)
  trapz(rus_rocData$fpr, rus_rocData$tpr)
  
  # # Cut offs
  cutoffs = data.frame(cut=rus_perf@alpha.values[[1]], fpr=rus_perf@x.values[[1]], tpr=rus_perf@y.values[[1]])
  cutoffs = cutoffs[order(cutoffs$fpr, decreasing=TRUE),]
  head(subset(cutoffs, fpr < 0.2))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Function for calculation fBeta - Score
  fBeta = function(R,P,B)
  {
  	fBeta = ((1 + B * B) * P * R)/((B * B * P) + R)
  	return(fBeta)
  }

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # CREATE AND PREDICT USING OWN CLASSIFIER
  # create a function that returns an object of class myClassifierClass
  myClassifier = function(trainingData, ...) {
    model = structure(list(x = trainingData[, -1], y = trainingData[, 1]), 
                      class = "myClassifierClass") 
    return(model)
  }
  
  # create a method for function print for class myClassifierClass
  predict.myClassifierClass = function(modelObject) {
    return(rlogis(length(modelObject$y)))
  } 
  
  # test
  mA = matrix(rnorm(100*10), nrow = 100, ncol = 10)
  modelA = myClassifier(mA)
  predict(modelA)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Create a directory if not exists and set the path inside that
  mainDir = getwd() # Get current path
  subDir = "FolderName"
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Remove all variables except functions
  rm(list = setdiff(ls(), lsf.str()))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # REMOVE CORRELATED VARIABLES
  tmp = cor(data)
  tmp[upper.tri(tmp)] = 0
  diag(tmp) = 0
  # Above two commands can be replaced with 
  # tmp[!lower.tri(tmp)] <- 0
  data.new = data[,!apply(tmp,2,function(x) any(x > 0.99))]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # REMOVE CONSTANT COLUMNS
  dataFile = subset(dataFile, select = names(apply(dataFile, 2, sd)[!apply(dataFile, 2, sd)==0]))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Replace NA by median of that column
  f=function(x){
     x = as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
     x[is.na(x)] = median(x, na.rm=TRUE) #convert the item with NA to median value from the column
     x #display the column
  }
  df = data.frame(apply(df,2,f))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # EXPAND GRID
  data.frame(expand.grid(c("WT", "KO"), LETTERS[1:4]), 
             matrix(sample(40), ncol = 5))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # # GOOGLE VIS CHARTS - https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
  df=data.frame(country=c("US", "GB", "BR"), val1=c(10,13,14), val2=c(23,12,32))
  library(googleVis)
  Column <- gvisColumnChart(df)
  plot(Column)
  cat(Column$html$chart, "fileName.html") # Export it into html

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # User Defined Functions in Aggregate
  data = data.frame(col = c(1,2,3,1,2,3,1,2,3), col2 = c(1,1,1,1,1,1,2,2,2), col3 = c(1,2,3,3,2,1,1,2,3))
  calc = function(data)
  {
  	apply(data,2,diff)
  }
  library(plyr)
  ddply(data,"col",calc)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # User Defined Functions in APPLY
  calcPosNeg = function(x)
  {
  	return(length(x[x>0]))
  }
  df = data.frame(a = c(1,-2,1,1,0))
  apply(df,2,calcPosNeg)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # To eliminate the scientific notation;
  options("scipen"=100, "digits"=4)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Calculate centroid manually
  df = data.frame(Name = c(1,1,1,2,2,2,3,3,3),X = c(1,2,1,2,1,2,3,2,1),Y=c(1,3,4,2,3,4,6,7,4))
  ctrs <- lapply(unique( df$Name ) , function(x) gCentroid( SpatialPoints( df[ df$Name == x , c('X','Y') ] ) ) )
  setNames( ctrs , unique(df$Name ) )

  aggregate(.~Name, data=dat, mean)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Get the data from a variable
  get(paste0("df","0"))	# same as print(df0)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Self Join
  query = "SELECT count(*), d1.SerialNumber, d1.ReceiptDate FROM dF2 d1, dF2 d2 WHERE d1.SerialNumber = d2.SerialNumber AND d1.ReceiptDate = d2.ReceiptDate group by d1.SerialNumber, d1.ReceiptDate";
  # or you can use this
  # query = "SELECT count(*), d1.SerialNumber, d1.ReceiptDate FROM dF2 d1 JOIN dF2 d2 ON d1.SerialNumber = d2.SerialNumber AND d1.ReceiptDate = d2.ReceiptDate group by d1.SerialNumber, d1.ReceiptDate";
  library(sqldf)
  dF3 = sqldf(query)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # PRCOMP PCA
  pcal = prcomp(dataFile2, scale = T, center = T)
  plot(pcal)
  vars <- apply(pcal$x, 2, var) 
  props <- vars / sum(vars)
  cumvar<-cumsum(props)
  temp<-length(props)
  i=1
  repeat
  {
  	if(cumvar[i]>.99) {break}
      i=i+1
  }
  no=i  # number of eigenvectors to explain 99% of variance
   
  Rot = pcal$rotation #rotation matrix
  PC = dataFile2%*%Rot[,1:no]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Create Data Partition Splitting into training and testing
  trainIndex1 <- createDataPartition(datafile[,OutlierColumn], p = .60, list = FALSE, times = 1) #for balanced sampling
  dataTrain1 <- datafile[ trainIndex1,]
  dataTest1 <- datafile[-trainIndex1,]
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

  # # # KNN 
  knn <- function(mat, k){
     n <- nrow(mat)
     if (n <= k) stop("k can not be more than n-1")
     neigh <- matrix(0, nrow = n, ncol = k)
     for(i in 1:n) {
        euc.dist <- colSums((mat[i, ] - t(mat)) ^ 2)  
        neigh[i, ] <- order(euc.dist)[2:(k + 1)]
     }
     return(neigh)
  }

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Change a particular column name
  dat <- data.frame(a = 1:3, b = 1:3, c = 1:3)
  colnames(dat)[grepl("b", colnames(dat))]
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  # # # Order by all the columns
  df[do.call(order, as.list(df)), ]
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

  # # # Put the Outlier column at the end
  dataFile = subset(dataFile, select = c(setdiff(colnames(dataFile), "Y"), "Y"))
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Take a sample of positives and sample of negatives each time
  df = iris
  df = df[df$Species != "setosa", ]
  df$Species = as.numeric(df$Species=="versicolor")
  positives = df[df$Species == 1,];
  normals = df[df$Species == 0,];
  
  data = rbind(positives[sample(nrow(positives), 30), ], negatives[sample(nrow(negatives), 30), ])
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # How to plot multiple charts in the same plot
  plot(rus_rocData$fpr,rus_rocData$tpr,type="l",col="green",ann=F)	
  par(new=T)
  plot(smote_rocData$fpr,smote_rocData$tpr,type="l",col="red",ann=F)
  
  # # # How to plot two plots in tow different plots
  plot(x1,y1)
  x11()
  plot(x2,y2)
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Shuffle a dataframe
  data1 = data1[sample(nrow(data1)), ]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Pick Random Data from a Dataframe (Say 150 rows)
  data1 = data1[sample(nrow(data1), 150), ]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Create quantiles
  N = 10 ; # No . of quantiles required
  data$quantile = ntile(data$score, N) # here it forms quantiles based on the score column

  # # # Divide data into quantiles
  ntiles<-quantile(1:nrow(data),seq(0,1,0.01))	#Creates 100 quantiles

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Get Great Decision Tree Plot
  library(rattle)
  rattle()
  library(rpart.plot)
  library(RColorBrewer)
  fancyRpartPlot(tree)	#tree=rpart(Y~., data=myTrainData,method="class")
  
  # # # Pruning using Complexity Parameter
  printcp(tree)
  plotcp(tree)
  # # From the above mentioned list of cp values, we can select the one having the least cross-validated error and use it to prune the tree.
  # # The value of cp should be least, so that the cross-validated error rate is minimum.
  # # Say fit = tree; i.e Rpart Object
  # # To select that -> fit$cptable[which.min(fit$cptable[ , "xerror"]), "CP"]
  ptree = prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"])
  fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # K-Means Optimum No.of Clusters
  kmeans.wss.k <- function(crime, k)
  {
   km = kmeans(crime, k)
   return (km$tot.withinss)
  }
  
  kmeans.dis <- function(crime, maxk)
  {
   dis=(nrow(crime)-1)*sum(apply(crime,2,var))
   dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime=crime)
   return(dis)
  }
  maxk = 10
  dis = kmeans.dis(crime, maxk);
  plot(1:maxk, dis, type='b', xlab="Number of Clusters", ylab="Distortion", col="blue")
  # # Select the point beyond which the change in distortion is constant
  # # For animation
  library(animation)
  cl = kmeans.ani(crime, 4)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # #list of rows with missing values
  data[!complete.cases(data),]
  
  # # # list of columns with missing values
  data[,!complete.cases(data)]
  
  # # # If any missing values are there omit them
  data <- na.omit(data,na.action=TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Function to replace outliers by some value
  library(data.table)
  outlierReplace = function(dataframe, cols, rows, newValue = NA) 
  {
   if (any(rows)) 
   {
   	set(dataframe, rows, cols, newValue)
   }
  }
  # # Example
  outlierReplace(data, "colname", which(data$colname > 1.5), 1.5)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # K Means distance from center ? (EUCLIDEAN)
  kmeans_fit = kmeans(PC, 4)
  centers = kmeans_fit$centers;
  PC$cluster = kmeans_fit$cluster;
  # Euclidean Distance
  euc.dist = function(x1, x2) {	sqrt(sum((x1 - x2) ^ 2))	}
  euc.dist(centers[4,],PC[2,-8])	# -8 because 8th column is cluster
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # K N N optimal K ?
  train<-read.csv("sonar_train.csv",header=FALSE)
  y<-as.factor(train[,61])
  x<-train[,1:60]
  		
  test<-read.csv("sonar_test.csv",header=FALSE)
  y_test<-as.factor(test[,61])
  x_test<-test[,1:60]
  
  train_error <- rep(0, 10)
  test_error <- rep(0, 10)
  
  library(class)
  
  for(i in 1:10){
  	fit<-knn(x,x,y,k=i)
  	train_error[i] <- 1-sum(y==fit)/length(y)
  	
  	fit_test<-knn(x,x_test,y,k=i)
  	test_error[i] <- 1-sum(y_test==fit_test)/length(y_test)	
  }
  
  plot(train_error, type="o", col="blue", ylim=c(0,0.5), xlab='K', ylab='Error', main='Atul Kumar')
  lines(test_error, type="o", pch=22, lty=2, col="red", ylim=c(0,0.5))
  legend(1,0.50,c("Training Error","Test Error"), col=c("blue","red"),lwd=c(1,4))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # # # Calculate Lift
  Calc_Lift = function(Logistic_Output,test,name)
  {
  	
  	Predict = data.frame(Predicted=Logistic_Output);
  	Data=data.frame(Actual_Y=test$Y,Score=Predict$Predicted)
  	library(dplyr)
  	Data=Data[order(Data$Score),]
  	Data$Decile=ntile(Data$Score,10)
  	P_Min=numeric()
  	P_Max=numeric()
  	Total_1=integer()
  	Total_0=integer()
  	for(i in 1:10)
  	{
  		P_Min=c(P_Min,min(Data$Score[Data$Decile==i]))
  		P_Max=c(P_Max,max(Data$Score[Data$Decile==i]))
  		Total_0=c(Total_0,length(Data$Score[Data$Decile==i & Data$Actual_Y==0]))
  		Total_1=c(Total_1,length(Data$Score[Data$Decile==i & Data$Actual_Y==1]))
  	}
  	Dev=data.frame(
  					Rank=order(as.integer(names(table(Data$Decile)))),
  					P_Min=P_Min,
  					P_Max=P_Max,
  					Total=Total_1+Total_0,
  					Ones=Total_1,
  					Zeroes=Total_0);
  					
  	Dev=Dev[order(Dev$Rank,decreasing=T),]
  	Dev$Cumilative_Ones=cumsum(Dev$Ones)
  	Dev$Cumilative_Zeroes=cumsum(Dev$Zeroes)
  	Dev$Cumilative_Ones_Perc=Dev$Cumilative_Ones/sum(Dev$Ones)
  	Dev$Cumilative_Zeroes_Perc=Dev$Cumilative_Zeroes/sum(Dev$Zeroes)
  	Dev$KS=Dev$Cumilative_Ones_Perc-Dev$Cumilative_Zeroes_Perc;
  	Dev$EVENT=(Dev$Ones/sum(Dev$Ones))
  	Dev$GAIN=cumsum(Dev$EVENT)
  	Dev$Total_Perc=(Dev$Total/sum(Dev$Total))
  	Dev$Cumilative_Total_Perc=cumsum(Dev$Total_Perc)
  	Dev$LIFT=Dev$GAIN/Dev$Cumilative_Total_Perc
  	
  	Dev$Cumilative_Ones_Perc=round(Dev$Cumilative_Ones_Perc*100)
  	Dev$Cumilative_Zeroes_Perc=round(Dev$Cumilative_Zeroes_Perc*100)
  	Dev$EVENT=round(Dev$EVENT*100)
  	Dev$GAIN=round(Dev$GAIN*100)
  	Dev$Total_Perc=round(Dev$Total_Perc*100)
  	Dev$Cumilative_Total_Perc=round(Dev$Cumilative_Total_Perc*100)
  	
  	write.csv(Dev,name,row.names=F)
  }
  Calc_Lift(Logistic_Output,Analysis_Data2,"LIFT.csv");
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

