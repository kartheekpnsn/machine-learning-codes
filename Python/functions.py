# load required packages
import math
import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.metrics import accuracy_score, fbeta_score, precision_score, recall_score
from sklearn.metrics import mean_squared_error, median_absolute_error, r2_score, silhouette_score
from sklearn.model_selection import train_test_split
from sklearn.cross_validation import KFold
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, GradientBoostingClassifier
from sklearn.ensemble import RandomForestRegressor, AdaBoostRegressor, GradientBoostingRegressor
from sklearn.datasets import make_classification, make_regression
from sklearn.cluster import KMeans, DBSCAN
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.decomposition import PCA
from imblearn.under_sampling import RandomUnderSampler
from imblearn.over_sampling import SMOTE
from sklearn.feature_selection import SelectKBest, chi2, RFE, SelectFromModel

def data_process(data, target_name = 'Y'):
	"""
	Function to divide into X and Y from the data
	Parameters:
		data: input data
		target_name: target variable(column) name
	"""
	print('>> Started Data Processing')
	X = data[data.columns[np.where(data.columns != target_name)[0]]]
	Y = data[data.columns[np.where(data.columns == target_name)[0]]]
	return({'X' : X, 'y' : Y})

def encoding_features(data, label_encoding = False):
	"""
	Function to convert categorical variables to numeric (one hot encoding / label encoding)
	Parameters:
		data: input data
		label_encoding: flag to indicate to do label encoding if false it does one hot encoding
	"""
	le = LabelEncoder()
	for col in data.columns.values:
		# Encoding only categorical variables
		if data[col].dtypes == 'object':
			if label_encoding:
				print('>> Label Encoding for: ' + col)
				le.fit(data[col].values)
				data[col] = le.transform(data[col])
			else:
				print('>> One Hot Encoding for: ' + col)
				temp = pd.get_dummies(data[col])
				data = data.drop(col, axis = 1)
				data = data.join(temp)
	return data

def data_sampling(X, y, method = 'rus', kind = 'regular', seed = 294056):
	"""
	Function to balance classes using random under sampling or smote
	Parameters:
		X: the independent variable values in a numpy array / pandas dataframe
		y: the target variable valeus in a numpy array / pandas dataframe
		method: currently accepts: 'rus' or 'smote'
		kind: parameter for smote where kind = ['regular', 'borderline1', 'borderline2', 'svm']
		seed: A random seed to maintain reproducability
	"""
	if method == 'smote':
		sm = SMOTE(kind = kind)
		X_res, y_res = sm.fit_sample(X, y)
	else:
		rus = RandomUnderSampler(return_indices = True, random_state = seed)
		X_res, y_res, idx_res = rus.fit_sample(X, y)
	return {'X' : X_res, 'y' : y_res}


def get_cutoff(predicted, actual, measure = 'fscore', beta = 5):
	"""
	- Function to get best cutoff(threshold) from predicted probabilities w.r.t. actual
	- Note: Works only for 2-class classification
	- Parameters:
		predicted: predicted probabilities
		actual: actual class values
		measure: choose either 'fscore' or 'accuracy' (as of now)
		beta: instead of simple f1-score we chose f-beta-score
	"""
	print('>> Started the process of selection of threshold')
	if not isinstance(predicted, np.ndarray):
		predicted = np.array(predicted)
	if not isinstance(actual, np.ndarray):
		actual = np.array(actual)

	if len(np.unique(predicted)) <= 2:
		print('ERROR: Predicted values should be given in probabilities not as classes')
	
	conv_int = np.vectorize(lambda x : int(x))
	
	metric = []
	cutoffs = np.arange(0, 1.01, 0.01)
	for each_cutoff in cutoffs:
		predicted_class = conv_int(predicted >= each_cutoff)
		if measure == 'fscore':
			metric.append(fbeta_score(actual, predicted_class, average = 'micro', beta = beta))
		else:
			metric.append(accuracy_score(actual, predicted_class))
	metric = np.array(metric)
	fcutoff = round(cutoffs[np.argmax(metric)], 2)
	print('>> Chosen Cutoff: ' + str(fcutoff))
	return fcutoff

def performance_measure(predicted, actual, optimal_threshold = False, threshold = 0.5, how = 'fscore', regression = False, beta = 5):
	"""
	- Function to return performance measures from predicted values and actual values
	- Note: Works only for 2-class classification and regression problems
	- Parameters:
		predicted: predicted probabilities/regressed values
		actual: actual class values/actual continuous values
		optimal_threshold: A flag to control the selection of optimal threshold value in case of classification
		threshold: Default value of threshold for probabilities in case of classification
		how: A measure used to optimize the threshold value. Choose either 'fscore' or 'accuracy' (as of now)
		regression: A flag indicating it is a regression problem
		beta: instead of simple f1-score we chose f-beta-score
	"""
	if not isinstance(predicted, np.ndarray):
		predicted = np.array(predicted)
	if not isinstance(actual, np.ndarray):
		actual = np.array(actual)

	if regression:
		print('>> Starting to calculate performance measures for regression')
		mae = median_absolute_error(actual, predicted)
		mse = mean_squared_error(actual, predicted)
		r2 = r2_score(actual, predicted)
		rmse = math.sqrt(mse)
		return {'MAE' : mae, 'MSE' : mse, 'RMSE' : rmse, 'R2' : r2}
	else:
		print('>> Starting to calculate performance measures for classification')
		if optimal_threshold:
			threshold = get_cutoff(predicted = predicted, actual = actual, measure = how, beta = beta)
		conv_int = np.vectorize(lambda x : int(x))
		predicted_class = conv_int(predicted >= threshold)
		accuracy = accuracy_score(actual, predicted_class)
		precision = precision_score(actual, predicted_class, average = 'micro')
		recall = recall_score(actual, predicted_class, average = 'micro')
		fscore = fbeta_score(actual, predicted_class, average = 'micro', beta = beta)
		return {'Accuracy' : accuracy, 'Precision' : precision, 'Recall' : recall, 'Fscore' : fscore}

def build_model(model, X, y, data = None, target_name = 'Y', beta = 5, optimal_threshold = True, regression = False, seed = 294056, k_folds = 0):
	"""
	- Function to build a machine learning model for the task of regression/classification
	- Parameters:
		model: the model object from sklearn
		X: the independent variable values in a numpy array / pandas dataframe
		y: the target variable valeus in a numpy array / pandas dataframe
		data: the entire input data (it divides into X, y)
		target_name: target variable(column) name for dividing the data into X, y
		beta: instead of simple f1-score we chose f-beta-score
		optimal_threshold: A flag to control the selection of optimal threshold value in case of classification
		threshold: Default value of threshold for probabilities in case of classification
		regression: A flag indicating it is a regression problem
		seed: A random seed to maintain reproducability
		k_folds: for K fold cross validation [NEED TO ADD THIS !!]
	"""
	print('>> Started the process of model building')
	print('>> Chosen model: ' + str(type(model)))
	if data is not None:
		processed_data = data_process(data, target_name)
		X = processed_data['X']
		y = processed_data['y']
	X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.30, random_state = seed)
	if not regression:
		model.fit(X_train, y_train)
		importance = model.feature_importances_
		predicted = model.predict_proba(X_test)
		if not isinstance(predicted, np.ndarray):
			predicted = np.array(predicted)
		predicted = predicted[:, 1] # for a 2-class problem
		performance = performance_measure(predicted = predicted, actual = y_test, optimal_threshold = optimal_threshold, beta = beta)
		return {'fit' : model, 'importance' : importance, 'predicted' : predicted, 'performance' : performance}
	else:
		model.fit(X, y)
		importance = model.feature_importances_
		predicted = model.predict(X_test)
		if not isinstance(predicted, np.ndarray):
			predicted = np.array(predicted)
		performance = performance_measure(predicted = predicted, actual = y_test, regression = True)
		return {'fit' : model, 'importance' : importance, 'predicted' : predicted, 'performance' : performance}

def xgboost_model(X, y, data = None, target_name = 'Y', beta = 5, optimal_threshold = True, regression = False, nclass = 2, seed = 294056, k_folds = 0):
	"""
	- Function to build a xgboost model for classification or regression
	- Parameters:
		X: the independent variable values in a numpy array / pandas dataframe
		y: the target variable valeus in a numpy array / pandas dataframe
		data: the entire input data (it divides into X, y)
		target_name: target variable(column) name for dividing the data into X, y
		beta: instead of simple f1-score we chose f-beta-score
		optimal_threshold: A flag to control the selection of optimal threshold value in case of classification
		threshold: Default value of threshold for probabilities in case of classification
		regression: A flag indicating it is a regression problem
		nclass: if multi-class problem this should be changed
		seed: A random seed to maintain reproducability
		k_folds: for K fold cross validation [NEED TO ADD THIS !!]
	"""
	if data is not None:
		processed_data = data_process(data, target_name)
		X = processed_data['X']
		y = processed_data['y']
	# X = encoding_features(X)
	X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.30, random_state = seed)
	dtrain = xgb.DMatrix(X_train, label = y_train, missing = -999.0)
	dtest = xgb.DMatrix(X_test, label = y_test, missing = -999.0)
	evallist = [(dtest, 'eval'), (dtrain, 'train')]
	param = {'max_depth': 2, 'eta': 0.5, 'silent': 1}
	num_round = 100
	if not regression:
		print('>> Started the process of xgboost for classification')
		if nclass == 2:
			param['objective'] = 'binary:logistic'
		elif nclass > 2:
			param['objective'] = "multi:softprob"
			param['num_class'] = nclass
		param['eval_metric'] = ['auc', 'logloss']
		bst = xgb.train(param, dtrain, num_round, evallist)
		predicted = bst.predict(dtest)
		performance = performance_measure(predicted = predicted, actual = y_test, optimal_threshold = optimal_threshold, beta = beta)
		return {'fit' : bst, 'predicted' : predicted, 'performance' : performance}
	else:
		print('>> Started the process of xgboost for regression')
		print(X_train)
		print(y_train)
		param['objective'] = 'reg:linear'
		# param['eval_metric'] = ['auc']
		bst = xgb.train(param, dtrain, num_round, evallist)
		predicted = bst.predict(dtest)
		performance = performance_measure(predicted = predicted, actual = y_test, regression = True)
		return {'fit' : bst, 'predicted' : predicted, 'performance' : performance}

def cluster_data(X, type = 'kmeans', n_clusters = 2, eps = 0.5, min_samples = 5, scale = True, seed = 294056):
	"""
	- Function to build a clustering model
	- Parameters:
		X: the input data in the form of pandas data frame or numpy array
		type: type of cluster (Currently we have kmeans/dbscan)
		n_clusters: A parameter that chooses number of cluster for kmeans clustering
		eps: A parameter that chooses size of the epsilon neighborhood in the case of dbscan
		min_samples: A parameter that chooses the number of minimum points in the eps region (for core points) in case of dbscan
		scale: A flag that allows us to standardize the data
		seed: For reproducability purpose
	- Silhouette Score:
		The best value is 1 and the worst value is -1. 
		Values near 0 indicate overlapping clusters. 
		Negative values generally indicate that a sample has been assigned to the wrong cluster, as a different cluster is more similar.
	"""
	if not isinstance(X, np.ndarray):
		X = np.array(X)

	if scale:
		X = StandardScaler().fit_transform(X)

	if type == 'kmeans':
		kmeans = KMeans(n_clusters = n_clusters, random_state = seed).fit(X)
		labels = kmeans.labels_
		centers = kmeans.cluster_centers_
		n_clusters = len(set(labels))
		silhouette = silhouette_score(X, labels)
		return {'fit' : kmeans, 'labels' : labels, 'centers' : centers, 'n_clusters' : n_clusters, 'silhouette_score' : silhouette}
	elif type == 'dbscan':
		dbscan = DBSCAN(eps = eps, min_samples = 5).fit(X)
		labels = dbscan.labels_
		n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
		if len(set(labels)) == 1:
			silhouette = -1.00
		else:
			silhouette = silhouette_score(X, labels)
		return {'fit' : dbscan, 'labels' : labels, 'n_clusters' : n_clusters, 'silhouette_score' : silhouette}
	else:
		print('Choose either "kmeans" or "dbscan" value for the parameter "type"')

def mvg_outliers(X, scale = True, perc_variance = 0.95, threshold = 1.5):
	"""
	- Function to build a clustering model
	- Parameters:
		X: the input data in the form of pandas data frame or numpy array
		scale: A flag indicating whether to scale the data or not
		perc_variance: choose the components that explain minimum perc_variance of variance
		threshold: threshold to choose outliers
	"""
	if not isinstance(X, np.ndarray):
		X = np.array(X)
	if scale:
		print('>> Scaling the data')
		scaler = StandardScaler().fit(X)
		X = scaler.transform(X)
	print('>> Applying PCA')
	pca_fit = PCA()
	pca_fit.fit(X)
	vars = pca_fit.explained_variance_
	props = vars/sum(vars)
	cumvar = np.cumsum(props)
	print('>> Cumulative Variance explained by each component is given below:')
	print(cumvar)
	temp = len(props)
	flag = True
	i = 0
	while flag:
		if cumvar[i] > perc_variance:
			flag = False
		else:
			i = i + 1
	no = i # number of components chosen
	print('>> Number of components chosen: ' + str(no + 1))
	PC = pca_fit.transform(X)
	PC = PC[:, 0:(no + 1)]
	pc_sd = np.std(PC, axis = 0) # standard deviation of all PCs
	pc_mean = np.mean(PC, axis = 0) # mean of all PCs
	pc_cov = np.cov(PC, rowvar = False)
	inv_pc_cov = np.linalg.inv(pc_cov)
	diff_pc = PC - pc_mean
	print('>> Calculating Mahalanobis Distance')
	md = []
	for i in range(len(diff_pc)):
		md.append(np.sqrt(np.dot(np.dot(np.transpose(diff_pc[i]),inv_pc_cov),diff_pc[i])))
	threshold = np.mean(md) * threshold # adjust threshold = 1.5 accordingly
	conv_int = np.vectorize(lambda x : int(x))
	outliers = conv_int(md > threshold)
	return {'md' : md, 'outliers' : outliers}

def feature_selection(X, y, method = 'kbest', model_fit = None):
	"""
	- Function to build a xgboost model for classification or regression
	- Parameters:
		X: the independent variable values in a numpy array / pandas dataframe
		y: the target variable valeus in a numpy array / pandas dataframe
		method: currently supports 'kbest' or 'rfe' or 'model' or 'forward' or 'backward'
		model: the model object from sklearn
	"""
	if not isinstance(X, np.ndarray):
		X = np.array(X)
	ncol = X.shape[1]
	if method == 'kbest':
		print('>> Started KBest feature selection')
		print ncol
		k = round(ncol * 0.7)
		print('>> Selecting ' + str(k) + " features") 
		X_new = SelectKBest(chi2, k = k).fit_transform(X, y)
		print('>> Removed ' + str(ncol - k) + ' features')
	elif method == 'rfe':
		k = round(ncol * 0.7)
		if model_fit is not None:
			print('>> Started Recurssive Feature Elimination Process')
			rfe = RFE(model_fit, k)
			fit = rfe.fit(X, y)
			print('>> Removed ' + str(ncol - k) + ' features')
			print(">> Feature Ranking: %s") % fit.ranking_
			selected_features = [y for x, y in zip(fit.support_, range(ncol)) if x]
			X_new = X[:, selected_features]
	elif method == 'model':
		if model_fit is not None:
			print('>> Started Feature selection using ML Model')
			model_fit.fit(X, y)
			model = SelectFromModel(model_fit, prefit = True)
			X_new = model.transform(X)
			print('>> Removed ' + str(ncol - X_new.shape[1]) + ' features out of ' + str(ncol) + ' features')
		else:
			print('>> model_fit parameter cannot be None')
	elif method == 'forward':
		pass
	elif method == 'backward':
		pass
	return X_new

def modelfit(alg, dtrain, predictors, useTrainCV = True, cv_folds = 5, early_stopping_rounds = 50, metrics = 'logloss', ):
	from sklearn import cross_validation, metrics
	if useTrainCV:
		xgb_param = alg.get_xgb_params()
		xgtrain = xgb.DMatrix(dtrain[predictors].values, label=dtrain['target'].values)
		cvresult = xgb.cv(xgb_param, xgtrain, num_boost_round=alg.get_params()['n_estimators'], nfold=cv_folds,
			metrics=metrics, early_stopping_rounds=early_stopping_rounds, verbose_eval=True)
		alg.set_params(n_estimators=cvresult.shape[0])

	# Fit the algorithm on the data
	alg.fit(dtrain[predictors], dtrain['target'], eval_metric='mlogloss')
        
	#Predict training set:
	dtrain_predictions = alg.predict(dtrain[predictors])
        
	#Print model report:
	print "\nModel Report"
	print "Accuracy : %.4g" % metrics.accuracy_score(dtrain['target'].values, dtrain_predictions)
                    
	feat_imp = pd.Series(alg.get_booster().get_fscore()).sort_values(ascending=False)
	feat_imp.plot(kind='bar', title='Feature Importances')
	plt.ylabel('Feature Importance Score')

def gridSearchCV_lgb_clf(train, predictor_variables, target, estimator, params):
	# Import LightGBM and sklearn LightGBM
	# import lightgbm as lgb
	# from lightgbm.sklearn import LGBMClassifier
	# LGBMClassifier(learning_rate=0.05,
	#                n_estimators=100,
	#                max_depth=10,
	#                num_leaves=32,
	#                max_bin=264,
	#                subsample=0.6,
	#                colsample_bytree=0.8,
	#                random_state=2017)
	# param_test = {
	# 'n_estimators':range(10,101,10)
	# }
	gsearch = GridSearchCV(estimator = estimator, param_grid = params, scoring = 'accuracy', iid = False, cv = 5)
	gsearch.fit(train[predictor_variables], target)
	print gsearch.best_params_
	print gsearch.best_score_

# # # === IMPLEMENTATION EXAMPLES === # # #

# # # == DATA PREPARATION == # #
# X_class, y_class = make_classification(n_samples=100, n_features=14, n_informative=2, n_redundant=0, random_state=0, shuffle=False)
# X_reg, y_reg = make_regression(n_features=4, n_informative=2, random_state=0, shuffle=False)

# # # == CLASSIFICATION == # #
# fit = build_model(model = RandomForestClassifier(), X = X_class, y = y_class)
# print(fit['performance'])
# print('=========================================')
# fit = build_model(model = AdaBoostClassifier(), X = X_class, y = y_class)
# print(fit['performance'])
# print('=========================================')
# fit = build_model(model = GradientBoostingClassifier(), X = X_class, y = y_class)
# print(fit['performance'])
# print('=========================================')

# # # == REGRESSION == # #
# fit = build_model(model = RandomForestRegressor(), X = X_reg, y = y_reg, regression = True)
# print(fit['performance'])
# print('=========================================')
# fit = build_model(model = AdaBoostRegressor(), X = X_reg, y = y_reg, regression = True)
# print(fit['performance'])
# print('=========================================')
# fit = build_model(model = GradientBoostingRegressor(), X = X_reg, y = y_reg, regression = True)
# print(fit['performance'])
# print('=========================================')

# # # == CLUSTERING == # #
# X = np.array([[1, 2], [1, 4], [1, 0], [4, 2], [4, 4], [4, 0]])
# print(cluster_data(X, n_clusters = 2))
# print(cluster_data(X, type = 'dbscan', eps = 0.3, min_samples = 3))

# # # == XGBOOST == # #
# print xgboost_model(X = X_class, y = y_class)
# print xgboost_model(X = X_reg, y = y_reg, regression = True)

# # # == MVG == # #
# print mvg_outliers(X = X_reg, threshold = 0.75)['md']

# # # == Feature Selection == # #
# print feature_selection(X = X_class, y = y_class, method = 'model', model_fit = AdaBoostClassifier())
# print feature_selection(X = X_class, y = y_class, method = 'rfe', model_fit = AdaBoostClassifier())

