# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               File Owner - Kartheek Palepu            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # minimize the loss
loss = function(y, yhat) {
	term1 = y * log(yhat)
	term2 = (1 - y) * log((1 - yhat))
	term3 = sum(term1 + term2)
	term4 = - term3 * (1 / length(y))
	return(term4)
}

derivative1 = function(y, yhat, bias = 1) {
	d = sum(yhat - y)
	d = d * (bias / length(y)) # let bias term = 1
	return(d)
}

derivativeN = function(y, yhat, x_n, bias = 1) {
	d = sum((yhat - y) * x_n)
	d = d * (bias / length(y)) # let bias term = 1
	return(d)
}

# # sigmoid function
yEQ = function(x, intercept, slopes) {
	mx = as.matrix(x) %*% diag(slopes)
	mxc = rowSums(mx) + intercept
	yhat = (1 / (1 + exp(-mxc)))
	yhat = ifelse(yhat == 1, 0.9999999, yhat)
	return(yhat)
}

# # hyper parameters
alpha = 0.01
threshold = 0.001

# # initializations
# # generate x
x1 = 1:10
x2 = seq(1, 20, 2)
x3 = seq(0, 19, 2)
x = data.frame(x1, x2, x3)

# # formulate original 'y' i.e. y = 1/(1 + exp(-(1 + x1 - 2*x2 + x3)))
slopes = c(1, -4, 4)
intercept = 1
y = round(yEQ(x, intercept, slopes))

# # initialize values
slopes = rep(0, ncol(x))
intercept = 0

# # start iterations
for(i in 1:10000000) {
	yhat = yEQ(x, intercept = intercept, slopes = slopes)
	if(loss(y, yhat) <= threshold) {
		print("==============================================================================")
		print("==============================================================================")
		print(paste0("== Converged at ", i, " =="))
		print(paste0("- Slopes = ", paste(slopes, collapse = ", ")))
		print(paste0("- Intercept = ", intercept))
		print(paste0("- Y values = ", paste(y, collapse = ", ")))
		print(paste0("- Yhat values = ", paste(yhat, collapse = ", ")))
		print(paste0("- Loss = ", loss(y, yhat)))
		break;
	}
	if((i-1) %% 1000 == 0) {
		print(paste0("FINISHED ITERATION # ", i))
		print(paste0("At Slopes [", paste(slopes, collapse = ", "), "] and intercept = ", intercept, " loss is ", loss(y, yhat)))
	}
	intercept = round(intercept - alpha * derivative1(y, yhat), 6)
	for(j in 1:length(slopes)) {
		slopes[j] = round(slopes[j] - alpha * derivativeN(y, yhat, x[j]), 6)	
	}
}
