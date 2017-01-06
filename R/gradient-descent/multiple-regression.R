# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               File Owner - Kartheek Palepu            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Loss function = half-MSE # #
# # minimize the loss

loss = function(y, yhat) {
	se = sum((yhat - y) ^ 2)
	mse = se / (2.0 * length(y))
	return(mse)
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

yEQ = function(x, intercept, slopes) {
	mx = as.matrix(x) %*% diag(slopes)
	mxc = rowSums(mx) + intercept
	return(mxc)
}


# # # Define the hyper parameters
# # learning rate
alpha = 0.001
# # threshold error rate
threshold = 0.0001

# # generate x
x1 = 1:10
x2 = seq(1, 20, 2)
x3 = seq(0, 19, 2)
x = data.frame(x1, x2, x3)

# # formulate original 'y' i.e. y = 1 + x1 + 2*x2 + x3
slopes = c(1, 2, 1)
intercept = 1
y = yEQ(x, intercept = intercept, slopes = slopes)

# # initialize values
slopes = rep(0, ncol(x))
intercept = 0

# # start iterations
for(i in 1:10000){
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
	if((i - 1) %% 100 == 0){
		print(paste0("At iteration #: ", i, "- At Slopes [", paste(slopes, collapse = ", "), "] and intercept = ", intercept, " loss is ", loss(y, yhat)))
	}
	intercept = intercept - alpha * derivative1(y, yhat)
	for(j in 1:length(slopes)){
		slopes[j] = slopes[j] - alpha * derivativeN(y, yhat, x[j])
	}
}