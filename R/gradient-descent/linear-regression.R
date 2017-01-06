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

derivative2 = function(y, yhat, x, bias = 1) {
	d = sum((yhat - y) * x)
	d = d * (bias / length(y)) # let bias term = 1
	return(d)
}

yEQ = function(x, theta0, theta1) {
	return((theta1 * x) + theta0)
}

# # # Define the hyper parameters
# # learning rate
alpha = 0.001
# # threshold error rate
threshold = 0.0001

# # # formulate original 'y' i.e. y = 3 + 2*x (i.e. theta0 + theta1 * x)
x = 1 : 10
y = yEQ(x, theta0 = 3, theta1 = 2)

# # initialize values
theta0 = 0
theta1 = 0

# # start iterations
for(i in 1:100000) {
	yhat = yEQ(x, theta0, theta1)
	if(loss(y, yhat) <= threshold) {
		print("==============================================================================")
		print("==============================================================================")
		print(paste0("== Converged at ", i, " =="))
		print(paste0("- Slopes = ", theta1))
		print(paste0("- Intercept = ", theta0))
		print(paste0("- Y values = ", paste(y, collapse = ", ")))
		print(paste0("- Yhat values = ", paste(yhat, collapse = ", ")))
		print(paste0("- Loss = ", loss(y, yhat)))
		break;
	}
	if((i-1) %% 100 == 0) {
		print(paste0("At iteration: ", i, ": For ", theta0, " & ", theta1, ": Loss is ", loss(y, yhat)))
	}
	theta0 = theta0 - alpha * derivative1(y, yhat)
	theta1 = theta1 - alpha * derivative2(y, yhat, x)
}
