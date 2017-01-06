# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               File Owner - Kartheek Palepu            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Loss function = x^2 # #
# # minimize the loss
loss = function(x) {
	return(x^2)
}

derivative = function(x) {
	return(2 * x)
}

# # define hyper parameters
alpha = 0.01 # learning rate
threshold = 0.000001 # threshold to come out of iterations

initial = 3 # initial value (usually random)
for(i in 1:10000){
	if(loss(initial) <= threshold) {
		print("==============================================================================")
		print("==============================================================================")
		print(paste0("== Converged at " , i , " =="))
		print(paste0("- Value = ", initial))
		print(paste0("- Loss = " , loss(initial)))
		break;
	}
	if((i-1) %% 100 == 0) {
		print(paste0("At Iteration # ", i))
		print(paste0("- Value = ", initial))
		print(paste0("- Loss = " , loss(initial)))
	}
	initial = initial - (alpha * derivative(initial))
}