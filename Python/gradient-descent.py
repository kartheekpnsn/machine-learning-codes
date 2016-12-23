# # Loss function = x^2 # #
# # minimize the los
def loss(x):
	return(x**2)
def derivative(x):
	return(2*x)

alpha = 0.01 # learning rate
initial = 3 # initial value (usually random)
threshold = 0.000001 # threshold to come out of iterations
for i in range(10000):
	if loss(initial) <= threshold:
		print "== ENDED at " + str(i) + " =="
		print initial
		print loss(initial)
		break;
	print str(initial) + " - " + str(loss(initial))
	initial = initial - (alpha * derivative(initial))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # Loss function = MSE # #
# # minimize the loss
def loss(y, yhat):
	se = round(sum([(yhat[i] - y[i])**2 for i in range(len(y))]), 4)
	mse = se/(2.0 * len(y))
	return mse

def derivative1(y, yhat):
	d = sum([yhat[i] - y[i] for i in range(len(y))])
	d = d / float(len(y))
	return(d)
def derivative2(y, yhat, x):
	d = sum([((yhat[i] - y[i]) * x[i]) for i in range(len(y))])
	d = d / float(len(y))
	return(d)
def yEQ(x, theta0, theta1):
	return [(theta0 + (i * theta1)) for i in x]
alpha = 0.01
threshold = 0.0001
# let y = 3 + 2x (theta0 + theta1 * x)
x = range(1, 11)
y = yEQ(x, theta0 = 3, theta1 = 2) # [3, 5, 7, 9, 11, 13, 15, 17, 19, 21]
theta0 = 0
theta1 = 0
for i in range(10000):
	print "ITERATION - " + str(i)
	yhat = yEQ(x, theta0, theta1)
	if loss(y, yhat) <= threshold:
		print "== ENDED at " + str(i) + " =="
		print theta0, theta1
		print loss(y, yhat)
		break;
	print "For " + str(theta0) + " & " + str(theta1) + ": Loss is " + str(loss(y, yhat))
	theta0 = round(theta0 - alpha * derivative1(y, yhat), 4)
	theta1 = round(theta1 - alpha * derivative2(y, yhat, x), 4)