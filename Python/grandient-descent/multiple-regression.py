#!/usr/bin/env python
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               File Owner - Kartheek Palepu            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Loss function = half-MSE # #
# # minimize the loss
def loss(y, yhat):
	se = round(sum([(yhat[i] - y[i])**2 for i in range(len(y))]), 6)
	mse = se/(2.0 * len(y))
	return mse

def derivative1(y, yhat):
	d = sum([yhat[i] - y[i] for i in range(len(y))])
	d = d / float(len(y))
	return(d)

def derivativeN(y, yhat, x_n):
	d = sum([((yhat[i] - y[i]) * x_n[i]) for i in range(len(y))])
	d = d / float(len(y))
	return(d)

def yEQ(x, intercept, slopes):
	mx = [[j * slopes[i] for j in x[i]] for i in range(len(x))]
	mxc = [intercept + sum(i) for i in zip(*mx)]
	return mxc


# # # Define the hyper parameters
# # learning rate
alpha = 0.001
# # threshold error rate
threshold = 0.0001

# # generate x
x = []
x1 = range(1, 11)
x2 = range(1, 20, 2)
x3 = range(0, 19, 2)
x.append(x1)
x.append(x2)
x.append(x3)

# # formulate original 'y' i.e. y = 1 + x1 + 2*x2 + x3
slopes = [1, 2, 1]
intercept = 1
y = yEQ(x, intercept = intercept, slopes = slopes)

# # initialize values
slopes = [0] * len(x)
intercept = 0

# # start iterations
for i in range(10000):
	print "ITERATION # " + str(i)
	yhat = yEQ(x, intercept = intercept, slopes = slopes)
	if loss(y, yhat) <= threshold:
		print "=============================================================================="
		print "=============================================================================="
		print "== Converged at " + str(i) + " =="
		print "- Slopes = " + ",".join([str(k) for k in slopes])
		print "- Intercept = " + str(intercept)
		print "- Y values = " + ", ".join([str(k) for k in y])
		print "- Yhat values = " + ", ".join([str(k) for k in yhat])
		print "- Loss = " + str(loss(y, yhat))
		break;
	print "At Slopes [" + ", ".join([str(k) for k in slopes]) + "] and intercept = " + str(intercept) + " loss is " + str(loss(y, yhat))
	intercept = round(intercept - alpha * derivative1(y, yhat), 6)
	for j in range(len(slopes)):
		slopes[j] = round(slopes[j] - alpha * derivativeN(y, yhat, x[j]), 6)
