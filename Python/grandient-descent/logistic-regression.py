#!/usr/bin/env python
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               File Owner - Kartheek Palepu            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

import math

# # minimize the loss
def loss(y, yhat):
	term1 = [y[i] * math.log(yhat[i]) for i in range(len(y))]
	term2 = [(1 - y[i]) * math.log((1 - yhat[i])) for i in range(len(y))]
	term3 = sum([term1[i] + term2[i] for i in range(len(term1))])
	term4 = -term3/float(len(y))
	return term4

def derivative1(y, yhat):
	d = sum([yhat[i] - y[i] for i in range(len(y))])
	d = d / float(len(y))
	return(d)

def derivativeN(y, yhat, x_n):
	d = sum([((yhat[i] - y[i]) * x_n[i]) for i in range(len(y))])
	d = d / float(len(y))
	return(d)

# # sigmoid function
def yEQ(x, intercept, slopes, flag = False):
	mx = [[j * slopes[i] for j in x[i]] for i in range(len(x))]
	mxc = [intercept + sum(i) for i in zip(*mx)]
	if flag:
		print mxc
	yhat = [(1/ (1 + math.exp(-i))) for i in mxc]
	yhat = [0.9999999999999999 if i == 1.0 else i for i in yhat]
	return yhat

# # hyper parameters
alpha = 0.01
threshold = 0.001

# # initializations
# # generate x
x = []
x1 = range(1, 11)
x2 = range(1, 20, 2)
x3 = range(0, 19, 2)
x.append(x1)
x.append(x2)
x.append(x3)

# # formulate original 'y' i.e. y = 1/(1 + exp(-(1 + x1 + 2*x2 + x3)))
slopes = [1, -2, 1]
intercept = 1
y = [round(i) for i in yEQ(x, intercept = intercept, slopes = slopes)]

# # initialize values
slopes = [0] * len(x)
intercept = 0

# # start iterations
for i in range(10000000):
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
	if i % 1000 == 0:
		print "FINISHED ITERATION # " + str(i)
		print "At Slopes [" + ", ".join([str(k) for k in slopes]) + "] and intercept = " + str(intercept) + " loss is " + str(loss(y, yhat))
	intercept = round(intercept - alpha * derivative1(y, yhat), 6)
	for j in range(len(slopes)):
		slopes[j] = round(slopes[j] - alpha * derivativeN(y, yhat, x[j]), 6)