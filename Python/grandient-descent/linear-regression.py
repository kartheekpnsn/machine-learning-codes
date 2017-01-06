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

def derivative2(y, yhat, x):
	d = sum([((yhat[i] - y[i]) * x[i]) for i in range(len(y))])
	d = d / float(len(y))
	return(d)

def yEQ(x, theta0, theta1):
	return [(theta0 + (i * theta1)) for i in x]


def plot(oX, oY, pX, pY, slope, y_intercept, text = ""):
	import matplotlib.pyplot as plt
	# # plot 1st plot
	fig, (ax1, ax2) = plt.subplots(1, 2)
	ax1.plot(oX, oY, "o")
	ax1.plot(pX, pY, 'k-', lw = 2)
	ax1.set_xlim([min(oX) - 3, max(oX) + 3])
	ax1.set_ylim([min(oY) - 5, max(oY) + 5])
	if text != "":
		ax1.annotate(text, xy=(min(oX), max(oY) - 2), xytext=(min(oX), max(oY) - 2))
	ax1.annotate('slope = ' + str(slope[-1]) + ', intercept = ' + str(y_intercept[-1]), xy=(min(oX) + 1, min(oY) + 1), xytext=(min(oX) + 1, min(oY) + 1))

	# # plot 2nd plot
	ax2.plot(slope, y_intercept, "o")
	ax2.plot(slope, y_intercept, 'k-', lw = 2)
	ax2.set_xlim([min(slope) - 3, max(slope) + 3])
	ax2.set_ylim([min(y_intercept) - 5, max(y_intercept) + 5])
	plt.show()


# # # Define the hyper parameters
# # learning rate
alpha = 0.001
# # threshold error rate
threshold = 0.0001

# # # formulate original 'y' i.e. y = 3 + 2*x (i.e. theta0 + theta1 * x)
x = range(1, 10)
y = yEQ(x, theta0 = 3, theta1 = 2)

# # initialize values
theta0 = 0
theta1 = 0
slope = []
y_intercept = []
plotFlag = True

# # start iterations
for i in range(10000):
	print "ITERATION # " + str(i)
	yhat = yEQ(x, theta0, theta1)
	slope.append(theta1)
	y_intercept.append(theta0)
	if plotFlag:
		if i%1000 == 0:
			plot(x, y, x, yhat, slope, y_intercept)
	if loss(y, yhat) <= threshold:
		print "=============================================================================="
		print "=============================================================================="
		print "== Converged at " + str(i) + " =="
		print "- Slopes = " + str(theta1)
		print "- Intercept = " + str(theta0)
		print "- Y values = " + ", ".join([str(k) for k in y])
		print "- Yhat values = " + ", ".join([str(k) for k in yhat])
		print "- Loss = " + str(loss(y, yhat))
		if plotFlag:
			plot(x, y, x, yhat, slope, y_intercept, text = "Iterations ended at = " + str(i) + ", with loss = " + str(loss(y, yhat)))
		break;
	print "For " + str(theta0) + " & " + str(theta1) + ": Loss is " + str(loss(y, yhat))
	theta0 = round(theta0 - alpha * derivative1(y, yhat), 4)
	theta1 = round(theta1 - alpha * derivative2(y, yhat, x), 4)
