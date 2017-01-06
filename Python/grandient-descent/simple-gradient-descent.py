#!/usr/bin/env python
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               File Owner - Kartheek Palepu            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # Requirements - matplotlib # #
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