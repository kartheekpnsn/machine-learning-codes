from math import *
import pandas as pd
import random

# df = pd.DataFrame({'a' : range(10), 'b' : range(1,11), 'c': [round(random.random())for i in range(10)]})

def createDataPartition(df, p = 0.75, target = 'Y', rand_flag = True):
	import numpy as np
	import random
	ones = sum([1 for i in df[target] if i == 1])
	zeroes = len(df[target]) - ones
	train_ones = int(round(ones * p))
	train_zeroes = int(round(zeroes * p))
	ones = df[df[target] == 1]
	zeroes = df[df[target] == 0]
	if rand_flag:
		ones = ones.reindex(np.random.permutation(ones.index))
		zeroes = zeroes.reindex(np.random.permutation(zeroes.index))
	train = list(ones[:train_ones].index) + list(zeroes[:train_zeroes].index)
	random.shuffle(train)
	return train

# index = createDataPartition(df, p = 0.5, target = 'c')
# train = df.iloc[[i for i in index]]
# test = df.iloc[[i for i in df.index if i not in index]]



# # == Function to calculate similarity measures == # #
# # Parameters
# ob1 = object 1
# ob2 = object 2
# measure = measure that you want to use for calculating similarity [euc, jac, cos, pearson, man]
# # where:
# # # euc = euclidean distance [lesser the better]
# # # jac = jaccard distance [lesser the better]
# # # man = manhattan distance [lesser the better]
# # # cos = cosine similiarity measure [more the better (-1 to 1)]
# # # pearson = pearsons correlation coefficient [more the better (-1 to 1)]
def similarityMeasure(ob1, ob2, measure = "pearson"):
	if measure == "euc": # euclidean distance - lesser the better
		return sqrt(sum([(i-j)^2 for i,j in zip(ob1, ob2)]))
	elif measure == "jac": # jaccard distance - lesser the better
		# # intersect(ob1, ob2)/union(ob1, ob2) - Hamming score
		ob1 = [i for i in range(len(ob1)) if i != 0]
		ob2 = [i for i in range(len(ob2)) if i != 0]
		return len(set(ob1).intersection(set(ob2)))/float(len(set(ob1).union(set(ob2))))
	elif measure == "man": # manhattan distance - lesser the better
		return sum([abs(i - j) for i, j in zip(ob1, ob2)])
	elif measure == "cos": # cosine similarity measure
		numerator = sum([i * j for i, j in zip(ob1, ob2)])
		denominator = sqrt(sum(map(lambda x: x ** 2, ob1))) * sqrt(sum(map(lambda x: x ** 2, ob2)))
		return(numerator/float(denominator))
	elif measure == "pearson": # pearsons correlation coefficient
		# cosine - mean
		ob1 = [i - sum(ob1)/float(len(ob1)) for i in ob1]
		ob2 = [i - sum(ob2)/float(len(ob2)) for i in ob2]
		return(similarityMeasure(ob1, ob2, measure = "cos"))
