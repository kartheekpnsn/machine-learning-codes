from math import *
import pandas as pd
import numpy as np

def createDataPartition(target, p = 0.75, valid = False):
    import numpy as np
    if not isinstance(target, np.ndarray):
        target = np.array(target)
    u_classes = np.unique(target)        
    n_classes = len(u_classes)
    index = []
    for eachClass in u_classes:
        i = np.where(target == eachClass)[0]
        np.random.shuffle(i)
        i = i[:int(round(len(i) * p))]
        index.extend(i.tolist())
    index = np.array(index)
    np.random.shuffle(index)
    conv_int = np.vectorize(lambda x : int(x))
    train_index = conv_int(index)
    test_index = np.array(list(set(range(len(target))) - set(train_index)))
    return train_index, test_index

# df = pd.DataFrame({'a' : range(100), 'b' : np.random.random(100), 'c': np.round(np.random.random(100))})
# train_index, test_index = createDataPartition(df.c)
# print df.loc[train_index].shape
# print df.loc[test_index].shape

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
