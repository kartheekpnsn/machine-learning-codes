import pandas as pd
import random

df = pd.DataFrame({'a' : range(10), 'b' : range(1,11), 'c': [round(random.random())for i in range(10)]})

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

index = createDataPartition(df, p = 0.5, target = 'c')
train = df.iloc[[i for i in index]]
test = df.iloc[[i for i in df.index if i not in index]]

print train.shape
print test.shape
print train
print test
print df
