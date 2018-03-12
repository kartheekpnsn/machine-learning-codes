# # # # === INCLUDES DATA CREATION + TARGET CREATION + MODEL === # # # #
# # You can clearly see how the target is actually created
# # You can find out how the target is influenced by features created !! 


# # # # == DATA CREATION == # # # #
# 15 columns
# 1 target (classification)
# 10 million rows


# columns:
# 5 Categorical
# 3 Dates
# 7 Numerical

# Target influenced by 
# 1 date
# 3 numerical
# 2 categorical

# outliers in: parch, dob
# induce missing
# shuffle rows, columns
# add id

library(data.table)
nrows = 2000000
set.seed(294056)
# functions
name_generate = function(nrows) {
	l1 = sample(letters[1:10], nrows, replace = TRUE)
	l2 = sample(letters[10:20], nrows, replace = TRUE)
	l3 = sample(letters[15:26], nrows, replace = TRUE)
	l4 = sample(letters, nrows, replace = TRUE)
	l5 = sample(letters[c(1:5, 10:15, 20:26)], nrows, replace = TRUE)
	name = paste0(l1, l2, l3, l4, l5)
	sur_names = c('Mr', 'Mrs', 'Ms', 'Madam', 'Master', 'Dr')
	last_names = c('sharma', 'varma', 'shaik', 'shetty', 'deere', 'jones', 'reddy', 'deshmukh')
	return(paste0(sample(sur_names, nrows, replace = TRUE), '. ', name, ' ', sample(last_names, nrows, replace = TRUE)))
}

city_generate = function(nrows) {
	l1 = sample(letters[1:10], nrows, replace = TRUE)
	l2 = sample(letters[10:20], nrows, replace = TRUE)
	l3 = sample(letters[15:26], nrows, replace = TRUE)
	l4 = sample(letters, nrows, replace = TRUE)
	l5 = sample(letters[c(1:5, 10:15, 20:26)], nrows, replace = TRUE)
	name = paste0(l3, l5, l2, l1, l4)
	return(name)
}


# numerical: 7 (3)
data = data.table(proportion = runif(nrows))
data[, cost := round(runif(nrows, min = 100, max = 1000))]
data[, digital_address := sample(seq(32, 256, 32), nrows, replace = T)]
data[, family_max_age := runif(nrows, min = 1, max = 100)]
data[, income := runif(nrows, min = 10000, max = 100000)]
data[, blah := (runif(nrows, min = -1, max = 1) * 100)]
data[, previous_income := income - blah]
data[, blah := NULL]
data[, parch := sample(c(round(runif(c(nrows - 4), min = 0, max = 6)), -10, 22, 100, 0.8))]
# categorical: 5 (2)
data[, name := name_generate(nrows)]
data[, country := sample(c('India', 'Pakistan', 'UK', 'China', 'USA'), nrows, replace = T)]
data[, city := sample(city_generate(1000), nrows, replace = TRUE)]
data[, house_country := sample(c('India', 'Pakistan', 'UK', 'China', 'USA', 'Others'), nrows, replace = T)]
data[, relation := sample(c('single', 'married', 'widowed', 'divorced', 'others'), nrows, replace = T)]
# date: 3 (1)
data[, dob := sample(c(sample(seq(as.Date('1993/09/13'), as.Date('2000/09/23'), by="day"), (nrows - 2), replace = T), as.Date('2018/02/20'), as.Date('1847/08/15')))]
data[, join_date := sample(seq(as.Date('2014/05/01'), as.Date('2018/02/20'), by = 'day'), nrows, replace = TRUE)]
data[, nomination_date := sample(seq(as.Date('2017/01/01'), as.Date('2017/05/31'), by = 'day'), nrows, replace = TRUE)]
# index
data[, index := sample(1:nrow(data))]
data = data[order(index)]
data = data[, c('index', sample(setdiff(colnames(data), 'index'))), with = FALSE]

# feature engineering
f_data = copy(data)
data[, currency_conv := ifelse(country == 'USA', 64.79, ifelse(country == 'UK', 90.67, ifelse(country == 'China', 10.22, ifelse(country == 'Pakistan', 0.58, 1))))]
data[, income := income * currency_conv]
data[, previous_income := previous_income * currency_conv]
data[, cost := cost * currency_conv]
data[, target1 := 0]
data[parch > 4 & parch < 10 & cost > 56004, target1 := 1]
data[, target2 := 0]
data[, income_diff := income - previous_income]
data[, target2 := ifelse(income_diff < 0, 0, ifelse(income_diff > 0 & income_diff < 30, 1, 0))]
data[, age := as.numeric(difftime(Sys.Date(), dob, unit="weeks"))/52.25]
data[, target3 := 0]
data[age > 25 & age < 30 & cost > 20000, target3 := 1]
data[age > 30 & age < 100 & cost > 5000 & cost < 15000, target3 := 1]
data[age == 0, target3 := 1]
data[age > 100, target3 := 1]
data[, first_name := unlist(lapply(strsplit(name, '.', fixed = T), function(x) x[[1]]))]
data[, last_name := unlist(lapply(strsplit(name, ' '), function(x) x[[3]]))]
data[, target4 := 0]
data[, target5 := 0]
data[first_name %in% c('Mrs', 'Dr') & age > 25, target4 := 1]
data[first_name == 'Mr' & cost > 5000, target4 := 1]
data[last_name %in% c('deere', 'jones'), target5 := 1]
data[last_name %in% c('sharma', 'varma', 'shetty', 'reddy', 'deshmukh') & cost > 10000 & age > 25, target5 := 1]
data[, target6 := 0]
data[country == house_country, target6 := 1]
data[, country_equal := as.numeric(country == house_country)]

data[, target := ifelse(target1 + target2 + target3 + target4 + target5 + target6 >= 1, 1, 0)]

drop_cols = c('currency_conv', paste0('target', 1:6), 'dob', 'name', 'country', 'house_country')
useless_cols = c('proportion', 'digital_address', 'family_max_age', 'city', 'relation', 'join_date', 'nomination_date', 'income', 'previous_income')

model_data = data[, !c(drop_cols, useless_cols), with = FALSE]
model_data[last_name %in% c('sharma', 'varma', 'shetty', 'reddy', 'deshmukh'), last_name := 'hindu']
model_data[last_name %in% c('deere', 'jones'), last_name := 'christian']
model_data[first_name %in% c('Mrs', 'Dr'), first_name := 'eligible']

model_data[, first_name := as.factor(first_name)]
model_data[, last_name := as.factor(last_name)]

library(rpart)
library(ranger)
library(MLmetrics)
index = dataSplit(model_data[, target])
train = model_data[index$train]
test = model_data[-index$train]

fit = rpart(factor(target) ~ ., data = train)
test[, MLmetrics::F1_Score(as.numeric(predict(fit, test)[, 2] >= 0.5), target)] # 0.9772744

fit = ranger(factor(target) ~ ., data = train)
test[, MLmetrics::F1_Score(predict(fit, test)$predictions, target)] # 1


# # rsrt data
f_data[, target := data[, target]]
train_index = dataSplit(model_data[, target])
train = f_data[train_index$train]
test = f_data[-train_index$train]

train[, index := paste0('train_', 1:nrow(train))]
test[, index := paste0('test_', 1:nrow(test))]

fwrite(train, 'rsrt_train.csv')
fwrite(test, 'rsrt_test_answer.csv')
fwrite(test[, !c('target'), with = FALSE], 'rsrt_test.csv')
