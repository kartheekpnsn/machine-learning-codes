# setting workspace
setwd('E:\\Data Science\\DataSciecePractise\\Karteek')

# Readingcsv
HouseDeatils = fread("rsrt_train.csv")
str(HouseDeatils)


# To find missing values percentage in dataset
missing_count = apply(HouseDeatils,2,function(x) {sum(is.infinite(x))/length(x)})
colswithallmiss <- names(missing_count[missing_count>0]);    
print("the columns with all values missing");    
print(colswithallmiss);

# Finding the character columns
characterdata = colnames(HouseDeatils)[which(sapply(HouseDeatils,class) == "character")]

## converting all character columns to factors
for(i in characterdata)
{
  HouseDeatils[[i]] = factor(HouseDeatils[[i]])
}
# # MODIFIED !!
HouseDeatils[, (characterdata) := lapply(.SD, factor), .SDcols = characterdata]

################################## Feature Engineering ##############################################
HouseDeatils <- HouseDeatils[, digital_address:=as.factor(digital_address)] # as its static
HouseDeatils = HouseDeatils[, target:=as.factor(target)] # convertin target variable to category

# Difference between income and previous income
HouseDeatils[["income_diff"]] = HouseDeatils[["income"]] - HouseDeatils[["previous_income"]]
# # MODIFIED !!
HouseDeatils[, income_diff := income - previous_income]

# creating a status column by income difference
HouseDeatils[["income_status"]] = ifelse(HouseDeatils[["income_diff"]] > 0 , "Promoted" , "Depromoted")
HouseDeatils[["income_status"]] = as.factor(HouseDeatils[["income_status"]])
# # MODIFIED !!
HouseDeatils[, income_status := as.numeric(income_diff > 0)]


#Age of person upto the nomination date
HouseDeatils[["Age_nominate"]] = as.numeric(difftime(as.Date(HouseDeatils[["nomination_date"]]),as.Date(HouseDeatils[["dob"]]),units = "weeks"))/52.25
HouseDeatils[["Age_nominate"]] = floor(HouseDeatils[["Age_nominate"]])
HouseDeatils[["Age_join"]] = as.numeric(difftime(as.Date(HouseDeatils[["join_date"]]),as.Date(HouseDeatils[["dob"]]),units = "weeks"))/52.25
HouseDeatils[["Age_join"]] = floor(HouseDeatils[["Age_join"]])
# # MODIFIED !!
HouseDeatils[, Age_nominate := floor(as.numeric(difftime(as.Date(HouseDeatils[["nomination_date"]]),as.Date(HouseDeatils[["dob"]]),units = "weeks"))/52.25)]
HouseDeatils[, Age_join := floor(as.numeric(difftime(as.Date(HouseDeatils[["join_date"]]),as.Date(HouseDeatils[["dob"]]),units = "weeks"))/52.25)]

# creating Title column
HouseDeatils[["Title"]] = gsub(". .*", "", HouseDeatils[["name"]])
HouseDeatils[["Title"]] = as.factor(HouseDeatils[["Title"]])
# # MODIFIED !!
HouseDeatils[, Title := as.factor(gsub(". .*", "", name))]

# creating family size
HouseDeatils[["FamilySize"]] = HouseDeatils[["parch"]] + 1
# # MODIFIED !!
HouseDeatils[, FamilySize := parch + 1]

## removing the decimal number
HouseDeatils = HouseDeatils[!(FamilySize == 1.8)]
# # MODIFIED !!
HouseDeatils = HouseDeatils[FamilySize != 1.8]

# adding family description
HouseDeatils[["FSizeD"]][HouseDeatils[["FamilySize"]] == 1] = "Singleton"
HouseDeatils[["FSizeD"]][HouseDeatils[["FamilySize"]] > 1 & HouseDeatils[["FamilySize"]] <5] = "Medium"
HouseDeatils[["FSizeD"]][HouseDeatils[["FamilySize"]] > 4] = "Large"
HouseDeatils[["FSizeD"]] = as.factor(HouseDeatils[["FSizeD"]])
class(HouseDeatils[["FSizeD"]])
# # MODIFIED !!
HouseDeatils[, FSizeD := 'Others']
HouseDeatils[FamilySize == 1, FSizeD := 'Singleton']
HouseDeatils[FamilySize > 1 & FamilySize < 5, FSizeD := 'Medium']
HouseDeatils[FamilySize > 4, FSizeD := 'Large']
HouseDeatils[, FSizeD := as.factor(FSizeD)]

## Creating local and NRI status
levels(HouseDeatils[["country"]]) = levels(HouseDeatils[["house_country"]])
HouseDeatils[["Citizenship"]] = ifelse(HouseDeatils[["country"]] == HouseDeatils[["house_country"]] , "Local" , "NRI")
HouseDeatils[["Citizenship"]] = as.factor(HouseDeatils[["Citizenship"]])
# # MODIFIED !!
HouseDeatils[, Citizenship := as.numeric(country == house_country)]


## splitting data - train and test
data = createDataPartition(HouseDeatils[["target"]],p=0.7,list=FALSE)
train = HouseDeatils[data,]
test = HouseDeatils[-data,]


################################## modeling part ##############################################

# removing the following columns
# cloumn name - reason
# index - unique
# Name - more unique values
# nomination_date,join_date,dob - created age usinng these

# removed decission - as its taking more time
# performing random forest
model = ranger(target ~.,data = subset(train,select = -c(index,name,city,nomination_date,join_date,dob)),num.trees = 50,importance = "impurity")
model$variable.importance # to get the importance of model

# predection part
testmodel = predict(model,test)
head(testmodel)
metric = confusionMatrix(testmodel$predictions,test$target)
## F-score
metric$byClass[7]

# custom function to convert category to numeric as xgboost doesnt accept category
manipulate = function(temp)
{
  
  # removing columns as mentioned above
  temp = subset(temp,select = -c(nomination_date,join_date,dob,name,index,city))
  
  #one hot encoding code if required(removed it as not found useful)
  #temp1 = temp
  #temp <- subset(temp,select=-c(target,income_status,FamilySize,Citizenship))
  #temp = data.frame(model.matrix(~.-1,temp),target = temp1$target,income_status = temp1$income_status,FamilySize = temp1$FamilySize,Citizenship = temp1$Citizenship)
  
  
  ## label encoding 
  temp[["Citizenship"]] = as.numeric(temp[["Citizenship"]])
  temp[["income_status"]] = as.numeric(temp[["income_status"]])
  temp[["Title"]] = as.numeric(temp[["Title"]])
  temp$country = as.numeric(temp$country)
  temp$house_country = as.numeric(temp$house_country)
  temp[["relation"]] = as.numeric(temp[["relation"]])
  temp[["digital_address"]] = as.numeric(temp[["digital_address"]])
  
  # convertin target to numeric as xgboost takes only numeric
  temp$target = as.numeric(temp$target)
  # subtract by 1 beacause when converted to numeric 0,1 becomes 1,2
  temp$target = temp$target - 1
  return(temp)
}

traindata = manipulate(train)
testdata = manipulate(test)

# removing fsizeD column
traindata = traindata[, !('FSizeD'), with = FALSE]
testdata = testdata[, !('FSizeD'), with = FALSE]

##### applying xgboost for the model

# setting up parameters for model
xgb_params = list("objective" = "multi:softprob","eval_metric" = "mlogloss","num_class" = 2)
#modelboost = xgb.train(params = xgb_params,data = xgb.DMatrix(data = as.matrix(subset(traindata,select = -c(target))), label = traindata$target),nrounds = 10)
modelboost =xgboost(params = xgb_params,data = as.matrix(subset(traindata,select = -c(target))),label = traindata[["target"]],nrounds = 100)

# variable importance
imp = xgb.importance(colnames(traindata) , model = modelboost)
print(imp)
xgb.plot.importance(imp)

# prediction
prediction = predict(modelboost, newdata =  xgb.DMatrix(data = as.matrix(subset(testdata,select = -c(target))), label = testdata$target) )
pred= matrix(prediction , nrow = 2,ncol = length(prediction)/2) %>% 
  t() %>%
  data.frame() %>%
  mutate(label = testdata$target,max_prob = max.col(.,"last")-1)

# confusion matrix
metric = confusionMatrix(pred$max_prob, pred$label)
fscore = metric$byClass[7]
