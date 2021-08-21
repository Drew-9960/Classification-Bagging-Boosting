
library(rpart)
library(caret)
library(adabag)

df = read.csv('Insurance.csv')
str(df)
df = df[,-6] #dropping region column

df$sex = ifelse(df$sex == 'male',1,0)
df$smoker = ifelse(df$smoker == 'yes',1,0)
df$sex = as.factor(df$sex)
df$smoker = as.factor(df$smoker)
table(df$sex)
table(df$smoker)

# partitioning data
train_index = sample(c(1:dim(df)[1]), dim(df)[1]*0.7)
train_set = df[train_index, ]
valid_set = df[-train_index, ]

# single tree 
tree = rpart(smoker ~., data = train_set)
pred = predict(tree, valid_set, type = 'class')
confusionMatrix(pred, valid_set$smoker)

# bagging 

bag = bagging(smoker ~., data = train_set)
pred_bag = predict(bag, valid_set, type = 'class')
confusionMatrix(as.factor(pred_bag$class), as.factor(valid_set$smoker))

# boosting 
boost = boosting(smoker ~., data = train_set)
pred_boost = predict(boost, valid_set, type = 'class')
confusionMatrix(as.factor(pred_boost$class), as.factor(valid_set$smoker))
