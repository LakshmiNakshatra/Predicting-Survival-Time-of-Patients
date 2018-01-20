
# Importing  datasets
data1 = read.csv("Data_training.csv", header=TRUE)
test.set = read.csv("Data_test.csv", header=TRUE)

# Attaching training dataset to the path
attach(data1)

# Analyzing the imported dataset
summary(data1)
str(data1)
is.na(data1)

# Multi-collinearity analysis
cor(data1[,c(-6,-7,-8)])

## Data Preprocessing
# Encoding categorical variables in training set
data1$gender=factor(data1$gender, levels=c(0,1), labels=c("male", "female"))
data1$alco=factor(data1$alco, levels=c(0,1,2), labels=c("none", "moderate", "severe"))

# Encoding categorical variables in test set
test.set$gender=factor(test.set$gender, levels=c(0,1), labels=c("male", "female"))
test.set$alco=factor(test.set$alco, levels=c(0,1,2), labels=c("none", "moderate", "severe"))

## Fitting regression models
# Performing backward elimination on full model
fit.lm = lm(survival~.,data1)
bwd = step(fit.lm,data=data1,direction="backward")
summary(bwd)
# Fitting linear model using significant predictors obtained from backward elimination
fit.lm4 = lm(survival~bldclot+prog+enzy+alco, data1)
summary(fit.lm4)

# Performing backward elimination on full model with interaction terms
fit.lmint = lm(survival~.*.,data1)
bwd.int = step(fit.lm,data=data1,direction="backward")
summary(bwd.int)
# Fitting linear model using significant predictors obtained from backward elimination
fit.lmint3 = lm(survival~(bldclot+prog+enzy+alco+bldclot:alco+enzy+alco), data1)
summary(fit.lmint3)

# Fitting polynomial model to the dataset
poly2 = lm(survival~polym(bldclot,prog,enzy,liverfunc,age,degree=2)+gender+alco,data=data1)
summary(poly2)

# Fitting linear model with log transformation 
fit.lm6=lm(log(survival)~(bldclot+log(prog)+enzy+alco),data1)
summary(fit.lm6)

## Predicting results using the two best models
# Predicting test set results using linear model with interaction
predint = predict(fit.lmint3,test.set)
mse = mean((test.set$survival-predint)^2)

# Predicting test set results using polynomial model
predpoly=predict(poly2,test.set)
mse=mean((test.set$survival-predpoly)^2)
