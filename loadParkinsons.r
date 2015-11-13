parkinsons <- read.csv("parkinsons.data")
parkinsons <- parkinsons[,-1]
set.seed(0506)
train <- sample(nrow(parkinsons),120)
test <- -train
X <- parkinsons[,-17]
Y <- parkinsons[,17]

default <- c(1:length(X))
print("Input the column number which you want to as attributes:(the default is all)")
print("(Number of the best column is 22, 19)")
print("(split every number with one space, like, 1 2 3)")
default = scan()

if (length(default)==1) {
    trainX <- t(t(X[train,c(default)]))
    testX <- t(t(X[test,c(default)]))
}else{
    trainX <- X[train,c(default)]
    testX <- X[test,c(default)]
}

trainY <- Y[train]
testY <- Y[test]
