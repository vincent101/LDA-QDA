iris <- read.csv("iris.txt")
set.seed(0506)
train <- sample(nrow(iris),70)
test <- -train
X = iris[,-5]
Y = iris[,5]
# normalize Y
for (i in 1:length(Y)) {
    if (Y[i]==-1) {
        Y[i] = 0
    }
}

default <- c(1:length(X))
print("Input the column number which you want to as attributes:(the default is all)")
print("(Number of the best column is 3, 4)")
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
