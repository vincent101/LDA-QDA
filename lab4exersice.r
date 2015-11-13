# lab4
getwd()
parkinsons  <- read.csv("parkinsons.data")
#dim(parkinsons)
#str(parkinsons)
parkinsons  <- parkinsons[,-1]
Y  <- parkinsons[,17]
X  <- parkinsons[,-17]
str(X)
attach(parkinsons)
train = sample(nrow(parkinsons),120,replace=F)
test = -train
library(MASS)
lda.fit <- lda(status~PPE, data=parkinsons, subset=train)

# Predict using lda,fit 
lda.pred <- predict(lda.fit, X[test,])
lda.class <- lda.pred$class
true.labels <- Y[test]
table(lda.class,true.labels)
mean(lda.class==true.labels)
# the best two are PPE and speard1

# Calculate mean
mean.0 <- mean(X[(Y==0),20])
mean.1 <- mean(X[(Y==1),20])
st.dev <- sd(X[,20])
(mean.1-mean.0)/st.dev
plot(X[(Y==0),19],X[(Y==0),22],type="p",col="red",xlab="19",ylab="22")
points(X[(Y==1),19],X[(Y==1),22],type="p",col="blue")

# Calculate inverse matrix by sovle(A)
# Calculate x from A %*% x = B by solve(A,B)
A <- matrix(c(1,2,3,4),2,2)
E <- matrix(c(1,0,0,1),2,2)
solve(A,E)
solve(A)
A%*%solve(A)
# e^x by exp(x)
exp(1)
exp(c(1,2,3,4,5))

# Apply LDA in iris
names(iris)
dim(iris)
str(iris)
attach(iris)
train = sample(nrow(iris),100,replace=F)
test = -train
library(MASS)
lda.fit <- lda(Species~. , data=iris, subset=train)
# traceback() 
lda.fit
plot(lda.fit)

# Predict using lda,fit 
lda.pred <- predict(lda.fit, iris[test,1:4])
lda.class <- lda.pred$class
true.labels <- iris[test,5]
table(lda.class,true.labels)
mean(lda.class==true.labels)
# the best two is X1.7 and X0.5

# Use posterior probability threshold to control error rate(sensitivity, specificity)
lda.pred$class
lda.pred$post[,1]>0.5

# Apply QDA in iris
qda.fit <- qda(Species~., data=iris, subset=train)
qda.fit
qda.pred <- predict(qda.fit, iris[test,1:4])
qda.class <- qda.pred$class
true.labels <- iris[test,5]
table(qda.class,true.labels)


