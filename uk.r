uk <- function(trainX, trainY){
    k <- length(unique(trainY))
    #p <- length(trainX)
    p <- ncol(trainX)
    if (k != 2) {
        print("Error: There are more than 2 class!")
    }
    res <- matrix(,k,p)
    for (i in 1:p) {
        #class !1: k=1
        res[1,i] <- mean(trainX[(trainY!=1),i])
        #class 1: k=2
        res[2,i] <- mean(trainX[(trainY==1),i])
    }
    return(res)
}
