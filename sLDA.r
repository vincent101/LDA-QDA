sLDA <- function(testX, testY, vpik, vuk, vcov){
    K <- length(unique(testY))
    predictY <- c()
    for (i in 1:nrow(testX)) {
        delta <- c()
        for (k in 1:K) {
            delta[k] <- sum(solve(vcov,t(testX[i,]))*vuk[k,])-1/2*sum(solve(vcov,t(t(vuk[k,])))*vuk[k,])+log(vpik[k])
            
        }
        if (delta[1]>delta[2]) {
            predictY[i] <- 0
        }
        else{
            predictY[i] <- 1
        }
    }
    return(predictY)
}
