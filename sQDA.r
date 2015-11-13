sQDA <- function(testX, testY, vpik, vuk, vcovk){
    K <- length(unique(testY)) 
    predictY <- c()
    for (i in 1:nrow(testX)) {
        delta <- c()
        for (k in 1:K) {
            vc <- do.call(rbind,vcovk[k])
            #print(vc)
            delta[k] <- -1/2*log(det(vc))-1/2*sum(solve(vc,t(testX[i,]))*testX[i,])+sum(solve(vc,t(testX[i,]))*vuk[k,])-1/2*sum(solve(vc,t(t(vuk[k,])))*vuk[k,])+log(vpik[k])
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
