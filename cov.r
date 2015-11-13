cov <- function(trainX, trainY) {
    source("uk.r")
    K <- length(unique(trainY))
    #p <- length(trainX)
    p <- ncol(trainX)
    uk <- uk(trainX, trainY)
    cov <- matrix(0,p,p)
    for (k in 1:K) {
        for (n in 1:nrow(trainX)) {
            if (trainY[n]==k-1) {
                for (i in 1:p) {
                    for (j in 1:p) {
                        cov[i,j] <- cov[i,j]+(trainX[n,i]-uk[k,i])*(trainX[n,j]-uk[k,j])
                    }
                }
            }
            
        }
    }
    cov <- cov/(nrow(trainX)-K)
    return(cov)

}
