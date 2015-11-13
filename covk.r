covk <- function(trainX, trainY) {
    source("uk.r")
    K <- length(unique(trainY))
    #p <- length(trainX)
    p <- ncol(trainX)
    uk <- uk(trainX, trainY)
    a <- matrix(0,p,p)
    nk <- 0
    for (n in 1:nrow(trainX)) {
        if (trainY[n]==0) {
            nk <- nk+1
            for (i in 1:p) {
                for (j in 1:p) {
                    a[i,j] <- a[i,j]+(trainX[n,i]-uk[1,i])*(trainX[n,j]-uk[1,j])
                }
            }
        }
    }
    a <- a/(nk-1)

    b <- matrix(0,p,p)
    nk <- 0
    for (n in 1:nrow(trainX)) {
        if (trainY[n]==1) {
            nk <- nk+1
            for (i in 1:p) {
                for (j in 1:p) {
                    b[i,j] <- b[i,j]+(trainX[n,i]-uk[2,i])*(trainX[n,j]-uk[2,j])
                }
            }
        }
    }
    b <- b/(nk-1)
    covk <- list(a,b)
    return(covk)

}
