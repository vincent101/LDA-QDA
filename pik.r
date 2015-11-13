pik <- function(trainY){
    class0 <- 0
    class1 <- 0
    for (i in 1:length(trainY)) {
        if (trainY[i]==0) {
            class0 <- class0 + 1
        }
        else{
            class1 <- class1 + 1
        }
    }
    class0 <- class0/length(trainY)
    class1 <- class1/length(trainY)
    if (class0+class1 != 1) {
        print("Error: There are more than 2 class!")
    }
    res <- c(class0, class1)
    return(res)
}
