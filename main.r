# select data set
print("Select DataSet:(1,iris; 2,parkinsons)")
selectData = scan()

if (selectData == 1) {
    source("loadIris.r")
}
if (selectData == 2) {
    source("loadParkinsons.r")
}
source("pik.r")
vpik <- pik(trainY)
source("uk.r")
vuk <- uk(trainX, trainY)

# select mode(lad or qda)
print("Select mode:(1,LDA; 2,QDA)")
mode = scan()

if (mode == 1) {
    source("cov.r")
    vcov <- cov(trainX, trainY)
    source("sLDA.r")
    predictY <- sLDA(testX, testY, vpik, vuk, vcov)
    print(mean(predictY==testY))
}
if (mode == 2) {
    source("covk.r")
    vcovk <- covk(trainX, trainY)
    source("sQDA.r")
    predictY <- sQDA(testX, testY, vpik, vuk, vcovk)
    print(mean(predictY==testY))
}
