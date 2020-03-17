source('loadData.R')
trn <- loadData('songListTrainSet')
trn <- trn[sample(1:nrow(trn), 10000),]
vld <- loadData('songListValidationSet')
vld <- vld[sample(1:nrow(vld), 10000),]
tst <- loadData('songListTestSet')
tst <- tst[sample(1:nrow(tst), 10000),]
source('crossValidate.R')
x <- crossValidate(trn, vld, tst, 5, "NMF", FALSE, FALSE)