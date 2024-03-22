library(rpart)
library(rpart.plot)

datadc <- read.csv("decision3.csv")
head(datadc)

set.seed(124)

indexes = createDataPartition(datadc$diagnosis, p = .8, list = F)
traindc = datadc[indexes, ]
testdc = datadc[-indexes, ]
dim(traindc)
dim(testdc)

modeldc=rpart(diagnosis~.,data=traindc)
print(modeldc)

preddc = predict(modeldc,  testdc, type="class")

cmdc = table(testdc$diagnosis, preddc)
print(cmdc) 

akurasidc = sum(diag(cmdc))/sum(cmdc)
print(akurasidc)

par(mar = c(5, 5, 2, 2))  

rpart.plot(modeldc)


