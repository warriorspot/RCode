
Unsupervised Learning

data(iris); library(caret);
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
kMeans1 <- kmeans(subset(training, select=-c(Species)), centers = 3)
training$clusters <-  as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length,colour=clusters,data=training)
table(kMeans1$cluster, training$Species)
m <- train(clusters ~ ., data=subset(training, select=-c(Species)), method="rpart")
table(predict(m, training), training$Species)
table(predict(m, testing), testing$Species)

Week 4 Lecture 1

library(ElemStatLearn)
data(prostate)



Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

inTrain <- createDataPartition(y=segmentationOriginal$Case, p=0.7, list=FALSE)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

set.seed(125)
m <- train(Class ~ ., data=training, method="rpart")


Question 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

m <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method="glm", family="binomial")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(m, newdata=trainSA))
missClass(testSA$chd, predict(m, newdata=testSA))

Question 5


library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
m <- train(y ~ ., data=vowel.train,method="rf")
varImp(m)
