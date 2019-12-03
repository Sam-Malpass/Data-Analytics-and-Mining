# Holdout
library(rpart)

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/iris.csv", header=TRUE)
numRecords<-length(input_data[[1]])
numTrials<-20
accuracies<-c()

for(i in 1:numTrials)
{	
	sample<-sample.int(n=nrow(input_data), size=numRecords*0.9, replace=FALSE)
	training_set<-input_data[sample,]
	test_set<-input_data[-sample,]
	decision_tree = rpart(class~., data=training_set, method='class')
	prediction<-predict(decision_tree, newdata=test_set[-5], type='class')
	confMat<-table(test_set$class, prediction)
	accuracies<-append(accuracies, sum(diag(confMat))/sum(confMat))
}
mean_accuracy<-sum(accuracies)/length(accuracies)
std_deviation<-sd(accuracies)
