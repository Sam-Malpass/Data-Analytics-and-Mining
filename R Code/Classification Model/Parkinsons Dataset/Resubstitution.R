# Resubstitution
library(rpart)

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/parkinsons.data", header=TRUE)
input_data<-subset(input_data, select=-c(name))
input_data
numRecords<-length(input_data[[1]])
numTrials<-20
accuracies<-c()

for(i in 1:numTrials)
{	
	training_set<-input_data[sample(nrow(input_data), numRecords*0.9),]
	test_set<-training_set
	decision_tree = rpart(status~., data=training_set, method='class')
	prediction<-predict(decision_tree, newdata=test_set[-17], type='class')
	confMat<-table(test_set$status, prediction)
	accuracies<-append(accuracies, sum(diag(confMat))/sum(confMat))
}
mean_accuracy<-sum(accuracies)/length(accuracies)
std_deviation<-sd(accuracies)
