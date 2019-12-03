# Resubstitution
library(rpart)

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/breast-cancer-wisconsin.data", header=FALSE)
colnames(input_data) <- c("Sample Code Number", "Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion", "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses", "class")
numRecords<-length(input_data[[1]])
numTrials<-20
accuracies<-c()

for(i in 1:numTrials)
{	
	training_set<-input_data[sample(nrow(input_data), numRecords*0.9),]
	test_set<-training_set
	decision_tree = rpart(class~., data=training_set, method='class')
	prediction<-predict(decision_tree, newdata=test_set[-11], type='class')
	confMat<-table(test_set$class, prediction)
	accuracies<-append(accuracies, sum(diag(confMat))/sum(confMat))
}
mean_accuracy<-sum(accuracies)/length(accuracies)
std_deviation<-sd(accuracies)