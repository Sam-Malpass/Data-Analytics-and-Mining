# Resubstitution
library(rpart)

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/wine.data", header=FALSE)
colnames(input_data) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium", "Total Phenols", "Flavanoids", "Non-flavanoid Phenols", "Proanthocyanis", "Color Intensity", "Hue", "OD280/OD315 of Diluted Wines", "Proline")
numRecords<-length(input_data[[1]])
numTrials<-20
accuracies<-c()

for(i in 1:numTrials)
{	
	training_set<-input_data[sample(nrow(input_data), numRecords*0.9),]
	test_set<-training_set
	decision_tree = rpart(class~., data=training_set, method='class')
	prediction<-predict(decision_tree, newdata=test_set[-15], type='class')
	confMat<-table(test_set$class, prediction)
	accuracies<-append(accuracies, sum(diag(confMat))/sum(confMat))
}
mean_accuracy<-sum(accuracies)/length(accuracies)
std_deviation<-sd(accuracies)

