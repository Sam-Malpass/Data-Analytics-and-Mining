# Cross-Validation
library(rpart)

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/wine.data", header=FALSE)
colnames(input_data) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium", "Total Phenols", "Flavanoids", "Non-flavanoid Phenols", "Proanthocyanis", "Color Intensity", "Hue", "OD280/OD315 of Diluted Wines", "Proline")
numRecords<-length(input_data[[1]])
numTrials<-20
numFolds<-10
accuracies<-c()

for(val in 1:numTrials)
{
	shuffled_indices<-sample(nrow(input_data))
	shuffled_data<-input_data[shuffled_indices,]
	correct_predictions<-0
	folds<-cut(seq(1,numRecords), breaks=numFolds, labels=FALSE)
	for(i in 1:numFolds)
	{
		sample<-which(folds==i, arr.ind=TRUE)
		test_set<-shuffled_data[sample,]
		training_set<-shuffled_data[-sample,]
		
		decision_tree = rpart(class~., data=training_set, method='class')
		prediction<-predict(decision_tree, newdata=test_set[-15], type='class')
		confMat<-table(test_set$class, prediction)
		
		correct_predictions<-correct_predictions + sum(diag(confMat))
		
	}
	accuracies<-append(accuracies, correct_predictions / numRecords)
}
mean_accuracy<-sum(accuracies)/length(accuracies)
std_deviation<-sd(accuracies)
