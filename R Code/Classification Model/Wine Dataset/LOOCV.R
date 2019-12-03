# LOOCV
library(rpart)

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/wine.data", header=FALSE)
colnames(input_data) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium", "Total Phenols", "Flavanoids", "Non-flavanoid Phenols", "Proanthocyanis", "Color Intensity", "Hue", "OD280/OD315 of Diluted Wines", "Proline")
numRecords<-length(input_data[[1]])
numTrials<-20
sample_size<-numRecords * 0.9 + 1
accuracies<-c()

for(trial in 1:numTrials)
{
	sample<-sample.int(n=nrow(input_data), size=sample_size, replace=FALSE)
	main_training_set<-input_data[sample,]
	correct_predictions<-0
	for(i in 1:sample_size)
	{
		row<-main_training_set[i,]
		training_set<-main_training_set[-i,]
		decision_tree = rpart(class~., data=training_set, method='class')
		prediction<-predict(decision_tree, newdata=row[-15], type='class')
		predict<-0
		if(row$class == prediction)
		{
			predict<-1
		}
		correct_predictions<- correct_predictions + predict		
	}
	accuracies<-append(accuracies, (correct_predictions / sample_size))
}
mean_accuracy<-sum(accuracies)/length(accuracies)
std_deviation<-sd(accuracies)

