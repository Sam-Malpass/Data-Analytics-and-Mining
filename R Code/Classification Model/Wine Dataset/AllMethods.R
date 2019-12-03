# Resubstitution
library(rpart)

all_accuracies<-c()
all_devs<-c()

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

all_accuracies<-append(all_accuracies, mean_accuracy)
all_devs<-append(all_devs, std_deviation)

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/wine.data", header=FALSE)
colnames(input_data) <- c("class","Alcohol","Malic Acid","Ash","Alcalinity of Ash","Magnesium", "Total Phenols", "Flavanoids", "Non-flavanoid Phenols", "Proanthocyanis", "Color Intensity", "Hue", "OD280/OD315 of Diluted Wines", "Proline")
numRecords<-length(input_data[[1]])
numTrials<-20
accuracies<-c()

for(i in 1:numTrials)
{	
	sample<-sample.int(n=nrow(input_data), size=numRecords*0.9, replace=FALSE)
	training_set<-input_data[sample,]
	test_set<-input_data[-sample,]
	decision_tree = rpart(class~., data=training_set, method='class')
	prediction<-predict(decision_tree, newdata=test_set[-15], type='class')
	confMat<-table(test_set$class, prediction)
	accuracies<-append(accuracies, sum(diag(confMat))/sum(confMat))
}
mean_accuracy<-sum(accuracies)/length(accuracies)
std_deviation<-sd(accuracies)

all_accuracies<-append(all_accuracies, mean_accuracy)
all_devs<-append(all_devs, std_deviation)

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

all_accuracies<-append(all_accuracies, mean_accuracy)
all_devs<-append(all_devs, std_deviation)

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

all_accuracies<-append(all_accuracies, mean_accuracy)
all_devs<-append(all_devs, std_deviation)

combined<-rbind(all_accuracies, all_devs)
xlab<-c("Resubstitution","Holdout","10f-XValidation","LOOCV")
colnames(combined)<-xlab
barplot(combined, beside=TRUE, main="Wine")