# Hierarchical Clustering

input_data<-read.csv("C:/Users/sam/Desktop/DM Coursework Data/teeth.csv", header=TRUE)
rownames(input_data)<-input_data$Animal
clusters<-hclust(dist(input_data[], method="euclidean"), method = "complete", members = NULL)
plot(clusters)

