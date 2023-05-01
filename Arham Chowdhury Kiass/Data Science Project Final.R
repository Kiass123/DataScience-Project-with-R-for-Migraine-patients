Amena Akter
20-42761-1

*Final TErm *
  #dataset input
  mydata<- read.csv("C:/zoo.csv",header = TRUE,sep = ",")
mydata

str(mydata)

#scalling data
scale(mydata_new)

#remove names
mydata_new <- mydata[, -1]
mydata_new

#see any zero
colSums(mydata == 0)

#remove zero
constant_cols <- which(colSums(mydata == 0) == nrow(mydata))
mydata_new <- mydata[,-constant_cols]

mydata_new

#intalling packages
install.packages("ClusterR")
install.packages("cluster")
install.packages("factoextra")


# Loading packages
library(ClusterR)
library(cluster)
library(factoextra)

fviz_nbclust(mydata_new, kmeans, method = "wss")


#K-means Clustering Algorithm 

km <- kmeans(mydata_new, centers = 5, nstart = 25)
km


#Visualize the output of K-means Clustering Algorithm

k_clusters <- cbind(mydata_new, cluster = km$cluster)
k_clusters


#Visualize the output of K-means Clustering Algorithm

fviz_cluster(km, data = mydata_new)

#find means of each cluster

aggregate(mydata_new, by=list(cluster=km$cluster), mean)
#hierarcical clustering
hc <- hclust(dist(mydata_new))
plot(hc)


