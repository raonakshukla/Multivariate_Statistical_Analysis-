install.packages("factoextra")
install.packages("ggrepel")
library(factoextra)
library("ggrepel")
library(cluster)
UN.scaled <- UN[,1:26]
UN.scaled[,3:26] <- scale(UN[,3:26])
h_clust<-hclust(dist(UN.scaled[,3:26],method="euclidean"),method="complete")
h_clust<-hclust(dist(UN.scaled[,3:26],method="euclidean"),method="average")

p<-plot(h_clust, labels=UN.scaled$country,cex=0.5)
cut<- cutree(h_clust, k=6)
table(cut, UN.scaled$continent)
rect.hclust(h_clust, k =6, border = "red")
clusters <- as.data.frame(cut)
UN_hclust <- cbind(UN,clusters)
ggplot(UN_hclust, aes(x=gdpPercap_2007,y=lifeExp_2007,col=as.factor(UN_hclust$cut)))+ 
  geom_point()+ geom_text(label=UN_hclust$country)+
  labs(x= 'GDP for the year 2007',y='Life Expectancy for year 2007',color='Clusters')
silhouette_info <- silhouette(cut, dist(UN.scaled[,3:26],method="euclidean"))
avg_silhouette <- mean(silhouette_info[, 2])
# K-means for clustering
un.k <- kmeans(UN.scaled[,3:26], centers = 6, nstart=25)
print(un.k)
table(un.k$cluster, UN.scaled$continent)
fviz_cluster(un.k,UN.scaled[,3:26], ellipse.type = "norm")
fviz_nbclust(UN.scaled[,3:26], kmeans, method = "wss")
clusters <- as.data.frame(un.k$cluster)
UN_kmeans <- cbind(UN,clusters)
ggplot(UN_kmeans, aes(x=gdpPercap_2007,y=lifeExp_2007,col=as.factor(UN_kmeans$`un.k$cluster`)))+ 
  geom_point()+ geom_text(label=UN_kmeans$country)+
  labs(x= 'GDP for the year 2007',y='Life Expectancy for year 2007',color='Clusters')
  #geom_label_repel(min.sep = 0, label.padding = unit(0.5, "lines")) 

silhouette_info <- silhouette(un.k$cluster, dist(UN.scaled[,3:26],method="euclidean"))
avg_silhouette <- mean(silhouette_info[, 2])
library(mclust)
un.m <- Mclust(UN.scaled[,3:26],G=7)
table(un.m$classification, UN.scaled$continent)
plot(un.m, what = c("classification"))

