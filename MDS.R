UN.transformed <- cbind(log(UN[,3:14]), UN[,15:26], log(UN[,27:38]))
un.mds <- cmdscale(dist(UN.transformed,method= "euclidean"),k=2)
coords = data.frame(x=un.mds[,1], y=un.mds[,2])
ggplot(coords, aes(x=x, y=y, col= UN$continent)) +  geom_point() +labs(x= 'Dimension 1',y='Dimension 2',color='Continent')+ geom_text_repel(aes(label = UN$country), size = 3)
plot3d(un.mds,col= as.integer(as.factor(UN$continent)))


plot(un.mds)
text(un.mds, labels = UN$country)

