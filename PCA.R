install.packages('factoextra')
install.packages('ggfortify')
library(factoextra)
library(ggfortify)

# Checking the dataset
head(gdp)
X <- gdp[,2:13]

# Performing PCA on GDP
gdp.pca<- prcomp(X,scale=TRUE)
summary(gdp.pca)

#Visualisation of Principal Components
fviz(gdp.pca, element='var')
autoplot(gdp.pca, data = gdp, colour='continent')
fviz_eig(gdp.pca, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_ind(gdp.pca,repel = TRUE) # Avoid text overlapping (slow if many points))
fviz_contrib(gdp.pca,choice="var")
fviz_contrib(gdp.pca,choice="ind")
fviz_ellipses(gdp.pca, habillage=gdp$continent,repel = TRUE)
fviz_pca_biplot(gdp.pca, label = "var", habillage=gdp$continent,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())

# Performing PCA on Life Expectancy
head(lifeExp)
Y<-lifeExp[,2:13]
life.pca<- prcomp(Y,scale=TRUE)
summary(life.pca)

#Visualisation of Principal Components
fviz(life.pca, element='var',ggtheme = theme_minimal())
autoplot(life.pca, data = lifeExp, colour='continent')
fviz_eig(life.pca, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(life.pca)
fviz_pca_ind(life.pca,repel = TRUE)
fviz_contrib(life.pca,choice="var")
fviz_contrib(life.pca,choice="ind")
fviz_ellipses(life.pca, habillage=lifeExp$continent,repel = TRUE)
fviz_pca_biplot(life.pca, label = "var", habillage=lifeExp$continent,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())

# GDP vs Life Expectancy
plot(x=gdp.pca$x[,1],y=life.pca$x[,1],col=as.factor(gdp$continent),pch=20, main="First Principal Component GDP Vs LifeExp",ylab = "Life Exp",xlab = "GDP")
legend('bottomright',inset=0.05,legend=unique(as.factor(gdp$continent)),pch=20,col=unique(as.factor(gdp$continent)),title="Continents",pt.bg = as.factor(gdp$continent))

