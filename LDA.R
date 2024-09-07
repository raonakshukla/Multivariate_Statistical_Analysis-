# Importing caTools to split data into test and train
install.packages("ggordiplots")
install.packages('caTools')
install.packages('klaR')
library(caTools)
library(klaR)
library(ggordiplots)

UN_lda <- UN[,c(1,3:38)]
UN_lda$continent <- as.factor(UN_lda$continent)

set.seed(787)
sample <- sample.split(UN_lda$continent, SplitRatio = 0.7)
train  <- subset(UN_lda, sample == TRUE)
test   <- subset(UN_lda, sample == FALSE)
un.lda<-lda(continent~., train)
un.pred <- predict(un.lda, test)
print(paste("The predictive accuracy is ", sum(un.pred$class== test$continent)/dim(test)[1]*100, "%"))
table(un.pred$class, test$continent)


partimat(continent~., data=UN_lda, method="lda",plot.matrix = FALSE,nplots.vert=1,nplots.hor=1)
par(pty="s")
plot(un.lda,col=as.integer(UN_lda$continent)+1,abbrev = 1)
un.lda1<-lda(continent~., UN_lda)
un.pred1 <- predict(un.lda1, UN_lda)
plot(x=un.pred1$x[,1],y=un.pred1$x[,2],col= UN_lda$continent,pch=20, xlab= "LDA 1", ylab="LDA 2", main= "Linear Discriminant Plot")
legend('bottomright',inset=0.0005,y.intersp=0.5,legend=unique(as.factor(gdp$continent)),pch=20,col=unique(as.factor(gdp$continent)),title="Continents",pt.bg = as.factor(gdp$continent))
gg_ordiplot(un.lda1, UN_lda$continent,ellipse = TRUE,label = TRUE,hull = FALSE,spiders = FALSE,kind = c("ehull"),pt.size = 2)
table(un.pred1$class, UN_lda$continent)



