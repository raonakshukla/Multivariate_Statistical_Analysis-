library(CCA)
prem.cca <- cc(X,Y)
plt.cc(prem.cca, ind.names = rownames(gdp), type='b',var.label=FALSE)

plot(x=prem.cca$scores$xscores[,1],y=prem.cca$scores$yscores[,1],pch=20,col=as.factor(gdp$continent),main="CC_1 Score GDP VS Life Exp",xlab="GDP_CC_1",ylab="LifeExp_CC_1")
legend('bottomright',inset=0.0005,y.intersp=0.5,legend=unique(as.factor(gdp$continent)),pch=20,col=unique(as.factor(gdp$continent)),title="Continents",pt.bg = as.factor(gdp$continent))

prem.cca$cor
prem.cca$xcoef
prem.cca$ycoef
prem.cca$scores$corr.X.xscores
prem.cca$scores$corr.X.yscores
prem.cca$scores$corr.Y.xscores
prem.cca$scores$corr.Y.yscores


plt.indiv(prem.cca,d1=1,d2=2)
par(mfrow=c(1,2))
hist(UN$gdpPercap_2007,main="Without Log")
hist(log(UN$gdpPercap_2007),main="With Log transformation")

