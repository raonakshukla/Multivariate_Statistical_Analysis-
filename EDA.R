setwd('C:\\Users\\raona\\OneDrive\\Documents\\Multivariate')


library(ggplot2)
library(tidyr)
library(dplyr)
UN <- read.csv('UN.csv')
str(UN)
summary(UN)
continent <- UN$continent


gdp <- UN[,c(1,3:14)] # The GDP per capita.
years <- seq(1952, 2007,5)
con <- "continent"
colnames(gdp) <- append(con,as.character(years))
rownames(gdp) <- UN[,2]
head(gdp)

lifeExp <- UN[,c(1,15:26)] # the life expectancy
colnames(lifeExp) <- append(con,as.character(years))
rownames(lifeExp) <- UN[,2]
head(lifeExp)

popn <- UN[,c(1,27:38)] # the population size
colnames(popn) <- append(con,as.character(years))
rownames(popn) <- UN[,2]
head(popn)

#EDA Life expectancy Vs GDP for 2007
ggplot(UN, aes(x=gdpPercap_2007,y=lifeExp_2007,col=continent,size=pop_2007))+ geom_point()+geom_text(label=UN$country)+labs(x= 'GDP for the year 2007',y='Life Expectancy for year 2007')

#EDA Life expectancy Vs GDP for 1952
ggplot(UN, aes(x=gdpPercap_1952,y=lifeExp_1952,col=continent,size=pop_1952))+ geom_point()+geom_text(label=UN$country)

#EDA Population Vs GDP for 2007
ggplot(UN, aes(x=gdpPercap_2007,y=lifeExp_2007,col=continent))+ geom_point(aes(color=continent,size=pop_2007))+facet_wrap(~continent)#+geom_text(label=UN$country)

#EDA Population Vs GDP for 2007
ggplot(UN, aes(x=gdpPercap_2007,y=pop_2007,col=continent))+ geom_point()+geom_point(aes(color=continent,size=pop_2007))+facet_wrap(~continent)#+geom_text(label=UN$country)

#EDA Life Expectancy Vs Population for 2007
ggplot(UN, aes(y=lifeExp_2007,x=pop_2007,col=continent,size=pop_2007))+ geom_point(aes(color=continent,size=pop_2007))+facet_wrap(~continent)#+geom_text(label=UN$country)

#EDA Life Expectancy Vs Population for 2007
ggplot(UN, aes(y=lifeExp_1952,x=pop_1952,col=continent,size=pop_1952))+ geom_point()#+geom_text(label=UN$country)


#Percentage change in the GDP, Life_Exp and Population in last 55 years
UN %>% group_by(continent) %>% summarise(pc_gdp = (mean(gdpPercap_2007)-mean(gdpPercap_1952))*100/mean(gdpPercap_1952), pc_life=(mean(lifeExp_2007)-mean(lifeExp_1952))*100/mean(lifeExp_1952),pc_pop=(mean(pop_2007)-mean(pop_1952))*100/mean(pop_1952))
UN %>% summarise(pc_gdp =(mean(gdpPercap_2007)-mean(gdpPercap_1952))*100/mean(gdpPercap_1952),pc_life=(mean(lifeExp_2007)-mean(lifeExp_1952))*100/mean(lifeExp_1952),pc_pop=(mean(pop_2007)-mean(pop_1952))*100/mean(pop_1952))
UN %>% group_by(continent) %>% summarise(mean_gdpPercap_2007=mean(gdpPercap_2007),mean_lifeExp_2007=mean(lifeExp_2007),mean_pop_2007=mean(pop_2007)/10**6)
UN %>% group_by(continent) %>% summarise(mean_gdpPercap_1952=mean(gdpPercap_1952),mean_lifeExp_1952=mean(lifeExp_1952),mean_pop_1952=mean(pop_1952),min= min(gdpPercap_1952),max=max(gdpPercap_1952))

cor(UN$gdpPercap_2007, UN$lifeExp_2007)
cor(UN$gdpPercap_2007, UN$pop_2007)
cor(UN$lifeExp_2007, UN$pop_2007)
